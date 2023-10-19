#!/usr/bin/env lua
-- luacheck: no unused

-- TODO Need a better way to organize notes and TODOs
-- TODO Implement config mini-language to allow for on-the-fly changes to
-- Homebase
-- TODO Add proper warnings, errors, execptions, etc.
-- TODO Check for unused stuff at some point (there is some stuff I'll just
-- want to keep around for debugging)
-- TODO Right now `nil` is creeping since we allow decleration without
-- assignment. Probably fine for now, but eventually we'll have to make a
-- decision about what to do with `nil` and nil/unitialized values.
-- TODO Re the `nil`/unitialized value issue: if we do use exceptions, remember
-- that exceptions are exceptional (looking at you ... Python)

-- When the following line is uncommented, we use a debug-capable version of
-- lpeg
package.cpath = "./?.so;" .. package.cpath
local lpeg      = require("lpeg")
local  C        =  lpeg.C
local  Cmt      =  lpeg.Cmt
local  Ct       =  lpeg.Ct
local  P        =  lpeg.P
local  R        =  lpeg.R
local  S        =  lpeg.S
local  V        =  lpeg.V
local libpt     = require("pt")
local  pt       =  libpt.pt
local peg_debug = require("pegdebug")
local  trace    =  peg_debug.trace

local exit      = os.exit
local format    = string.format
local insert    = table.insert
local open      = io.open
local os_exec   = os.execute
local read      = io.read
local stdout    = io.stdout
local unpack    = table.unpack

-- Utility functions

local function printf(format_str, ...)
    print(format(format_str, ...))
end

local function assertf(v, format_str, ...)
    assert(v, format(format_str, ...))
end

local function errorf(format_str, ...)
    error(format(format_str, ...))
end

-- Internals

local env_mt = {
    __index = function(t, k)
       if rawget(t, "parent") ~= nil then
            return t.parent[k]
        end
        return nil
    end
}

local function new_env(parent)
    local n_env = setmetatable({}, env_mt)

    if parent ~= nil then
        n_env.parent = parent
    end

    return n_env
end

local function extend_env(old_env, ...)
    local n_env = new_env(old_env)

    local kv_pairs = {...}
    for _,kv_pair in ipairs(kv_pairs) do
        n_env[kv_pair.key] = kv_pair.value
    end

    return n_env
end

-- Creating a shared exec function for call expressions and statements since
-- the only difference is that call statements discard the callee's return
-- value
local function exec_call(self, state)
    assertf(state.env[self.fn_name] == nil,
            "Error: '%s' is not a function", self.fn_name)
    assertf(state.protos[self.fn_name] ~= nil,
            "Error: Undefined function '%s'", self.fn_name)

    local proto = state.protos[self.fn_name]


    -- TODO Right now we're inserting params into the environment, but we might
    -- want to do it differently in the future.
    --
    -- TODO Also, another instance where we're bypassing the traditional
    -- methods defined for the environment. Time to reconsider its
    -- implementation?
    --
    -- TODO Also also, currently we just ignore extra arguments and formal
    -- parameters that don't have an accompanying argument. This is similar to
    -- how Lua does it, but we might want to do it differently.
    local func_env = new_env()
    for i = 1, #proto.formal_params do
        func_env[proto.formal_params[i]] = self.args[i]:exec(state)
    end

    -- Create new state for function, with a new environment but the same
    -- prototype definitions.
    --
    -- TODO Eventually want to be able to refer to variables within the
    -- enclosing scope of the prototype, similar to how Lua does it. Holding
    -- off for now since I want to understand what I'm doing first.
    --
    -- TODO Currently, this leads to an awkward situation where functions
    -- defined within functions can be called by functions defined in the
    -- outermost scope; that is, a function defined anywhere can be called
    -- anywhere assuming the prototype statement has been executed at call
    -- time. Figure out what to do with this; I don't like it as it is now.
    -- Resolution will probably go hand-in-hand with the other scoping changes
    -- I want done.
    local func_scope = {
        env = func_env,
        protos = state.protos
    }

    return proto.body:exec(func_scope)
end


-- TODO It's late. I'm tired. Probably need to revisit this.
-- TODO If I keep this function, it probably needs better error handling.
local function set_existing_var_in(env, var, val)
    local cur_scope = env

    while rawget(cur_scope, var) == nil do
        cur_scope = env.parent
        if cur_scope == nil then
            errorf("Error: '%s' is undefined", var)
        end
    end

    cur_scope[var] = val
end

local function num_expr_ast_node(num_str)
    return {
        tag = "num_ast_node",
        val = tonumber(num_str),
        exec = function(self)
            return self.val
        end
    }
end

local function var_expr_ast_node(iden_str)
    return {
        tag = "var_expr",
        iden_str = iden_str,
        exec = function(self, state)
            local val = state.env[self.iden_str]

            -- TODO Add better error handling.
            -- TODO Probably need to revisit this if/when we add boolean values
            assertf(val ~= nil, "Error: Undefined variable '%s'",
                    self.iden_str)

            return val
        end
    }
end

local bin_ops = {
    ["+"] = function(oper1, oper2) return oper1 + oper2 end,
    ["-"] = function(oper1, oper2) return oper1 - oper2 end,
    ["*"] = function(oper1, oper2) return oper1 * oper2 end,
    ["/"] = function(oper1, oper2) return oper1 / oper2 end,
    ["%"] = function(oper1, oper2) return oper1 % oper2 end,
    [">"] = function(oper1, oper2) return oper1 > oper2 and 1 or 0 end,
    ["<"] = function(oper1, oper2) return oper1 < oper2 and 1 or 0 end,
    [">="] = function(oper1, oper2) return oper1 >= oper2 and 1 or 0 end,
    ["<="] = function(oper1, oper2) return oper1 <= oper2 and 1 or 0 end,
    ["=="] = function(oper1, oper2) return oper1 == oper2 and 1 or 0 end,
    ["!="] = function(oper1, oper2) return oper1 ~= oper2 and 1 or 0 end,
}
local function bin_expr_eval_fn(self, state)
    return bin_ops[self.bin_op](self.oper1:exec(state), self.oper2:exec(state))
end

local function fold_bin(syms)
    -- If there is only one item in the list, it is a primary expression, so
    -- we should just return it
    if #syms == 1 then
        return syms[1]
    end

    local cur_expr = syms[1]
    for i = 2, #syms, 2 do
        cur_expr = {
            tag = format("bin_expr (%s)", syms[i]),
            bin_op = syms[i],
            oper1 = cur_expr,
            oper2 = syms[i + 1],
            exec = bin_expr_eval_fn
        }
    end

    return cur_expr
end

local function call_expr_ast_node(fn_name, args)
    return {
        tag = "call_expr",
        fn_name = fn_name,
        args = args,
        exec = exec_call
    }
end

local function let_stmt_ast_node(iden_str, expr)
    return {
        tag = "let_stmt",
        iden_str = iden_str,
        expr = expr,
        exec = function(self, state)
            -- TODO Reconsider metatable since it doesn't seem to be helpful
            -- and it looks like I have to bypass it anyway.
            --
            -- For now, we're making redeclaration throw an error. We'll
            -- probably keep it this way, but I don't want to make a decision
            -- right at this momentk.
            --
            -- TODO Make a decision about this whether redeclaration should
            -- throw an error.
            assertf(rawget(state.env, self.iden_str) == nil
                    and state.protos[self.iden_str] == nil,
                    "Error: Attempting to redeclare '%s'",
                    self.iden_str)

            -- We first test if there is an expression to assign to the new
            -- variable before trying to execute it. If there is no expression,
            -- we're just declaring it in the current scope.
            if self.expr == nil then
                -- We a way to mark a value as present in an environment if it
                -- has been declared but not defined. Lua uses doesn't
                -- distinguish between a table key associated with with the
                -- value `nil` and a table key not present in a table, so we
                -- need to use another value. `false` works well for now since
                -- it is the other falsey value in Lua (along with `nil`), so
                -- we can easily do tests for both, but we will probably want
                -- to revisit this choice if we want to add explicit boolean
                -- values to Homebase.
                --
                -- TODO Consider revisiting use of `false` to mark declared but
                -- undefined variables
                state.env[self.iden_str] = false
            else
                state.env[self.iden_str] = self.expr:exec(state)
            end

        end
    }
end

local function asgn_stmt_ast_node(iden_str, expr)
    return {
        tag = "asgn_stmt",
        iden_str = iden_str,
        expr = expr,
        exec = function(self, state)
            -- TODO Feels like I'm adding a lot of hacks. Revisit this solution.
            --
            -- TODO Add better error handling and think about globals and scope
            -- and if said considerations may change how we want to handle
            -- non-let assignments of variables
            --
            -- TODO Probably need to revisit this if/when we add boolean values
            assertf(state.proto[self.iden_str] == nil,
                    "Error: Attempting to assign to function '%s'",
                    self.iden_str)
            assertf(state.env[self.iden_str] ~= nil,
                    "Error: Undefined variable '%s'", self.iden_str)
            set_existing_var_in(state.env, self.iden_str, self.expr:exec(state))
        end
    }
end

-- Cheaty little hack. It's okay since we added the print expression here to
-- assist in test and debugging.
local prt_idx = 0
local function prt_stmt_ast_node(expr)
    return {
        tag = "prt_stmt",
        expr = expr,
        exec = function(self, state)
            local value
            -- TODO Need to check for string dues to weird quirk in how LPEG
            -- seems to parse print statement with empty expressions. Fix this
            -- when possible.
            if type(self.expr) == "string" then
                value = ""
            else
                value = self.expr:exec(state)
            end
            printf("<%d>: %s", prt_idx, value)
            prt_idx = prt_idx + 1
        end
    }
end

local function call_stmt_ast_node(fn_name, args)
    return {
        tag = "call_stmt",
        fn_name = fn_name,
        args = args,
        exec = function(self, state) exec_call(self, state) end
    }
end

-- TODO Currently, due to how returns are handled, attempting to return a
-- function that doesn't explicitly return a value fails to actually return;
-- execution continues at the next statement.
local function ret_stmt_ast_node(expr)
    return {
        tag = "ret_stmt",
        expr = expr,
        exec = function(self, state)
            return self.expr:exec(state)
        end
    }
end

local function stml_ast_node(stmts)
    return {
        tag = "stml",
        stmts = stmts,
        exec = function(self, state)
            for _,stmt in ipairs(self.stmts) do
                -- TODO This is specifically to handle comments. Find a better
                -- way to filter out comments
                if type(stmt) == "table" then
                    -- Note that expression in Homebase should always be
                    -- captured by a statement and therefore we shouldn't need
                    -- to worry about expressions here.
                    -- TODO What about call statements.
                    -- TODO We may also have an issue with return statements
                    -- that have empty expresssions.
                    --
                    -- TODO Right now the only type of statement that can
                    -- return a value is a "return" statement. We're basically
                    -- checking for early returns here. Is there a better way
                    -- to do this (that doesn't involve continuations -- I want
                    -- to try those at some point but not now).
                    local val = stmt:exec(state)
                    if val ~= nil then
                        return val
                    end
                end
            end
        end
    }
end

local function if_cnd_stmt_ast_node(test_expr, pass_stmts, elseifs, fail_stmts)
    return {
        tag = "if_cnd_stmt",
        test_expr = test_expr,
        pass_stmts = pass_stmts,
        elseifs = elseifs,
        fail_stmts = fail_stmts,
        exec = function(self, state)
            if self.test_expr:exec(state) ~= 0 then
                local val = self.pass_stmts:exec(state)
                if val ~= nil then
                    return val
                end
            else
                for _,elif in ipairs(self.elseifs) do
                    if elif[1]:exec(state) ~= 0 then
                        local val = elif[2]:exec(state)
                        if val ~= nil then
                            return val
                        end
                        return
                    end
                end

                local val =  fail_stmts:exec(state)
                if val ~= nil then
                    return val
                end
            end
        end
    }
end

local function whl_stmt_ast_node(test_expr, body_stmts)
    return {
        tag = "whl_stmt",
        test_expr = test_expr,
        body_stmts = body_stmts,
        exec = function(self, state)
            while test_expr:exec(state) ~= 0 do
                local val = self.body_stmts:exec(state)
                if val ~= nil then
                    return val
                end
            end
        end
    }
end

local function do_blk_stmt_ast_node(stmts)
    return {
        tag = "do_blk_stmt",
        stmts = stmts,
        exec = function(self, state)
            local old_env = state.env
            state.env = extend_env(old_env)
            stmts:exec(state)
            state.env = old_env
        end
    }
end

-- #TODO Random related question to look into. Why can closures in Lua only
-- refer to variables in their closing scope that were defined sequentially
-- before they were?
local function fn_proto_stmt_ast_node(name, formal_params, body_stmts)
    return {
        tag = "fn_proto_stmt",
        name = name,
        formal_params = formal_params,
        body_stmts = body_stmts,
	    exec = function(self, state)
            assertf(state.env[self.name] == nil,
                    "Error: Attempting to redeclare '%s'", self.name)
            state.protos[self.name] = {
                body = body_stmts,
                formal_params = formal_params
            }
	    end
    }
end

local function test_com(s, i, caps)
    printf("Found comment starting at index %d", i - 1)
    printf("Next char is '%s'", s:sub(i, i))
    return true
end

local line_end = S("\r\n")
local sp = S(" \t\r\n")^0

local dec_dig= R("09")
local num_expr = dec_dig^1 / num_expr_ast_node

local alpha = R("az", "AZ")
local alnum = alpha + dec_dig

local iden_char = alnum + "_"
local iden = sp * C((alpha + "_") * iden_char^0) * sp

local var_expr = iden / var_expr_ast_node

-- Sanity check for keywords
local keywords = {
    "if", "then", "elseif", "else", "end", "while", "do", "print", "let", "fn",
    "return",
}

local keyword = P(false)
for _,kw in ipairs(keywords) do
    keyword = keyword + kw
end
local function K(kw_str)
    assertf(keyword:match(kw_str), "Undefined keyword %s", kw_str)
    return sp * kw_str * -iden_char * sp -- -iden prevents keyword run-together
end

-- Genericize the pattern for a call so we can support call expressions (calls
-- that return their functions return value) and call statements (calls where
-- the function's return value is discarded.)
local call = sp * V("call") * sp

local expr = sp * V("expr") * sp

local prim_expr = sp * V("prim_expr") * sp

local bin_expr = sp * V("bin_expr") * sp
local term = sp * V("term") * sp

-- TODO Currently, Homebase uses 0 and not-0 as its boolean operands.
-- Expression priorities establishing order of operation. Lower ordinal values
-- indicate higher priority.
local p0_bin_op = C(S("*/%"))
local p1_bin_op = C(S("+-"))
local p2_bin_op = C(P(">=") + "<=" + "==" + "!=" + ">" + "<")
local p0_bin_expr = sp * V("p0_bin_expr") * sp -- Multiply-like operations
local p1_bin_expr = sp * V("p1_bin_expr") * sp -- Add-like operations
local p2_bin_expr = sp * V("p2_bin_expr") * sp -- Comparison operations

local call_expr = V("call_expr")

local stmt = sp * V("stmt") * sp

local stml = sp * V("stml") * sp

local reg_stmt = V("reg_stmt")
local let_stmt = V("let_stmt")
local asgn_stmt = V("asgn_stmt")
local prt_stmt = V("prt_stmt")
local ret_stmt = V("ret_stmt")

local blk_stmt = V("blk_stmt")
local if_cnd_stmt = V("if_cnd_stmt")
local whl_stmt = V("whl_stmt")
local do_blk_stmt = V("do_blk_stmt")

local fn_proto_stmt = V("fn_proto_stmt")
local call_stmt = V("call_stmt")

local comment = V("comment")

local grammar_table = {"prog",
    call = iden * "(" * sp * Ct(((expr * ",")^0 * expr)^0) * ")",

    expr = call_expr + bin_expr + prim_expr,
    prim_expr = num_expr + ("(" * expr * ")") + var_expr,

    bin_expr = p2_bin_expr,
    term = call_expr + prim_expr,
    p0_bin_expr = Ct(term * (p0_bin_op * term)^0) / fold_bin,
    p1_bin_expr = Ct(p0_bin_expr * (p1_bin_op * p0_bin_expr)^0) / fold_bin,
    p2_bin_expr = Ct(p1_bin_expr * (p2_bin_op * p1_bin_expr)^0) / fold_bin,
    call_expr = call / call_expr_ast_node,

    -- Use sp instead of stmt to allow for programs composed of only spaces to
    -- be considered valid programs.
    -- TODO Support the empty statement (e.g. just  a ';')?
    -- TODO Reconsider how comments are included?
    stml = sp * Ct((blk_stmt + reg_stmt + comment)^1) / stml_ast_node,
    reg_stmt = stmt * ";" * sp,
    stmt = asgn_stmt + prt_stmt + let_stmt + call_stmt + ret_stmt,

    -- TODO Right now, if a variable is declared in some outer scope, in an
    -- inner scope we can reassign the variable *and then* declare a new
    -- variable of the same name (e.g. `let x = 2; do x = 3; let x = 4; end`).
    -- The change made in the inner scope to the variable declared in the outer
    -- scope will persist. After the variable of the same name is declared in
    -- the inner scope, all assignments to that name (the shared one) will be
    -- reflected in the new variable of the same name in the inner scope. The
    -- new variable declared in the inner scope will go out of scope once in
    -- the inner scope completes. Is this the behavior we want? Note that Lua
    -- also seems to behave this way.
    --
    -- TODO Figure out what to do when declared but unassigned variable is accessed
    -- TODO Allow multi-assignment statements?
    let_stmt = K"let" * iden * ("=" * expr)^-1 / let_stmt_ast_node,
    asgn_stmt = iden * "=" * expr / asgn_stmt_ast_node,

    -- Allow print statements to be devoid of an expressions so we can print
    -- blank lines in our Homebase programs.
    --
    -- TODO For some reason, LPEG captures the string 'print' if it tries to
    -- parse a print statement with an empty expression. Figure out how to fix
    -- this.
    prt_stmt = K"print" * expr^-1  / prt_stmt_ast_node,

    call_stmt = call / call_stmt_ast_node,

    ret_stmt = K"return" * expr / ret_stmt_ast_node,

    blk_stmt = if_cnd_stmt + whl_stmt + do_blk_stmt + fn_proto_stmt,
    if_cnd_stmt = K"if" * expr * K"then" * stml
                    * Ct(Ct(K"elseif" * expr * K"then" * stml)^0)
                    * (K"else" * stml)^-1 * K"end" / if_cnd_stmt_ast_node,
    whl_stmt = K"while" * expr * K"do" * stml * K"end" / whl_stmt_ast_node,
    do_blk_stmt = K"do" * stml * K"end" / do_blk_stmt_ast_node,

    fn_proto_stmt = K"fn" * iden * "(" * sp * Ct(((iden * ",")^0 * iden)^0)
                    * ")" * stml * K"end" / fn_proto_stmt_ast_node,

    comment = "#" * (1 - line_end)^0 * line_end,

    -- TODO Support the empty program.
    prog = stml + expr,
}
--grammar_table = trace(grammar_table) -- For debugging lexer/parser matching
local grammar = P(grammar_table) * -1

--grammar:pcode()

local function parse(input)
    local ast = grammar:match(input)

    if ast == nil then
        print("An error occurred while parsing")
    end

    return ast
end

local function exec(ast, state)
    return ast:exec(state)
end

-- Application section
local interactive = false

local input
if #arg == 1 then
    if arg[1] == "-" then
        input = read("a")
    else
        local prog_file = open(arg[1])
        input = prog_file:read("a")
        prog_file:close()
    end
else
    interactive = true
end

-- TODO There can be weirdness when surrounding piping or redirecting to stdin.
-- Need to look into this.
--
-- TODO Note that interactive sessions currently are not persistent. It would
-- also be nice if interactive input of potentially long statments (e.g. if
-- statements) could be done over multiple lines.
repeat
::loop_start::
    if interactive then
        stdout:write("> ")
        stdout:flush()

        input = read("l")
        if input == nil then
            print("Exiting...")
            return
        elseif input:byte() == 12 then
            -- TODO To really intercept ctrl-l here without needing to press
            -- the enter key, I'll need to create some termios bindings.
            --
            -- These are the magic control sequences that most terminals use to
            -- clear the screen (i.e. what happens when you press ctrl-l). For
            -- more info on this fascinating (and somewhat confusing) topic,
            -- lookup "ANSI escape sequences" (the Wikipedia article is really
            -- good.)
            stdout:write("\x1b[H\x1b[2J")
            goto loop_start -- Poor man's 'continue'
        end
    end

    local ast = parse(input)
    if ast == nil then
        if interactive then
            goto loop_start
        end

        return
    end

    -- TODO Right now we seperate variables and functions. We may not want to
    -- do this in the future.
    local initial_state = {
        env = new_env(),
        protos = {},
    }
    local result = exec(ast, initial_state)
    printf("Result: %s", result)

    print("Final state:")
    print(pt(initial_state))
until interactive == false

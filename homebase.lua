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
            assertf(not rawget(state.env, self.iden_str),
                    "Error: Attempting to redeclare variable '%s'",
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

local function stml_ast_node(stmts)
    return {
        tag = "stml",
        stmts = stmts,
        exec = function(self, state)
            for _,stmt in ipairs(self.stmts) do
                -- TODO This is specifically to handle comments. Find a better
                -- way to filter out comments
                if type(stmt) == "table" then
                    stmt:exec(state)
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
                self.pass_stmts:exec(state)
            else
                for _,elif in ipairs(self.elseifs) do
                    if elif[1]:exec(state) ~= 0 then
                        elif[2]:exec(state)
                        return
                    end
                end

                fail_stmts:exec(state)
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
                self.body_stmts:exec(state)
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
local function fn_proto_stmt_ast_node(body_stmts)
    return {
        tag = "fn_proto_stmt",
        body_stmts = body_stmts,
	exec = function(self, state)
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
local iden = C((alpha + "_") * iden_char^0)
local iden1 = sp * iden * sp

local var_expr = iden1 / var_expr_ast_node

-- Sanity check for keywords
local keywords = {
    "if", "then", "elseif", "else", "end", "while", "do", "print", "let", "fn"
}

local keyword = P(false)
for _,kw in ipairs(keywords) do
    keyword = keyword + kw
end
local function K(kw_str)
    assertf(keyword:match(kw_str), "Undefined keyword %s", kw_str)
    return sp * kw_str * -iden_char * sp -- -iden prevents keyword run-together
end

local expr = V("expr")
local expr1 = sp * expr * sp

local prim_expr = V("prim_expr")
local prim1_expr = sp * prim_expr * sp

local bin_expr = V("bin_expr")

-- TODO Currently, Homebase uses 0 and not-0 as its boolean operands.
-- Expression priorities establishing order of operation. Lower ordinal values
-- indicate higher priority.
local p0_bin_op = C(S("*/%"))
local p1_bin_op = C(S("+-"))
local p2_bin_op = C(P(">=") + "<=" + "==" + "!=" + ">" + "<")
local p0_bin_expr = sp * V("p0_bin_expr") * sp -- Multiply-like operations
local p1_bin_expr = sp * V("p1_bin_expr") * sp -- Add-like operations
local p2_bin_expr = sp * V("p2_bin_expr") * sp -- Comparison operations

local stmt = V("stmt")
local stmt1 = sp * stmt * sp

local stml = V("stml")

local reg_stmt = V("reg_stmt")
local let_stmt = V("let_stmt")
local asgn_stmt = V("asgn_stmt")
local prt_stmt = V("prt_stmt")

local blk_stmt = V("blk_stmt")
local if_cnd_stmt = V("if_cnd_stmt")
local whl_stmt = V("whl_stmt")
local do_blk_stmt = V("do_blk_stmt")

local comment = V("comment")

local grammar_table = {"prog",
    expr = bin_expr + prim_expr,
    prim_expr = num_expr + ("(" * expr * ")") + var_expr,

    bin_expr = p2_bin_expr,
    p0_bin_expr = Ct(prim1_expr * (p0_bin_op * prim1_expr)^0) / fold_bin,
    p1_bin_expr = Ct(p0_bin_expr * (p1_bin_op * p0_bin_expr)^0) / fold_bin,
    p2_bin_expr = Ct(p1_bin_expr * (p2_bin_op * p1_bin_expr)^0) / fold_bin,

    -- Use sp instead of stmt1 to allow for programs composed of only spaces to
    -- be considered valid programs.
    -- TODO Support the empty statement (e.g. just  a ';')?
    -- TODO Reconsider how comments are included?
    stml = sp * Ct((blk_stmt + reg_stmt + comment)^1) / stml_ast_node,
    reg_stmt = stmt1 * ";" * sp,
    stmt = asgn_stmt + prt_stmt + let_stmt,

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
    let_stmt = K"let" * iden1 * ("=" * expr1)^-1 / let_stmt_ast_node,
    asgn_stmt = iden1 * "=" * expr / asgn_stmt_ast_node,

    -- Allow print statements to be devoid of an expressions so we can print
    -- blank lines in our Homebase programs.
    --
    -- TODO For some reason, LPEG captures the string 'print' if it tries to
    -- parse a print statement with an empty expression. Figure out how to fix
    -- this.
    prt_stmt = K"print" * expr1^-1  / prt_stmt_ast_node,

    blk_stmt = if_cnd_stmt + whl_stmt + do_blk_stmt,
    if_cnd_stmt = K"if" * expr1 * K"then" * stml
                    * Ct(Ct(K"elseif" * expr1 * K"then" * stml)^0)
                    * (K"else" * stml)^-1 * K"end" / if_cnd_stmt_ast_node,
    whl_stmt = K"while" * expr1 * K"do" * stml * K"end" / whl_stmt_ast_node,
    do_blk_stmt = K"do" * stml * K"end" / do_blk_stmt_ast_node,

    fn_proto_stmt = K"fn" * sp * "(" * sp * ")" * stml * K"end"
                    / fn_proto_stmt_ast_node,

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
        funcs = {},
        cur_fn = nil,

    }
    local result = exec(ast, initial_state)
    printf("Result: %s", result)

    print("Final environment:")
    print(pt(initial_state.env))
until interactive == false

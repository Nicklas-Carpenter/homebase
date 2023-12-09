local function printf(tmpl, ...)
    print(string.format(tmpl, ...))
end

local function foo()
    printf("foo x=%s", x)
end

local x = 3

local function bar()
    printf("boo x=%s", x)
end

foo()
bar()

" Vim syntax file
" Language:     Homebase
" Maintainer:   Nicklas Carpenter <carpenter.nicklas@gmail.com> 
" Last Change:  2023 Oct 09

" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim


syn case match

" syncing method
syn sync minlines=1000

" catch errors caused by wrong parenthesis and wrong curly brackets or
" keywords placed outside their respective blocks

syn region luaParen transparent start='(' end=')' contains=TOP,luaParenError
syn match  luaParenError ")"
syn match  homebaseError "}"
syn match  homebaseError "\<\%(end\|else\|elseif\|then\|until\|in\)\>"

" Function declaration
syn region homebaseFunctionBlock transparent matchgroup=luaFunction start="\<fn\>" end="\<end\>" contains=TOP

" else
syn keyword luaCondElse matchgroup=luaCond contained containedin=luaCondEnd else

" then ... end
syn region luaCondEnd contained transparent matchgroup=luaCond start="\<then\>" end="\<end\>" contains=TOP

" elseif ... then
syn region luaCondElseif contained containedin=luaCondEnd transparent matchgroup=luaCond start="\<elseif\>" end="\<then\>" contains=TOP

" if ... then
syn region luaCondStart transparent matchgroup=luaCond start="\<if\>" end="\<then\>"me=e-4 contains=TOP nextgroup=luaCondEnd skipwhite skipempty

" do ... end
syn region luaBlock transparent matchgroup=luaStatement start="\<do\>" end="\<end\>" contains=TOP
" repeat ... until
syn region luaRepeatBlock transparent matchgroup=luaRepeat start="\<repeat\>" end="\<until\>" contains=TOP

" while ... do
syn region luaWhile transparent matchgroup=luaRepeat start="\<while\>" end="\<do\>"me=e-2 contains=TOP nextgroup=luaBlock skipwhite skipempty

" for ... do and for ... in ... do
syn region luaFor transparent matchgroup=luaRepeat start="\<for\>" end="\<do\>"me=e-2 contains=TOP nextgroup=luaBlock skipwhite skipempty

syn keyword luaFor contained containedin=luaFor in

" other keywords
syn keyword homebaseStatement return local break let

" operators
syn keyword luaOperator and or not

syn match homebaseSymbolOperator "[<>=!*/%+-]"

" comments
syn keyword luaTodo            contained TODO FIXME XXX
syn match   luaComment         "#.*$" contains=luaTodo,@Spell

syn keyword luaConstant nil

" strings
syn match  luaSpecial contained #\\[\\abfnrtv'"[\]]\|\\[[:digit:]]\{,3}#
syn region luaString2 matchgroup=luaStringDelimiter start=+\[\[+ end=+\]\]+ contains=luaString2,@Spell
syn region luaString matchgroup=luaStringDelimiter start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=luaSpecial,@Spell
syn region luaString matchgroup=luaStringDelimiter start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=luaSpecial,@Spell

" integer number
syn match luaNumber "\<\d\+\>"
" floating point number, with dot, optional exponent
syn match luaNumber  "\<\d\+\.\d*\%([eE][-+]\=\d\+\)\="
" floating point number, starting with a dot, optional exponent
syn match luaNumber  "\.\d\+\%([eE][-+]\=\d\+\)\=\>"
" floating point number, without dot, with exponent
syn match luaNumber  "\<\d\+[eE][-+]\=\d\+\>"

" hex numbers
syn match luaNumber "\<0[xX][[:xdigit:].]\+\%([pP][-+]\=\d\+\)\=\>"

" tables
syn region luaTableBlock transparent matchgroup=luaTable start="{" end="}" contains=TOP,luaStatement

" methods
syntax match luaFunc ":\@<=\k\+"

" built-in functions
syn keyword luaFunc assert collectgarbage dofile error next
syn keyword luaFunc print rawget rawset self tonumber tostring type _VERSION

" Define the default highlighting.
" Only when an item doesn't have highlighting yet

hi def link homebaseStatement       Statement
hi def link luaRepeat               Repeat
hi def link luaFor                  Repeat
hi def link luaString               String
hi def link luaString2              String
hi def link luaStringDelimiter      luaString
hi def link luaNumber               Number
hi def link luaOperator             Operator
hi def link homebaseSymbolOperator  luaOperator
hi def link luaConstant             Constant
hi def link luaCond                 Conditional
hi def link luaCondElse             Conditional
hi def link luaFunction             Function
hi def link luaMetaMethod           Function
hi def link luaComment              Comment
hi def link luaCommentDelimiter     luaComment
hi def link luaTodo                 Todo
hi def link luaTable                Structure
hi def link homebaseError           Error
hi def link luaParenError           Error
hi def link luaSpecial              SpecialChar
hi def link luaFunc                 Identifier
hi def link luaLabel                Label


let b:current_syntax = "homebase"

let &cpo = s:cpo_save
unlet s:cpo_save
" vim: et ts=8 sw=2

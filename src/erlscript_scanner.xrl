Definitions.
LC= [a-z]
UC= [A-Z_]
L = [A-Za-z_]
D = [0-9]
F = (\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)?
HEX = 0x[0-9A-Fa-f]+
WS  = ([\000-\s]|%.*)
KW = (if|else|script|return|wait)
DS = (>=|=<|&&|==|!=|\|\|)
S = [\(\)\]\[\{\};=><,\+-/\*\.!:]

Rules.
{KW} : {token, {list_to_atom(TokenChars), TokenLine}}.
{DS} : {token, {list_to_atom(TokenChars),TokenLine}}.
{S} : {token, {list_to_atom(TokenChars),TokenLine}}.
{LC}({L}|{D})* : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.
{UC}({L}|{D})* : {token, {var, TokenLine, TokenChars}}.
'({L}|{D})+' : S = strip(TokenChars,TokenLen),
         {token,{string,TokenLine,S}}.
"({L}|{D}|/)*" : S = strip(TokenChars,TokenLen),
         {token,{string,TokenLine,S}}.
{WS}+  : skip_token.
//.* : skip_token.
/\*([^\*]|\*[^/])*\*/ : skip_token.
-?{D}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{F} : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{HEX} : {token, {integer, TokenLine, hex_to_int(TokenChars)}}.

Erlang code.
strip(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).

hex_to_int([_,_|R]) ->
    {ok,[Int],[]} = io_lib:fread("~16u", R),
    Int.


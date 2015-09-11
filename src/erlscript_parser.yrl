%%非终结符
Nonterminals
scripts import statements statement if_statement elseif_statement function args expresses express conditions compare tuple list term arithmetic logic.

%终结符
Terminals '+' '-' '*' '/' '.' '=' ':' '&&' '||' '!' '>' '>=' '=<' '<' '==' '!=' ';' ',' '(' ')' '[' ']' '{' '}' 'if' 'else' 'script' 'return'  atom integer float var.

Rootsymbol scripts.

%%语法分析
scripts -> statements : '$1'.

%%段落分析
statements -> '$empty' : [].
statements -> import '.' statements : [{import,'$1'} | '$3'].         		 %%脚本文件参数导入
statements -> if_statement statements : [{if_statement, '$1'} | '$2'].       %%IF语句
statements -> statement ';' statements : [{statement, '$1'} | '$3'].         %%普通语句

%%参数
import -> '-' 'script' '(' '[' args ']' ')'  :{param, '$5'}.   %%支持参数

%%语句分析
%%IF子句
if_statement -> 'if' '(' conditions ')' '{'  statements '}' : {'if', '$3', '$6'}.
if_statement -> 'if' '(' conditions ')' '{'  statements '}' elseif_statement : {'if', '$3', '$6', 'elseif', '$8'}.
if_statement -> 'if' '(' conditions ')' '{' statements '}' 'else' '{' statements '}' :  {'if','$3','$6', 'else', '$10'}.


elseif_statement -> 'else' 'if' '(' conditions ')' '{' statements '}' : {'elseif', '$4', '$7'}.
elseif_statement -> 'else' 'if' '(' conditions ')' '{' statements '}' elseif_statement : {'elseif', '$4', '$7', 'elseif', '$9'}.
elseif_statement -> 'else' 'if' '(' conditions ')' '{' statements '}' 'else' '{' statements '}' : {'elseif', '$4', '$7', 'else', '$11'}.


%%语句
statement -> expresses  : {expresses, '$1'}.
statement -> term '=' expresses : {match, '$1', '$3'}.
statement -> 'return' expresses : {return, '$2'}.  %%支持脚本返回

%%条件分析
conditions -> expresses : {conditions, '$1'}.
conditions -> expresses logic conditions : {conditions, '$1','$2','$3'}.

%%表达示
expresses -> express : {express, '$1'}.
expresses -> express arithmetic expresses : {arithmetic, '$1', '$2', '$3'}.
expresses -> express compare expresses : {compare, '$1', '$2', '$3'}.

express -> term : {term, '$1'}.
express -> function : {function, '$1'}.
express -> '(' expresses ')' : {priority, '$2'}.%%支持优先级

%%函数
function -> atom '(' args ')' : {func, unwrap('$1'), '$3'}.
function -> atom ':' atom '(' args ')' : {func, unwrap('$1'),  unwrap('$3'), '$5'}.   %%支持命名空间

%%值
term -> tuple 		: {tuple, '$1'}.
term -> list 		: {list, '$1'}.
term -> atom 		: {atom, unwrap('$1')}.
term -> integer 	: {integer, unwrap('$1')}.
term -> float 		: {float, unwrap('$1')}.
term -> var 		: {var, unwrap('$1')}.


%%元组
tuple -> '{' args '}': {element, '$2'}.

%%列表
list -> '[' args ']': {element, '$2'}.

%%参数
args -> '$empty' : [].
args -> expresses : ['$1'].
args -> expresses ',' args : ['$1'| '$3'].


%%逻辑运算
logic -> '&&' : unwrap('$1').
logic -> '||' : unwrap('$1').
logic -> '!' : unwrap('$1').


%%比较运算
compare -> '>' : unwrap('$1').
compare -> '<' : unwrap('$1').
compare -> '>=' : unwrap('$1').
compare -> '=<' : unwrap('$1').
compare -> '==' : unwrap('$1').
compare -> '!=' : unwrap('$1').

%%算术运算
arithmetic -> '+' : unwrap('$1').
arithmetic -> '-' : unwrap('$1').
arithmetic -> '*' : unwrap('$1').
arithmetic -> '/' : unwrap('$1').


Erlang code.
unwrap({_,_,V}) -> V;
unwrap({V,_}) -> V.

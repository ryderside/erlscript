%% @author dzlaozhu35@outlook.com
%% @doc 脚本编译

-module(erlscript_compile).

%% ====================================================================
%% API functions
%% ====================================================================
-export([make/1, make/2]).

-include_lib("eunit/include/eunit.hrl").

%% 脚本代码
-record(script_source, {
    scriptfile = [],   %%文件名
    scriptid = 0,      %%脚本ID
    comment = [],      %%注释
    params = [],       %%脚本参数
    statements = []    %%语法分析结果
}).

%%Options:[{key, value}]
%%{outdir, Dir} : 输出目录
%%{file, FileName} : 输入文件名
make(ScriptDir) ->
    make(ScriptDir, []).
make(ScriptDir, Options) ->
    try
        ScriptFiles = filelib:wildcard(lists:concat([ScriptDir, "/*.script"])),
        make_scripts(ScriptFiles, Options)
    catch _:E ->
        io:format("make script error:~w, tracestack:~p~n",[E, erlang:get_stacktrace()])
    end.

make_scripts(ScriptFiles, Options) ->
    OutDir = proplists:get_value(outdir, Options, "./"),
    FileName = proplists:get_value(file, Options, "script_example.erl"),
    HeadScr = generate_script_module_head(FileName),
    ScriptScources = parse_script(ScriptFiles), %% 解析脚本文件
    CodeSrc = make_script_code(lists:reverse(ScriptScources)), %% 生成代码
    EndScr = generate_script_module_end(),
    FileSrc = lists:concat([HeadScr, CodeSrc, EndScr]),
    io:format("write file : ~ts~n",[filename:join([OutDir,FileName])]),
    ok = file:write_file(filename:join([OutDir,FileName]), unicode:characters_to_binary(FileSrc)).

parse_script(ScriptFiles) ->
    parse_script(ScriptFiles, []).
parse_script([], AccIn) -> AccIn;
parse_script([ScriptFile|Files], AccIn) ->
    FileName = filename:basename(ScriptFile),
    [ScriptId, Comment, _Ext] = string:tokens(FileName, "."),
    {ok, String} = parse_file(ScriptFile),   %% 词法分析
    case erlscript_parser:parse(String) of
        {ok, Statements}  ->
            parse_script(Files,[#script_source{scriptfile = FileName, scriptid = ScriptId, comment = Comment, statements = Statements}|AccIn]);
        Error ->
            io:format("Script:~ts parse error:~p~n", [ScriptId, Error]),
            parse_script(Files, AccIn)
    end.

make_script_code(ScriptScources) ->
    make_script_code(ScriptScources, []).
make_script_code([], AccIn) -> AccIn;
make_script_code([Script|T], AccIn) ->
    ScriptCode = generate_script_code(Script),
    make_script_code(T, lists:concat([AccIn, ScriptCode])).

generate_script_code(#script_source{scriptid = ScriptId, comment = Comment, statements = Statements}) ->
    CommentSrc = generate_comment(Comment),
    FuncSrc = generate_script_exec(ScriptId, Statements),
    lists:concat([CommentSrc, FuncSrc]).

%% @doc 脚本函数参数
generate_args([]) -> "[]";
generate_args([{import, {param, Params}}]) ->
    lists:concat(["[", args(Params), "]"]).

%% @doc 注释
generate_comment(Comment) ->
    lists:concat(["%% @doc ", unicode:characters_to_list(Comment), "\n"]).

%% @doc 脚本函数
generate_script_exec(ScriptId, Statements) ->
    {ImportStatement, CodeStatement} = spilt_import_statement(Statements),
    ArgsSrc = generate_args(ImportStatement),
    FuncSrc = generate_func_head(ScriptId, ArgsSrc),
    BodySrc = generate_func_body(CodeStatement),
    lists:concat([FuncSrc, BodySrc, ";\n\n"]).

%% @doc 函数头
generate_func_head(ScriptId, ArgsSrc) ->
    lists:concat(["script_exec(", ScriptId, ", ", ArgsSrc, ") ->\n"]).

%% @doc 函数体
generate_func_body([]) ->
    lists:concat([indent(1), "ok"]);
generate_func_body(CodeStatement) ->
    statements(CodeStatement, 1).

generate_script_module_head(FileName) ->
    ModuleName = filename:rootname(filename:basename(FileName)),
    lists:concat(["-module(", ModuleName, ").\n\n",
                  "-export([script_exec/1, script_exec/2]).\n\n",
                  "script_exec(ScriptId) ->\n",
                  "\tscript_exec(ScriptId, []).\n\n"]).

generate_script_module_end() ->
    lists:concat(["script_exec(ScriptId, []) ->","\n",
                  "\t", "erlang:error({badmatch, ScriptId}).\n\n"]).
%% ====================================================================
%% DSL(领域特定语言) generate  functions
%% ====================================================================
%% 段落分析
statements(Statements, IndentNum) ->
    statements(Statements, IndentNum, []).
statements([], _IndentNum, InCodeSrc) -> InCodeSrc;
statements([{statement, Statement} | []], IndentNum, InCodeSrc) ->
    CodeSrc = lists:concat([indent(IndentNum), statement(Statement)]),
    statements([], IndentNum, lists:concat([InCodeSrc, CodeSrc]));
statements([{statement, Statement} | OtherStatements], IndentNum, InCodeSrc) ->
    CodeSrc = lists:concat([indent(IndentNum), statement(Statement), ",\n"]),
    statements(OtherStatements, IndentNum, lists:concat([InCodeSrc, CodeSrc]));

%% if
statements([{if_statement, IfStatement} | []], IndentNum, InCodeSrc) ->
    CodeSrc = if_statement(IfStatement, IndentNum),
    statements([], IndentNum, lists:concat([InCodeSrc, CodeSrc]));
statements([{if_statement, IfStatement} | OtherStatements], IndentNum, InCodeSrc) ->
    CodeSrc = lists:concat([if_statement(IfStatement, IndentNum), ",\n"]),
    statements(OtherStatements, IndentNum, lists:concat([InCodeSrc, CodeSrc]));

%% import
statements([{import, _Params} | OtherStatements], IndentNum, InCodeSrc) ->
    statements(OtherStatements, IndentNum, InCodeSrc).

if_statement({'if', Conditions, Statements}, IndentNum) ->
    lists:concat([indent(IndentNum), "case ", conditions(Conditions), " of\n",
                  indent(IndentNum + 1), "true ->\n",
                  statements(Statements, IndentNum+2), ";\n",
                  indent(IndentNum + 1), "false ->\n",
                  indent(IndentNum + 2), "ok\n",
                  indent(IndentNum), "end"]);
if_statement({'if', Conditions, Statements, 'else', ElseStatements}, IndentNum) ->
    lists:concat([indent(IndentNum), "case ", conditions(Conditions), " of\n",
                  indent(IndentNum + 1), "true ->\n",
                  statements(Statements, IndentNum + 2), ";\n",
                  indent(IndentNum + 1), "false ->\n",
                  statements(ElseStatements, IndentNum + 2), "\n",
                  indent(IndentNum), "end"]);
if_statement({'if', Conditions, Statements, 'elseif', ElseStatements}, IndentNum) ->
    lists:concat([indent(IndentNum), "case ", conditions(Conditions), " of\n",
                  indent(IndentNum + 1), "true ->\n",
                  statements(Statements, IndentNum + 2), ";\n",
                  indent(IndentNum + 1), "false ->\n",
                  elseif_statement(ElseStatements, IndentNum + 2), "\n",
                  indent(IndentNum), "end"]).

elseif_statement({'elseif', Conditions, Statements}, IndentNum) ->
    if_statement({'if', Conditions, Statements}, IndentNum);
elseif_statement({'elseif', Conditions, Statements, 'elseif', ElseIfStatement}, IndentNum) ->
    lists:concat([if_statement({'if', Conditions, Statements}, IndentNum),
                  elseif_statement(ElseIfStatement, IndentNum + 1)]);
elseif_statement({'elseif', Conditions, Statements, 'else', ElseStatements}, IndentNum) ->
    lists:concat([indent(IndentNum), "case ", conditions(Conditions), " of\n",
                  indent(IndentNum + 1), "true ->\n",
                  statements(Statements, IndentNum + 2), ";\n",
                  indent(IndentNum + 1), "false ->\n",
                  statements(ElseStatements, IndentNum + 2), "\n",
                  indent(IndentNum), "end"]).


%% 语句分析
statement({expresses, Expresses}) ->
    lists:concat([ expresses(Expresses)]);
statement({match, Term, Expresses}) ->
    lists:concat([term(Term), " = ", expresses(Expresses)]);
statement({return, Expresses}) ->
    lists:concat(["erlang:error({return, ", expresses(Expresses), "})"]).

conditions({conditions, Expresses}) ->
    expresses(Expresses);
conditions({conditions, Expresses, Logic, Conditions}) ->
    lists:concat([expresses(Expresses), logic(Logic), conditions(Conditions)]).

expresses({express, Express}) ->
    express(Express);
expresses({arithmetic, Express, Arithmetic, Expresses}) ->
    lists:concat([express(Express), arithmetic(Arithmetic), expresses(Expresses)]);
expresses({compare, Express, Compare, Expresses}) ->
    lists:concat([express(Express), compare(Compare), expresses(Expresses)]).

express({term, Term}) ->
    term(Term);
express({function, Function}) ->
    function(Function);
express({priority, Expresses}) ->
    lists:concat(["(", expresses(Expresses), ")"]).

function({func, Function, Args}) ->
    lists:concat([Function, "(", args(Args), ")"]);
function({func, Module, Function, Args}) ->
    lists:concat([Module, ":", Function, "(", args(Args), ")"]).

term({tuple, Tuple})        -> tuple(Tuple);
term({list, List})          -> list(List);
term({atom, Atom})          -> Atom;
term({integer, Integer})    -> Integer;
term({float, Float})        -> Float;
term({var, Vars})           -> Vars.

%% 元组
tuple({element, Element}) ->
    lists:concat(["{", args(Element), "}"]).

%% 列表.
list({element, Element}) ->
    lists:concat(["[", args(Element), "]"]).

%% 参数
args(Args) ->
    args(Args, []).
args([], InCodeSrc) ->
    InCodeSrc;
args([Expresses|[]], InCodeSrc) ->
    args([], lists:concat([InCodeSrc, expresses(Expresses)]));
args([Expresses|T], InCodeSrc) ->
    args(T, lists:concat([InCodeSrc, expresses(Expresses), ","])).

%%逻辑运算符
logic('&&') -> " andalso ";
logic('||') -> " orelse ";
logic('!')  -> " not ".

%%比较运算符
compare('>') -> " > ";
compare('<') -> " < ";
compare('>=') -> " >= ";
compare('=<') -> " =< ";
compare('==') -> " == ";
compare('!=') -> " /= ".

%%运算符
arithmetic('+') -> " + ";
arithmetic('-') -> " - ";
arithmetic('*') -> " * ";
arithmetic('/') -> " / ".

%% ====================================================================
%% Internal functions
%% ====================================================================
spilt_import_statement(Statements) ->
    spilt_import_statement(Statements, [], []).
spilt_import_statement([],InImport, InStatements) -> {InImport, lists:reverse(InStatements)};
spilt_import_statement([{import, Import}|T], InImport, InStatements) ->
    spilt_import_statement(T, [{import, Import}|InImport], InStatements);
spilt_import_statement([Tuple|T], InImport, InStatements) ->
    spilt_import_statement(T, InImport, [Tuple|InStatements]).

%% @private
indent(IndentNum) ->
    lists:flatten(lists:duplicate(IndentNum, "\t")).


parse_file(FileName) ->
    {ok, IoDevice} = file:open(FileName, [read]),
    Tokens = parse_file(IoDevice, []),
    file:close(IoDevice),
    {ok, lists:reverse(Tokens)}.

%% @hidden
parse_file(IoDevice, Acc) ->
    case io:request(IoDevice,{get_until,prompt,erlscript_scanner,token,[1]}) of
        {ok, Token, _EndLine} ->
            parse_file(IoDevice, [Token|Acc]);
        {error, token} ->
            exit(scanning_error);
        {eof, _} ->
            Acc
    end.
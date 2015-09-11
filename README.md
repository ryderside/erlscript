# erlscript
custom script file to generate erlang source file

make:

    rebar get-deps && compile 

simple example:

    erlscript_compile:make("./examples").
    erlscript_compile:make("./examples", Options).
    
Options:
    
    [{Option1, Val}, {Option2, Val}...]
    
Option:
    
    outdir:         erlang source file out dir
    file:           erlang source file out filename

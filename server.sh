#!/bin/sh

unamestr=`uname`
if [[ "$unamestr" == 'Linux' || "$unamestr" == 'Darwin' ]]; then
    exename=erl
else
    exename='start //MAX werl.exe'
    #exename='erl.exe'
fi
             
$exename +K true -pa ebin/ -pa deps/*/ebin/ -s tcp_load -type server -ip $1 -port $2

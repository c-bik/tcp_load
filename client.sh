#!/bin/sh

unamestr=`uname`
if [[ "$unamestr" == 'Linux' || "$unamestr" == 'Darwin' ]]; then
    exename=erl
else
    exename='start //MAX werl.exe'
    #exename='erl.exe'
fi
             
$exename +K true -sname client -pa ebin/ -s tcp_load

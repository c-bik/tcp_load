#!/bin/sh

if [[ $# -ne 4 ]]; then
    echo "usage client.sh count source_ip server_ip server_port"
    exit 0
else
    unamestr=`uname`
    if [[ "$unamestr" == 'Linux' || "$unamestr" == 'Darwin' ]]; then
        exename=erl
    else
        exename='start //MAX werl.exe'
        #exename='erl.exe'
    fi
    $exename +K true -pa ebin/ -pa deps/*/ebin/ -s tcp_load -type client -count $1 -sip $2 -dip $3 -port $4
fi

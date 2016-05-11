#!/bin/bash
if [ $# -lt 3 ]
then
    echo "$0 <server_ip> <server_port> <no_of_clients>"
    exit
fi
LDFLAG="-lrt" make tcp_client
for i in `seq 1 $3`;
do
    ./tcp_client $1 $2 &
done
echo "[ENTER] to end"
read key
killall tcp_client

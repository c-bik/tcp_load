# tcp_load
tcp load testing

```erlang
tcp_load:server({127,0,0,1},8888).
tcp_load:clients(100, {127,0,0,1}, 8888).
tcp_load:clients(1000, {127,0,0,1}, 8888).

tcp_load:server({192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,34}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,35}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,36}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,37}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,38}, {192,168,2,42}, 8888).
```

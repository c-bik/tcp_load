# tcp_load
tcp load testing

```erlang
tcp_load:start().
tcp_load:stop().

tcp_load:server({127,0,0,1},8888).
tcp_load:clients(100, {127,0,0,1}, {127,0,0,1}, 8888).

tcp_load:server({192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,34}, {192,168,2,42}, 8888).

# wks010
tcp_load:clients(10000, {192,168,2,38}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,37}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,36}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,35}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,34}, {192,168,2,42}, 8888).

# wks011
tcp_load:clients(10000, {192,168,2,30}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,31}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,32}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,33}, {192,168,2,42}, 8888).
tcp_load:clients(10000, {192,168,2,39}, {192,168,2,42}, 8888).

# wks012 (TBD)
tcp_load:clients(50000, {192,168,2,26}, {192,168,2,42}, 8888).
tcp_load:clients(50000, {192,168,2,27}, {192,168,2,42}, 8888).
tcp_load:clients(50000, {192,168,2,28}, {192,168,2,42}, 8888).
tcp_load:clients(50000, {192,168,2,29}, {192,168,2,42}, 8888).
tcp_load:clients(50000, {192,168,2,40}, {192,168,2,42}, 8888).
```

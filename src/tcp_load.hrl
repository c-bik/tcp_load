-ifndef(_TCP_LOAD_HRL_).
-define(_TCP_LOAD_HRL_, true).

-define(L(__F,__A), io:format("[~p:~p] "__F"~n",[?MODULE,?LINE|__A])).
-define(L(__F), ?L(__F,[])).

-endif. % _TCP_LOAD_HRL_

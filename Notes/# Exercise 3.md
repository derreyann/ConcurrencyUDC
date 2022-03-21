# Exercise 3

**Exercice 1**

**Three function available**

```erlang
-module(notifier)

-export([start/0, register/1, send/2]).
-export([loop/...]).

%API:

start() -> spawn(?MODULE, loop, []).

register(N) -> 
    N!{Register, SELFC}
send(N, Msg) ->
    N!{Send, Msg}

%Internal function

loop() -> 
    receive
        {Register, Pid} ->
            loop([Pid | Pids]);
        {Send, Msg} ->
            send_msg(Msg, Pids),
            loop(pids).
    end.

send_msg(_, []) -> ok;

send_msg(Msg, [P | Pids]) ->
    P!Msg,
    send_msg(Msg,Pids).
```
*Better version of send message*

```erlang
send_msg(Msg, Pids) -> 
    List:ForEach(
        Fun(P) -> P|Msg End,
        Pids
    )

```

**Exercise 2**

```erlang  
-module(store).

-export([start/0, store/2, get/2]).
-export([loop/0]).

start_node() -> 
    spawn(?MODULE, init_node, []).

add_child() -> 
    Tree!{add_child, Child_Tree}.


```
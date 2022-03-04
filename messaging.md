# Messaging 
```
**Something called share nothing???**

Concurrent programming, sending messages between processes. No shared memory, no semaphores nor mutex
Each has copy
    -Managing the shared memory
    -Keeping copies of information coherent

Type of communication: 
    -Direct
    -Queueing
    -Mailboxes

send(destination, type, message)
recv(source, type, message)
```

# introduction to erlang

```erlang
-module(hello_world). %module name
-export(hello_world/0). %list of exported params
hello() ->
    io:format("hello world!"). %calling function in io module
```

## Compiling 

```erlc
erlc hello_world.erl
erl
```

## Factorials

```erlang
-module(facotrial). %module name

-export([fact/1]). %export function fact with 1 arg

fact(N) ->
    if N==0 -> 1;
        true -> N*fact(N-1);
    end.
```

## variables

```erlang
1> N=0.
0
2> N=0.
0
3> N=1.
%should give you an error
```

## Guards 

```erlang
factorial(0) -> 1;
factorial(N) ->
    N*factorial(N-1).
factorial(-1). <-% Never ends
factorial(0) -> 1;
factorial(N) when N>0 -> 
    N*factorial(N-1).
```

#### Type Guards

```erlang
is_number/1
is_integer/1
is_float/1
```

## Tuples

```erlang
{123, "hello"}
{}
```

## Lists

```erlang
[1|[2]|[3]|[4]|[5]]

"abc" = [97, 98, 99]
```
#### Function example 
```erlang
length([]) -> 0;
length([_|T]) -> 1+length(T).
map(F, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].
part([], _, L1, L2) -> {L1,L2};
part([H|T], Piv, L1, L2) ->
    if H<Piv -> part(T, Piv, [H|L1], L2);
        true -> part(T, Piv, L1, [H|L2])
    end.
qsort([]) -> [];
qsort([H|T]) ->
    {L1,L2} = part(T, H, [], []),
    qsort(L1)++[H]++qsort(L2).
```

## Atoms

```
Constants:  
    -everything with lower letter
    -use single quotes ex : 'joe'
```

#### Labeling atoms

```erlang
fact_aux(0, Acc) -> Acc;
fact_aux(N, Acc) -> fact_aux(N-1, N*Acc).
fact(N) ->
    if
        N<0 -> {error, badarg};
        true -> {ok, fact_aux(N, 1)}
    end.
```

## Modules

```erlang
-module(demo).
-export([double/1]).
times(X, N) ->
    X * N.
double(X) ->
    times(X, 2).


% calling a function in the same module

function(Arg1, Arg2, ..., ArgN)

% calling a function in a different module (must include the module name)

module:function(Arg1, Arg2, ..., ArgN)
demo:double(2)
```

## Case

```erlang
average([], Long, Sum) ->
    Sum / Long;
average([H | T], Long, Sum) ->
    average(T, Long + 1, Sum + H).
average(L) ->
    average(L, 0, 0).
```

## Funs (small functions)

```erlang
fun (arg1, arg2, ..., argN) ->
    <body>
end.

Fun  fun(X) -> X end.
% #Fun<erl_eval>
fun(X)->1+X end (2).
% 3
```
#### Import function from library function
```erlang

Fun = fun FunctionName/Arity
Fun={Module, FunctionName}

add(X,Y) ->
    X+Y.
1>Fun=fun add/2.
% #Fun<erl_eval>

```

# List comprehension


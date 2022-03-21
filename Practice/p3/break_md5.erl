-module(break_md5).
-define(PASS_LEN, 6).
-define(UPDATE_BAR_GAP, 1000).
-define(BAR_SIZE, 40).
-define(MAX_TH, 2).
-export([break_md5/1,
  pass_to_num/1,
  num_to_pass/1,
  num_to_hex_string/1,
  hex_string_to_num/1,
  break_md5s/1
]).

-export([break_md5/5]).
-export([progress_loop/5]).

% Base ^ Exp

pow_aux(_Base, Pow, 0) ->
  Pow;
pow_aux(Base, Pow, Exp) when Exp rem 2 == 0 ->
  pow_aux(Base * Base, Pow, Exp div 2);
pow_aux(Base, Pow, Exp) ->
  pow_aux(Base, Base * Pow, Exp - 1).

pow(Base, Exp) -> pow_aux(Base, 1, Exp).

%% Number to password and back conversion

num_to_pass_aux(_N, 0, Pass) -> Pass;
num_to_pass_aux(N, Digit, Pass) ->
  num_to_pass_aux(N div 26, Digit - 1, [$a + N rem 26 | Pass]).

num_to_pass(N) -> num_to_pass_aux(N, ?PASS_LEN, []).

pass_to_num(Pass) ->
  lists:foldl(fun(C, Num) -> Num * 26 + C - $a end, 0, Pass).

%% Hex string to Number

hex_char_to_int(N) ->
  if (N >= $0) and (N =< $9) -> N - $0;
    (N >= $a) and (N =< $f) -> N - $a + 10;
    (N >= $A) and (N =< $F) -> N - $A + 10;
    true -> throw({not_hex, [N]})
  end.

int_to_hex_char(N) ->
  if (N >= 0) and (N < 10) -> $0 + N;
    (N >= 10) and (N < 16) -> $A + (N - 10);
    true -> throw({out_of_range, N})
  end.

hex_string_to_num(Hex_Str) ->
  lists:foldl(fun(Hex, Num) -> Num * 16 + hex_char_to_int(Hex) end, 0, Hex_Str).

num_to_hex_string_aux(0, Str) -> Str;
num_to_hex_string_aux(N, Str) ->
  num_to_hex_string_aux(N div 16,
    [int_to_hex_char(N rem 16) | Str]).

num_to_hex_string(0) -> "0";
num_to_hex_string(N) -> num_to_hex_string_aux(N, []).


progress_loop(N, Bound, T, CheckedSec, LastChecked) ->
  receive
    {stop, Pid} ->
      Pid ! stop;
    {stop_hash, Hashes, Pid} ->
      Pid ! {stop_hashes, Hashes},
      ok;
    {progress_report, Checked} ->
      N2 = N + Checked,
      CheckedSum = CheckedSec + Checked,
      Monotonic_time = erlang:monotonic_time(microsecond),
      TimeMono = Monotonic_time - T,
      Full_N = N2 * ?BAR_SIZE div Bound,
      Full = lists:duplicate(Full_N, $=),
      Empty = lists:duplicate(?BAR_SIZE - Full_N, $-),
      if 1000000 =< TimeMono ->
        io:format("\r[~s~s] ~.2f%, ~w", [Full, Empty, N2 / Bound * 100, CheckedSum]),
        progress_loop(N2, Bound, Monotonic_time, 0, CheckedSum);
        true ->
          io:format("\r[~s~s] ~.2f%, ~w", [Full, Empty, N2 / Bound * 100, LastChecked]),
          progress_loop(N2, Bound, T, CheckedSec, LastChecked)
      end
  end.

break_md5([], _, _, _, Pid) ->
  Pid ! kill,
  ok;

break_md5(Hashes, N, N, _, Pid) ->
  Pid ! {not_found, Hashes},
  ok;

break_md5(Hashes, N, Bound, Progress_Pid, Pid) ->

  receive
    {del, Hashes} ->
      break_md5(Hashes, N, Bound, Progress_Pid, Pid);
    stop -> ok
  after 0 ->
    if N rem ?UPDATE_BAR_GAP == 0 ->
      Progress_Pid ! {progress_report, ?UPDATE_BAR_GAP};
      true ->
        ok
    end,
    Pass = num_to_pass(N),
    Hash = crypto:hash(md5, Pass),
    Num_Hash = binary:decode_unsigned(Hash),
    case lists:member(Num_Hash, Hashes) of
      true ->
        io:format("\e[2K\r~.16B: ~s~n", [Num_Hash, Pass]),
        Pid ! {find, lists:delete(Num_Hash, Hashes)},
        break_md5(lists:delete(Num_Hash, Hashes), N + ?MAX_TH, Bound, Progress_Pid, Pid);
      false ->
        break_md5(Hashes, N + ?MAX_TH, Bound, Progress_Pid, Pid)
    end
  end.

create_threads(Hashes, 0, Bound, Progress_Pid, Num_Threads, List_Pid) ->
  receive
    {stop_hashes, Hashes} -> {not_found, Hashes};
    stop -> ok;
    {find, Hashes} ->
      Fun = fun(Pid_Aux) -> Pid_Aux ! {del, Hashes} end,
      lists:foreach(Fun, List_Pid),
      create_threads(Hashes, 0, Bound, Progress_Pid, Num_Threads, List_Pid);
    kill ->
      if Num_Threads == ?MAX_TH - 1 ->
        Progress_Pid ! {stop, self()},
        create_threads(Hashes, 0, Bound, Progress_Pid, Num_Threads, List_Pid);
        true ->
          create_threads(Hashes, 0, Bound, Progress_Pid, Num_Threads + 1, List_Pid)
      end;
    {not_found, Hashes} ->
      if Num_Threads == ?MAX_TH - 1 ->
        Progress_Pid ! {stop_hashes, Hashes, self()},
        create_threads(Hashes, 0, Bound, Progress_Pid, Num_Threads, List_Pid);
        true ->
          create_threads(Hashes, 0, Bound, Progress_Pid, Num_Threads + 1, List_Pid)
      end
  end;
create_threads(Hashes, Threads, Bound, Progress_Pid, Num_Threads, List_Pid) ->
  Pid = spawn(?MODULE, break_md5, [Hashes, Threads, Bound, Progress_Pid, self()]),
  create_threads(Hashes, Threads - 1, Bound, Progress_Pid, Num_Threads, [Pid | List_Pid]).

break_md5(Hash) ->
  break_md5s([Hash]).

break_md5s(Hashes) ->
  List_Pid = [],
  Bound = pow(26, ?PASS_LEN),
  Progress_Pid = spawn(?MODULE, progress_loop, [0, Bound, erlang:monotonic_time(microsecond), 0, 0]),
  Num_Hashes = lists:map(fun hex_string_to_num/1, Hashes),
  Res = create_threads(Num_Hashes, ?MAX_TH, Bound, Progress_Pid, 1, List_Pid),
  Res.

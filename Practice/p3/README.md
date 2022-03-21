# Concurrency and Parallelism
Degree in Computer Science 2022
Lab 3 – MD5 in Erlang
We are going to reimplement the hash breaking program from the previous assignment using
message passing. The provided break md5 module breaks a hash using a single process.
```erlang
1> break_md5:break_md5("76a2173be6393254e72ffa4d6df1030a").
76A2173BE6393254E72FFA4D6DF1030A: passwd
ok
```
The progress bar runs in its own process, and prints a bar each time it receives an update with
the number of new checks completed.
## Exercise 1 (Check for several hashes) Write a function break md5s/1, that given a list of
hashes finds the corresponding passwords. For example:
```erlang
1> break_md5:break_md5s(["e80b5017098950fc58aad83c8c14978e",
"76a2173be6393254e72ffa4d6df1030a"]).
E80B5017098950FC58AAD83C8C14978E: abcdef
76A2173BE6393254E72FFA4D6DF1030A: passwd
ok
```
## Exercise 2 (Print the number of password checks per second) Print the number of passwords checked every second next to the progress bar.
You can measure time using erlang:monotonic time(microseconds). This functions returns
a monotonic value for the current time in microseconds (i.e. it returns a value that will always
increase in subsequent calls, even if the system time changes)
For example,
```erlang
T1 = erlang:monotonic_time(microseconds),
...
T2 = erlang:monotonic_time(microseconds)
```
T2 - T1 is the elapsed time between the calls to monotonic time.
## Exercise 3 (Break the hashes using several processes) Change the implementation so that
the hashes are calculated and checked using several processes. When a process finds the password
associated with a hash, it should notify the rest of the processes so that they stop checking for
it. The program should stop when there are no more hashes to break, and the processes that we
started should stop.
Checking that all processes stop
Use the debugger (debugger:start()) to check if all processes finish correctly. The debugger
will list all processes running in the modules selected in the menu modules=>interpret. Use that
option to check if all the processes have finished when all passwords have been found.
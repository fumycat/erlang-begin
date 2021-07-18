##### native

```erlang
> c(nt).                  
{ok,nt}
> timer:tc(nt, fib, [30]).
{34930,832040}

> c(nt, native).
{ok,nt}
> timer:tc(nt, fib, [30]).
{9321,832040}
```

#### collections

```erlang
> c(ttrees).
{ok,ttrees}
> timer:tc(ttrees, run_t, []).
{5,ok}
> timer:tc(ttrees, run_d, []).
{26,ok}
> timer:tc(ttrees, run_o, []).
{4,ok}
> timer:tc(ttrees, run_s, []).
{25,ok}
```

Collection | Avg Time (μs) 
---------- | -------------
gb_trees   | 5
dict       | 26
sets       | 25
ordsets    | 6

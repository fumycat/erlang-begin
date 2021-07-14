##### ETS vs DETS

```erlang
> c(myets).
{ok,myets}
> tc:timer(myets, ets_start, []).
{1825,ok}
> tc:timer(myets, dets_start, []).
{12717,ok}
```

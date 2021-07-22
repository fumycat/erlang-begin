### Remote calc

##### 1

```bash
$ erl -sname x1 -setcookie net1
Erlang/OTP 22 [erts-10.6.4] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

Eshell V10.6.4  (abort with ^G)
(x1@hostname)1> C = {x2, 'x2@hostname'}.
{x2,'x2@hostname'}
(x1@hostname)2> C ! {self(), [sqrt, 1024]}.
{<0.85.0>,[sqrt,1024]}
(x1@hostname)3> flush().
Shell got 32.0
ok
(x1@hostname)4>  
```

##### 2

```bash
$ erl -sname x2 -setcookie net1
Erlang/OTP 22 [erts-10.6.4] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

Eshell V10.6.4  (abort with ^G)
(x2@hostname)1> register(x2, self()).
true
(x2@hostname)2> c(calc).
{ok,calc}
(x2@hostname)3> calc:run(). 
[sqrt,1024]
```
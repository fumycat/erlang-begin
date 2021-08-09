```
iex --sname x2@localhost

c "r.ex"
```

```
iex --sname x1@localhost

c "r.ex"
a = ReciverTest.main
send a, "ads"
```

# Main


## Installation

    mix deps.get

## 1. MyXQL test

    iex -S mix

```elixir
iex(1)> XqlTest.run
{:ok,
 %MyXQL.Result{
   columns: ["time", "ip", "method", "path", "status"],
   connection_id: 18,
   last_insert_id: nil,
   num_rows: 2,
   num_warnings: 0,
   rows: [
     [~N[2021-08-04 11:47:37], "127.0.0.1", "GET", "/dasd", 404],
     [~N[2021-08-06 10:01:34], "127.0.0.1", "GET", "/favicon.ico", 404]
   ]
 }}
```

## 2. Run erlang webserver

    docker start mdb1
    iex -S mix

```elixir
iex(2)> Main.start
```

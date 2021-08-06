# Moved to [`../mix_server`](https://github.com/fumycat/erlang-begin/tree/master/mix_server)

# Http Server

## Run

Start database

    docker start mdb1

Run server

    rebar3 shell

```erlang
1> xserver:main().
```

<http://127.0.0.1:8080>


## Database Logs

Connect

    mysql -u root -p -h 127.0.0.1

Show

    MariaDB [(none)]> select * from ws.cons;
    +---------------------+-----------+--------+------------+--------+
    | time                | ip        | method | path       | status |
    +---------------------+-----------+--------+------------+--------+
    | 2021-08-04 11:45:33 | 127.0.0.1 | GET    | /          |    200 |
    | 2021-08-04 11:45:33 | 127.0.0.1 | GET    | /cat.jpg   |    200 |
    | 2021-08-04 11:45:45 | 127.0.0.1 | GET    | /info.html |    200 |
    | 2021-08-04 11:47:37 | 127.0.0.1 | GET    | /dasd      |    404 |
    | 2021-08-04 11:47:39 | 127.0.0.1 | GET    | /          |    200 |
    | 2021-08-04 11:47:39 | 127.0.0.1 | GET    | /cat.jpg   |    200 |
    +---------------------+-----------+--------+------------+--------+
    6 rows in set (0.001 sec)

## Docker database setup

    docker run -p 127.0.0.1:3306:3306  --name mdb1 -e MARIADB_ROOT_PASSWORD=secureasf -d mariadb:latest

and then

    mysql -u root -p -h 127.0.0.1
    
```sql
> CREATE DATABASE ws;
```

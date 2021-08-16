# Mix Server


## Run using docker

1. Run mysql container

```bash
docker start mdb1
```

2. Change config

```bash
mv default.config.xml config.xml

docker inspect mdb1 | grep IPAddress
vim config.xml
```

3. Build and run

```
docker build -t fmxs .
docker run -i -t --rm -p 8080:8080 fmxs bash

root@b4fb3905760e:/mix_server# iex -S mix

iex(1)> Main.start
```

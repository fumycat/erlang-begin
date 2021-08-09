FROM elixir

ENV DBHOST=172.17.0.2

COPY . /mix_server

WORKDIR /mix_server

RUN mix local.hex --force

RUN mix local.rebar --force

RUN mix deps.get

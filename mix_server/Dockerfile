FROM elixir

COPY . /mix_server

WORKDIR /mix_server

RUN mix local.hex --force

RUN mix local.rebar --force

RUN mix deps.get

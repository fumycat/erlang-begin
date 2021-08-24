defmodule ConfigAgent do
  @moduledoc """
    Module for managing application configuration.
  """
  require Logger

  use Agent

  @name {:global, __MODULE__}

  def start_link do
    Agent.start_link(fn -> read_config() end, name: @name)
  end

  def start_link([]) do
    Agent.start_link(fn -> read_config() end, name: @name)
  end

  def read_config do
    {:ok, config} = YamlElixir.read_from_file("config.yaml")
    Logger.info("Config successfully red")
    config["users"]
  end

  def get(username) do
    Agent.get(@name, fn state ->
      Map.get(state, username)
    end)
  end
end

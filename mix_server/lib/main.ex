defmodule Main do
  @moduledoc false

  def start do
    DbConfig.read_config |> :xserver.main
  end
end

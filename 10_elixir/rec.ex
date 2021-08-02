defmodule Rec do

  def go([]) do
    :ok
  end

  def go([h|t]) do
    IO.puts h
    go(t)
  end

end

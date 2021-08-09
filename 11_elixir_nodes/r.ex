defmodule ReciverTest do

  def main do
    Node.spawn_link(:"x2@localhost", ReciverTest, :listen, [])
  end

  def listen do
    receive do
      value ->
        IO.puts value
    end

    listen()
  end
end

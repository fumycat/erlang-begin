defmodule XqlTest do
  def run do
    {:ok, pid} =
      MyXQL.start_link(username: "root", password: "135", hostname: "localhost", database: "ws")

    MyXQL.query(pid, "SELECT * FROM cons WHERE status=404")
  end
end

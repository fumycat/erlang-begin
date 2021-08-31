defmodule XSip.Application do
  @moduledoc """
  Main application.
  """

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      Sippet.child_spec(name: :mystack),
      Sippet.Transports.UDP.child_spec(name: :mystack, port: 5061)
    ]

    opts = [strategy: :one_for_one, name: XSip.Supervisor]
    r = Supervisor.start_link(children, opts)

    Sippet.register_core(:mystack, MyCore)

    spawn(fn ->
      :timer.sleep(1000)
      SippetUtils.init_register()
    end)

    r
  end
end

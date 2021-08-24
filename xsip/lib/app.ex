defmodule XSip.Application do
  @moduledoc """
  Main application.
  """

  use Application

  @impl true
  def start(_type, _args) do

    children = [
      ConfigAgent,
      UsersAgent,
      Sippet.child_spec([name: :mystack]),
      Sippet.Transports.UDP.child_spec([name: :mystack])
    ]


    opts = [strategy: :one_for_one, name: XSip.Supervisor]
    r = Supervisor.start_link(children, opts)

    Sippet.register_core(:mystack, MyCore)

    r
  end
end

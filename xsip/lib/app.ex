defmodule XSip.Application do
  @moduledoc """
  Main application.
  """

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {SipServer, []},
      Sippet.child_spec(name: :mystack),
      Sippet.Transports.UDP.child_spec(name: :mystack, port: 15061)
    ]

    opts = [strategy: :one_for_one, name: XSip.Supervisor]
    r = Supervisor.start_link(children, opts)

    Sippet.register_core(:mystack, MyCore)

    spawn(fn ->
      # Запускает процесс аутентификации
      :timer.sleep(1000)
      SippetUtils.compose_register_request() |> SipServer.init_auth()
    end)

    r
  end
end

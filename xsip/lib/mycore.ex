defmodule MyCore do
  @moduledoc """
    Sippet Core behaviour. All handlers are here.
  """

  require Logger
  use Sippet.Core

  def receive_request(incoming_request, _server_key) do
    # route the request to your UA or proxy process

    IO.puts("request")
    IO.inspect(incoming_request)

    case incoming_request.start_line.method do
      :invite ->
        IO.puts("invite hmm")
        :ok = SipServer.invite(incoming_request)

      _ ->
        Logger.info("else")
    end
  end

  def receive_response(incoming_response, _client_key) do
    # route the response to your UA or proxy process

    IO.puts("response")
    IO.inspect(incoming_response)

    case incoming_response.start_line.status_code do
      401 ->
        Logger.info("Got response 401 (Unauthorized)")
        send(SipServer, {:"401", incoming_response})

      200 ->
        Logger.info("Got response 200 (OK)")

      a ->
        Logger.info("Got response #{a}")
    end
  end

  def receive_error(reason, client_or_server_key) do
    # route the error to your UA or proxy process
    IO.inspect(client_or_server_key)
    IO.inspect(reason)
  end
end

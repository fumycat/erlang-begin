defmodule MyCore do
  @moduledoc """
    Sippet Core behaviour. All handlers are here.
  """

  use Sippet.Core

  def receive_request(incoming_request, server_key) do
    # route the request to your UA or proxy process
    IO.inspect(server_key)
    IO.inspect(incoming_request)
  end

  def receive_response(incoming_response, client_key) do
    # route the response to your UA or proxy process
    IO.inspect(client_key)
    IO.inspect(incoming_response)
  end

  def receive_error(reason, client_or_server_key) do
    # route the error to your UA or proxy process
    IO.inspect(client_or_server_key)
    IO.inspect(reason)
  end
end

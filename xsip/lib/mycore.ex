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
    # IO.inspect(server_key)

    # headers = Map.get(incoming_request, :headers)

    # Logger.info(inspect(headers[:cseq]))

    # case headers[:cseq] do
    #   {_, :register} ->
    #     # {:ok, response} = Sippet.DigestAuth.make_response(incoming_request, 401, "xsip")
    #     # Sippet.send(:mystack, response)

    #     resp = Sippet.Message.to_response(incoming_request, 200)
    #     Sippet.send(:mystack, resp)

    #   {_cseq_num, :invite} ->
    #     # IO.inspect(incoming_request[:body])

    #     {display_name, uri, params} = incoming_request.headers.to
    #     params = Map.put(params, "tag", Sippet.Message.create_tag())

    #     Sippet.Message.to_response(incoming_request, 100)
    #     |> SippetUtils.send(:mystack)

    #     Sippet.Message.to_response(incoming_request, 180)
    #     |> Sippet.Message.put_header(:to, {display_name, uri, params})
    #     |> SippetUtils.send(:mystack)

    #     resp =
    # Sippet.Message.to_response(incoming_request, 200)
    #       |> Sdp.put_sdp(Sdp.mock_sdp_whatever())
    #       |> Sippet.Message.put_header(:to, {display_name, uri, params})
    #       |> SippetUtils.send(:mystack)

    #     [{_, dest_uri, _} | _] = incoming_request[:headers][:contact]

    #     req1 =
    #       Sippet.Message.build_request(:invite, dest_uri)
    #       |> Sdp.put_sdp(Sdp.mock_sdp())
    #       |> Map.put(:headers, resp[:headers])
    #       |> Sippet.Message.put_header(:from, Sippet.Message.get_header(resp, :to))
    #       |> Sippet.Message.put_header(:to, Sippet.Message.get_header(resp, :from))

    #     # |> Sippet.Message.put_header(:cseq, {cseq_num + 1, :invite})
    #     # |> Sippet.Message.put_header(:call_id, Sippet.Message.get_header(resp, :call_id))
    #     # |> Sippet.Message.put_header(:via, Sippet.Message.get_header(resp, :via))

    #     SippetUtils.send(req1, :mystack, true)

    #   _ ->
    #     Logger.info("else")
    # end
  end

  def receive_response(incoming_response, _client_key) do
    # route the response to your UA or proxy process
    IO.puts("response")
    IO.inspect(incoming_response)
    # IO.inspect(client_key)
    case incoming_response.start_line.status_code do
      401 ->
        # TODO move all this to a different module and send incoming_response via send()
        # register stage 2
        IO.puts("Got 401 unauthorized")
        out_request = Agent.get({:global, :ra}, fn s -> s end)
        Agent.stop({:global, :ra})
        {:ok, new_req} =
          Sippet.DigestAuth.make_request(out_request, incoming_response, fn _realm ->
            {:ok, Application.fetch_env!(:xsip, :username),
             Application.fetch_env!(:xsip, :secret)}
          end)

        new_req =
          new_req
          |> Sippet.Message.update_header(:cseq, fn {seq, method} ->
            {seq + 1, method}
          end)
          |> Sippet.Message.update_header_front(:via, fn {ver, proto, hostport, params} ->
            {ver, proto, hostport, %{params | "branch" => Sippet.Message.create_branch()}}
          end)
          |> Sippet.Message.update_header(:from, fn {name, uri, params} ->
            {name, uri, %{params | "tag" => Sippet.Message.create_tag()}}
          end)

        Sippet.send(:mystack, new_req)

      200 ->
        IO.puts("200")

      _ ->
        IO.puts("else")
    end
  end

  def receive_error(reason, client_or_server_key) do
    # route the error to your UA or proxy process
    IO.inspect(client_or_server_key)
    IO.inspect(reason)
  end
end

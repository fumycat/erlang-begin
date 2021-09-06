defmodule SipServer do
  @moduledoc """
    Модуль обработки запросов, которые приходят из MyCore.
  """

  require Logger
  use GenServer

  def init(init_arg) do
    {:ok, init_arg}
  end

  @doc """
    Эта функция будет вызвана при запуске приложения (app.ex).
    В ответ на отправленный register запрос Asterisk вернет ответ 401,
    который будет получен в mycore.ex и отправлен сюда при помощи send.
  """
  def handle_call({:auth, register_request}, _from, state) do
    Logger.info("#{__MODULE__} call :auth")
    :ok = Sippet.send(:mystack, register_request)

    receive do
      {:"401", incoming_response} ->
        {:ok, new_req} =
          Sippet.DigestAuth.make_request(register_request, incoming_response, fn _realm ->
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
    end

    {:reply, :ok, state}
  end

  def handle_call({:invite, in_request}, _from, state) do
    Logger.info("#{__MODULE__} call :invite")

    # trying_response = Sippet.Message.to_response(in_request, 100)
    # Sippet.send(:mystack, trying_response)

    ringing_response = Sippet.Message.to_response(in_request, 180)
    Sippet.send(:mystack, ringing_response)

    # IO.puts("PORT")
    # IO.inspect(Sdp.parse_sdp_body(in_request.body) |> Sdp.get_port_from_sdp())
    port = Sdp.parse_sdp_body(in_request.body) |> Sdp.get_port_from_sdp()

    Task.start(fn ->
      System.cmd(File.cwd!() <> "/a.out", ["voice.wav", "192.168.0.104", "#{port}"])
    end)

    ok_response = Sippet.Message.to_response(in_request, 200)
    ok_response = ok_response |> Sdp.put_sdp(Sdp.mock_sdp())
    Sippet.send(:mystack, ok_response)

    {:reply, :ok, state}
  end

  def start_link(state \\ []) do
    GenServer.start_link(__MODULE__, state, name: __MODULE__)
  end

  def init_auth(register_request), do: GenServer.call(__MODULE__, {:auth, register_request})
  def invite(in_request), do: GenServer.call(__MODULE__, {:invite, in_request})
end

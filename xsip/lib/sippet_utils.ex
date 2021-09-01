defmodule SippetUtils do
  require Logger

  @moduledoc """
    Дополнительные функции для работы с библиотекой Sippet.
  """

  @doc """
    Меняет местами параметры, позволяя использовать pipe оператор |>.
    Возвращает обратно message.
  """
  @spec send(Sippet.Message.t(), atom(), boolean()) :: Sippet.Message.t()
  def send(message, sippet, log \\ false) do
    if log do
      Logger.info("About to send this: " <> inspect(message, pretty: true))
    end

    Logger.info("Validation: " <> inspect(Sippet.Message.validate(message)))

    :ok = Sippet.send(sippet, message)
    message
  end

  @doc """
    Запускает процесс регистрации клиента. Дальнейшее взаимодействие будет обрабатываться в MyCore.
  """
  def init_register do
    port = Application.fetch_env!(:xsip, :port)
    # remote_port = Application.fetch_env!(:xsip, :remote_port)
    host = Application.fetch_env!(:xsip, :host)
    remote_host = Application.fetch_env!(:xsip, :remote_host)
    username = Application.fetch_env!(:xsip, :username)

    sip_uri_to_from = %Sippet.URI{
      authority: username <> "@" <> remote_host,
      headers: nil,
      host: remote_host,
      parameters: ";transport=UDP",
      # port: 5060,
      scheme: "sip",
      userinfo: username
    }

    sip_uri_via_contact = %Sippet.URI{
      authority: username <> "@" <> host,
      headers: nil,
      host: host,
      parameters: ";transport=UDP",
      port: port,
      scheme: "sip",
      userinfo: username
    }

    # Via: SIP/2.0/UDP 192.168.0.101:42544;branch=z9hG4bK-524287-1---841f2f77896f1fe9;rport
    # https://datatracker.ietf.org/doc/html/rfc3261#section-8.1.1.7
    via = [
      {{2, 0}, :udp, {host, port}, %{"branch" => Sippet.Message.create_branch()}}
    ]

    request =
      Sippet.Message.build_request("REGISTER", "sip:" <> remote_host)
      |> Sippet.Message.put_header(:call_id, Sippet.Message.create_call_id())
      |> Sippet.Message.put_header(:cseq, {1, :register})
      |> Sippet.Message.put_header(:from, {"", sip_uri_to_from, %{"tag" => Sippet.Message.create_tag()}} )
      |> Sippet.Message.put_header(:to, {"", sip_uri_to_from, %{}})
      |> Sippet.Message.put_header(:via, via)
      |> Sippet.Message.put_header(:contact, [{"", sip_uri_via_contact, %{}}])

    IO.inspect(Sippet.Message.validate(request))

    IO.inspect(request)

    Agent.start_link(fn -> request end, name: {:global, :ra})

    Sippet.send(:mystack, request)

    # TODO move this to a different module and remove agent

  end
end

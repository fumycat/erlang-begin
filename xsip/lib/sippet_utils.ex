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
  @deprecated "yikes"
  def send(message, sippet, log \\ false) do
    if log do
      Logger.info("About to send this: " <> inspect(message, pretty: true))
    end

    Logger.info("Validation: " <> inspect(Sippet.Message.validate(message)))

    :ok = Sippet.send(sippet, message)
    message
  end

  @doc """
    Создает запрос регистрации. Все параметры берутся из ENV (см. mix.exs).
  """
  @spec compose_register_request() :: Sippet.Message.t()
  def compose_register_request do
    port = Application.fetch_env!(:xsip, :port)
    host = Application.fetch_env!(:xsip, :host)
    remote_host = Application.fetch_env!(:xsip, :remote_host)
    username = Application.fetch_env!(:xsip, :username)

    sip_uri_to_from = %Sippet.URI{
      authority: username <> "@" <> remote_host,
      headers: nil,
      host: remote_host,
      parameters: ";transport=UDP",
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

    # https://datatracker.ietf.org/doc/html/rfc3261#section-8.1.1.7
    via = [
      {{2, 0}, :udp, {host, port}, %{"branch" => Sippet.Message.create_branch()}}
    ]

    Sippet.Message.build_request("REGISTER", "sip:" <> remote_host)
    |> Sippet.Message.put_header(:call_id, Sippet.Message.create_call_id())
    |> Sippet.Message.put_header(:cseq, {1, :register})
    |> Sippet.Message.put_header(
      :from,
      {"", sip_uri_to_from, %{"tag" => Sippet.Message.create_tag()}}
    )
    |> Sippet.Message.put_header(:to, {"", sip_uri_to_from, %{}})
    |> Sippet.Message.put_header(:via, via)
    |> Sippet.Message.put_header(:contact, [{"", sip_uri_via_contact, %{}}])
  end
end

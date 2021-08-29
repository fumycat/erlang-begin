defmodule Sdp do
  @moduledoc """
    Функции для работы с SDP.
  """

  @doc """
    Добавляет body внутрь sip пакета.
    Эта функция сама обновляет Content-length.
  """
  @spec put_body(Sippet.Message.t(), binary()) :: Sippet.Message.t()
  def put_body(message, body) do
    # put_in([:headers, :content_length], byte_size(body))
    Map.update!(message, :body, fn _ ->
      body
    end)
    |> Sippet.Message.put_header(:content_length, byte_size(body))
  end

  @doc """
    Возвращает Keyword list с полями SDP. Возможны дубликаты ключей.
  """
  @spec parse_sdp_body(binary()) :: keyword()
  def parse_sdp_body(body) do
    lines = body |> String.split("\r\n", trim: true)

    Enum.map(lines, fn x ->
      [a, b] = String.split(x, "=")
      {String.to_atom(a), b}
    end)

  end


  @doc """
    TODO
  """
  @spec compose_sdp(keyword()) :: binary()
  def compose_sdp(_kvs) do
    "TODO"
  end

  @doc """
    TODO
  """
  @spec mock_sdp() :: binary()
  def mock_sdp do
    "v=0\r\no=Zoiper 1630120310317 1 IN IP4 192.168.0.104\r\ns=Z\r\nc=IN IP4 192.168.0.104\r\nt=0 0\r\nm=audio 52600 RTP/AVP 0 101 8 3\r\na=rtpmap:101 telephone-event/8000\r\na=fmtp:101 0-16\r\na=sendrecv\r\n"
  end
end
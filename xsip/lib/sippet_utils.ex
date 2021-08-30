defmodule SippetUtils do
  require Logger

  @moduledoc """
    Дополнительные функции для работы с библиотекой Sippet.
  """

  @doc """
    Меняет местами параметры, позволяя использовать pipe оператор |>.
    Возвращает обратно message.
  """
  def send(message, sippet, log \\ false) do
    if log do
      Logger.info("About to send this: " <> inspect(message, pretty: true))
    end

    Logger.info("Validation: " <> inspect(Sippet.Message.validate(message)))

    :ok = Sippet.send(sippet, message)
    message
  end
end

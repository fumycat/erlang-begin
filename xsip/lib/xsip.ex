defmodule Xsip do
  @moduledoc """
  Documentation for `Xsip`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Xsip.hello()
      :world

  """
  def hello do
    Sippet.start_link(name: :mystack)
    Sippet.Transports.UDP.start_link(name: :mystack)

    Sippet.register_core(:mystack, MyCore)


  end
end

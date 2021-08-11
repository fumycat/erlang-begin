defmodule Main do
  @moduledoc """
  Documentation for `Main`.
  """
  @api "https://api.vk.com/method/"

  defp call_vk(method, args) do
    args_list = for {k, v} <- args, do: "#{k}=#{v}"

    request =
      @api <>
        method <>
        "?" <> Enum.join(args_list, "&") <> "&access_token=" <> System.fetch_env!("VK_TOKEN")

    {:ok, request}
  end

  def users_get(user_ids) do
    {:ok, s} = call_vk("users.get", %{user_ids: Enum.join(user_ids, ","), v: "5.131"})
    s
  end

  def mongo do
    {:ok, conn} = Mongo.start_link(url: "mongodb://localhost:27017/vk")
    Mongo.insert_many(conn, "users", test_users(), [])
    :ok
  end

  def test_users do
    :inets.start()

    {:ok, {_status, _headers, body}} =
      users_get(["e1iana", "doombar", "salobur", "tanya_lukasevich", "innerstate"])
      |> :httpc.request()

    JSON.decode!(body) |> Map.get("response")
  end
end

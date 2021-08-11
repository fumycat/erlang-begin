defmodule Main do
  @moduledoc """
  Documentation for `Main`.
  """
  @api "https://api.vk.com/method/"

  defp form_request(method, args) do
    args_list = for {k, v} <- args, do: "#{k}=#{v}"

    @api <> method <> "?" <> Enum.join(args_list, "&") <> "&access_token=" <> System.fetch_env!("VK_TOKEN")
  end

  def users_get(user_ids) do
    form_request("users.get", %{user_ids: Enum.join(user_ids, ","), v: "5.131"})
  end

  def start do
    {:ok, conn} = Mongo.start_link(url: "mongodb://localhost:27017/vk")
    Mongo.insert_many(conn, "users", test_users(), [])
  end

  def test_users do
    :inets.start()

    request_str = users_get(["e1iana", "doombar", "salobur", "tanya_lukasevich", "innerstate"])
    {:ok, {_status, _headers, body}} = :httpc.request(request_str)

    case body |> JSON.decode!() do
      r when is_map_key(r, "response") ->
        Map.get(r, "response")

      r ->
        IO.inspect(r)
        raise "VK Error"
    end

  end
end

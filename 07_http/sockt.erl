-module(sockt).

-export([main/0, client/1]).

-define(not_found,
        <<"HTTP/1.1 404 Not Found\r\nContent-Length: 35\r\n\r\n<html><body>Not "
          "found</body></html>">>).

create_response(Str) ->
    Data = list_to_binary(Str),
    H = list_to_binary("HTTP/1.1 200 OK\r\nContent-Length: "
                       ++ integer_to_list(byte_size(Data))
                       ++ "\r\n\r\n"),
    <<H/binary, Data/binary>>.

route(<<"/">>) ->
    create_response("<html><body>Bienvenue</body></html>");
route(_) ->
    ?not_found.

client(Socket) ->
    {ok, Msg} = gen_tcp:recv(Socket, 0),
    [_, Path, _] = re:split(Msg, "(?<=GET )(.*?)(?= HTTP)"),
    io:format("GET ~p~n", [Path]),
    Html = route(Path),
    gen_tcp:send(Socket, Html),
    gen_tcp:close(Socket).

server(ServerSocket) ->
    {ok, Socket} = gen_tcp:accept(ServerSocket),
    spawn(sockt, client, [Socket]),
    server(ServerSocket).

main() ->
    {ok, ServerSocket} = gen_tcp:listen(8080, [binary, {active, false}, {reuseaddr, true}]),
    server(ServerSocket).

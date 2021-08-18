-module(xserver).

-export([main/1, client/3, make_json/1]).

-define(not_found,
        <<"HTTP/1.1 404 Not Found\r\nContent-Length: 35\r\n\r\n<html><body>Not "
          "found</body></html>">>).

create_response(Data, Headers) ->
    H = list_to_binary("HTTP/1.1 200 OK\r\nContent-Length: "
                       ++ integer_to_list(byte_size(Data))
                       ++ "\r\n"),
    O = list_to_binary(lists:foldl(fun(E, S) -> S ++ E ++ "\r\n" end, "", Headers)),
    <<H/binary, O/binary, <<"\r\n">>/binary, Data/binary>>.

get_file_content(Fname) ->
    {ok, Data} = file:read_file(Fname),
    Data.

route(<<"/">>) ->
    {200, create_response(get_file_content("static/index.html"), [])};
route(<<"/cat.jpg">>) ->
    {200, create_response(get_file_content("static/cat.jpg"), ["Content-Type: image/jpeg;"])};
route(<<"/info.html">>) ->
    {200, create_response(<<"<html><body>Info page</body></html>">>, [])};
route(<<"/show.html">>) ->
    {200, create_response(render_info(), [])};
route(<<"/get_json">>) ->
    {200, create_response(api(), ["Content-Type: application/json;"])};
route(<<"/stats.html">>) ->
    {200, create_response(get_file_content("static/stats.html"), [])};
route(<<"/elm.min.js">>) ->
    {200, create_response(get_file_content("static/elm.min.js"), [])};
route(_) ->
    {404, ?not_found}.

client(Socket, DbPid, Mnesia) ->
    {ok, Msg} = gen_tcp:recv(Socket, 0),
    [_, Path, _] = re:split(Msg, "(?<=GET )(.*?)(?= HTTP)"),

    {ok, {Adr, _}} = inet:peername(Socket),
    AdrString = inet:ntoa(Adr),
    When = calendar:local_time(),

    {Status, Html} = route(Path),

    gen_tcp:send(Socket, Html),
    gen_tcp:close(Socket),

    log_con([When, AdrString, "GET", Path, Status], DbPid, Mnesia).

server(ServerSocket, DbPid, Mnesia) ->
    {ok, Socket} = gen_tcp:accept(ServerSocket),
    spawn(xserver, client, [Socket, DbPid, Mnesia]),
    server(ServerSocket, DbPid, Mnesia).

log_con([When, Adr, Method, Path, Status], MySQLPid, Mnesia) ->
    io:format("~p ~p ~p ~p ~p~n", [When, Adr, Method, Path, Status]),

    case MySQLPid of
        false ->
            _ = ok;
        _ ->
            ok =
                mysql:query(MySQLPid,
                            "INSERT INTO ws.cons VALUE (?, ?, ?, ?, ?)",
                            [When, Adr, Method, Path, Status])
    end,

    case Mnesia of
        true ->
            Uwhen = calendar:datetime_to_gregorian_seconds(When),
            R = mnesia:transaction(fun() -> mnesia:write({con, Uwhen, Adr, Method, Path, Status})
                                   end),
            io:format("Mnesia write ~p~n", [R]);
        _ ->
            _ = ok
    end,
    ok.

mne_start(Config) ->
    case Config of
        {_, {true}} ->
            mnesia:create_schema([node()]),
            mnesia:start(),
            A = mnesia:create_table(con,
                                    [{attributes, [time, ip, method, path, status]},
                                     {disc_copies, [node()]}]),
            io:format("Create mnesia table ~p~n", [A]),
            true;
        _ ->
            false
    end.

mysql_start(Config) ->
    case Config of
        {{true, Host, User, Pass, Db}, {_}} ->
            {ok, Pid} =
                mysql:start_link([{host, binary_to_list(Host)},
                                  {user, binary_to_list(User)},
                                  {password, binary_to_list(Pass)},
                                  {database, binary_to_list(Db)}]),
            ok =
                mysql:query(Pid,
                            "CREATE TABLE IF NOT EXISTS ws.cons (time DATETIME, ip VARCHAR(40), "
                            "method VARCHAR(8), path TEXT, status INTEGER);"),
            Pid;
        _ ->
            false
    end.

make_json(L) ->
    {_, Utime, Host, Method, Path, Status} = L,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(Utime),
    StrTime =
        lists:flatten(
            io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                          [Year, Month, Day, Hour, Minute, Second])),
    #{date => list_to_binary(StrTime),
      addr => list_to_binary(Host),
      method => list_to_binary(Method),
      path => Path,
      status => Status}.

api() ->
    {atomic, Q} =
        mnesia:transaction(fun() ->
                              mnesia:select(con,
                                            [{{con, '$1', '_', '_', '_', '_'},
                                              [{'>', '$1', 0}],
                                              ['$_']}])
                           end),
    case Q of
        [] ->
            jsone:encode(#{error => "empty"});
        _ ->
            ListOfMaps = lists:map(fun(X) -> make_json(X) end, Q),
            jsone:encode(#{data => ListOfMaps})
    end.

view_row([]) ->
    "";
view_row([H | T]) ->
    {_, Utime, Host, Method, Path, Status} = H,
    view_row(T)
    ++ lists:flatten(
           io_lib:format("~p ~s ~s ~s ~p<br>", [Utime, Host, Method, Path, Status])).

render_info() ->
    SomeDate = 63796349103,
    {atomic, Q} =
        mnesia:transaction(fun() ->
                              mnesia:select(con,
                                            [{{con, '$1', '_', '_', '_', '_'},
                                              [{'<', '$1', SomeDate}],
                                              ['$_']}])
                           end),

    {atomic, Q2} =
        mnesia:transaction(fun() ->
                              mnesia:select(con,
                                            [{{con, '$1', '_', '_', '_', '_'},
                                              [{'>', '$1', SomeDate}],
                                              ['$_']}])
                           end),

    C = list_to_binary(view_row(Q)),
    C2 = list_to_binary(view_row(Q2)),

    S1 = <<"<p>Logs before some hardcoded date</p><br>">>,
    S2 = <<"<p>Logs after some hardcoded date</p><br>">>,
    <<S1/binary, C/binary, S2/binary, C2/binary>>.

main(Config) ->
    DbPid = mysql_start(Config),
    Mnesia = mne_start(Config),

    {ok, ServerSocket} = gen_tcp:listen(8080, [binary, {active, false}, {reuseaddr, true}]),
    server(ServerSocket, DbPid, Mnesia).

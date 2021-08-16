-module(xserver).

-export([main/1, client/3]).

-record(cn, {time, ip, method, path, status}).

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
    % Print to console
    io:format("~p ~p ~p ~p ~p~n", [When, Adr, Method, Path, Status]),

    % MySQL
    ok =
    mysql:query(MySQLPid,
                "INSERT INTO ws.cons VALUE (?, ?, ?, ?, ?)",
            [When, Adr, Method, Path, Status]),
    
    % TODO mnesia
    ok.

mne_start(Config) ->
    case Config of
        {_, {true}} ->
            mnesia:create_schema([node()]),
            mnesia:start(),
            % mnesia : create_table ( con , [ ] ) .
            A = mnesia:create_table(con, [{attributes, record_info(fields, cn)}]),
            io:format("~p~n", [A]),
            true;
        _ ->
            false
    end.

mysql_start(Config) ->
    % SqlHost = os:getenv("DBHOST"),
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

main(Config) ->
    DbPid = mysql_start(Config),
    Mnesia = false,
    % Mnesia = mne_start(Config),
    % WW = calendar:local_time(),
    
    % A = mnesia:transaction(fun() -> mnesia:write({con, WW, "testip", "GET", "/", 200}) end),
    % io:format("~p~n", [A]),

    % mnesia:transaction(fun() -> mnesia:match_object({con, '_', '_', '_', '_', '_'}) end).
    % Mnesia.
     
    {ok, ServerSocket} = gen_tcp:listen(8080, [binary, {active, false}, {reuseaddr, true}]),
    server(ServerSocket, DbPid, Mnesia).

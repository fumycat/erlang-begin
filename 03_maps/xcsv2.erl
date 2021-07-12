-module(xcsv2).

-export([xload/1]).

xload(F) ->
    {ok, Of} = file:open(F, [read]),
    file:read_line(Of), % skip header
    Res = xread(Of),
    io:format("~p~n", [lists:filter(fun(E) -> maps:get(age, E) < 18 end, Res)]),
    file:close(Of).


xread(O) -> xread(O, file:read_line(O)).

xread(_O, eof) -> [];

xread(O, {_, Line}) ->
    [Id, Fn, Ln, Em, Ag] = string:split(string:trim(Line), ",", all),
    Map = #{id => erlang:element(1, string:to_integer(Id)),
            first_name => Fn, 
            last_name => Ln, 
            email => Em, 
            age => erlang:element(1, string:to_integer(Ag))},
    [Map] ++ xread(O, file:read_line(O)).

-module(myets).

-export([ets_start/0, dets_start/0]).

ets_start() -> 
    A_Ets = ets:new(myets, []),
    csv_to_ets("test.csv", A_Ets, ets),
    io:format("~p~n", [ets:lookup(A_Ets, 20)]).

dets_start() ->
    {ok, A_Dets} = dets:open_file(mydets, [{auto_save, 1}]),
    csv_to_ets("test.csv", A_Dets, dets),
    io:format("~p~n", [dets:lookup(A_Dets, 20)]).

csv_to_ets(Fname, A_Ets, ModEorD) ->
    {ok, Device} = file:open(Fname, [read]),
    io:get_line(Device, ""), % skip header
    try proc_lines(Device, A_Ets, ModEorD)
        after file:close(Device)
    end.

proc_lines(Device, A_Ets, ModEorD) -> 
    case io:get_line(Device, "") of
        eof -> ok;
        Line ->
            [Id, Fn, Ln, Em, Ag] = string:split(string:trim(Line), ",", all),
            ModEorD:insert(A_Ets, {erlang:element(1, string:to_integer(Id)),
                                   Fn, 
                                   Ln, 
                                   Em, 
                                   erlang:element(1, string:to_integer(Ag))}),
            proc_lines(Device, A_Ets, ModEorD)
    end.

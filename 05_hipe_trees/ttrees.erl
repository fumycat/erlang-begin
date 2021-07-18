-module(ttrees).

-export([run_t/0, run_d/0, run_s/0, run_o/0]).

run_o() ->
    Z = [13, 7, 11, 8, 17, 5, 12, 3, 9, 1, 4, 18, 2, 10, 15, 14, 6, 19, 16],
    A = ordsets:new(),
    ssets(Z, A, ordsets),
    ok.

run_s() ->
    Z = [13, 7, 11, 8, 17, 5, 12, 3, 9, 1, 4, 18, 2, 10, 15, 14, 6, 19, 16],
    A = sets:new(),
    ssets(Z, A, sets),
    ok.

ssets([], _, _) -> ok;
ssets([H | T], Cont, Mod) ->
    Mod:add_element(H, Cont),
    ssets(T, Cont, Mod).

run_d() ->
    Z = [13, 7, 11, 8, 17, 5, 12, 3, 9, 1, 4, 18, 2, 10, 15, 14, 6, 19, 16],
    A = dict:new(),
    dicts(Z, A),
    ok.

dicts([], _) -> ok;
dicts([H | T], Dict) ->
    dict:append(H, H, Dict),
    dicts(T, Dict). 

run_t() ->
    Z = [13, 7, 11, 8, 17, 5, 12, 3, 9, 1, 4, 18, 2, 10, 15, 14, 6, 19, 16],
    A = gb_trees:empty(),
    trees(Z, A),
    ok.

trees([], _) -> ok;
trees([H | T], Tree) ->
    gb_trees:insert(H, H, Tree),
    trees(T, Tree).

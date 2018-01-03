-module(util).

-export([get_server_node/0]).
-export([role/0]).

-export([now_iolist/0]).

-export([
  print_iolist/2
  , println_iolist/1
  , println_iolist/2
]).

-export([get_ip/0]).
-export([get_host/0]).
-export([get_server_ip/0]).

get_ip() ->
  {ok, Ips} = inet:getif(),
  [{Ip, _, _} | _] = lists:filter(
    fun(X) ->
      case X of
        {{127, 0, 0, 1}, _, _} ->
          false;
        _ ->
          true
      end
    end, Ips),
  Ip.

get_host() ->
  {P1, P2, P3, P4} = get_ip(),
  io_lib:format("~w.~w.~w.~w", [P1, P2, P3, P4]).

get_server_ip() ->
  Body = get_host_list(),
  Str = find_service("hash_finder", Body),
  service_host_to_ip(Str).

get_host_list() ->
  inets:start(),
  ssl:start(),
  {ok, {_, _, Body}} = httpc:request(get, {"https://host-list.surge.sh/list", []}, [], [{sync, true}]),
  Body.

get_server_node() ->
  Host = find_service("hash_finder", get_host_list()),
  [Ip, _] = string:split(Host, ":"),
  Name = ["master@", Ip],
%%  Name = ["hash_finder@", Ip],
  Bin = iolist_to_binary(Name),
  binary_to_atom(Bin, latin1).

find_service(Name, Body) ->
  case string:split(Body, "\n") of
    [Line, Res] ->
      case string:split(Line, " ") of
        [Host, Name] ->
          Host;
        [_, _] ->
          find_service(Name, Res)
      end;
    [[]] ->
      {error, not_found}
  end.

service_host_to_ip(Str) ->
  [Ip_Str, _Port_Str] = string:split(Str, ":"),
  List0 = string:split(Ip_Str, ".", all),
  List1 = lists:map(fun(S) -> erlang:list_to_integer(S) end, List0),
  erlang:list_to_tuple(List1).

-spec role() -> worker|master|{error, _}.
role() ->
  Name = erlang:atom_to_list(erlang:node()),
  [Role, _] = string:split(Name, "@"),
  case Role of
    "worker" ->
      worker;
    "master" ->
      master;
    "hash_finder" ->
      master;
    _ ->
      {error, invalid_name}
  end.

now_iolist() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:timestamp()),
  io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Year, Month, Day, Hour, Minute, Second]).

print_iolist(IOList, Args) ->
  Str = lists:flatten(IOList),
  io:fwrite(Str, Args).

println_iolist(IOList) ->
  println_iolist(IOList, []).
println_iolist(IOList, Args) ->
  print_iolist([IOList, "~n"], Args).

-module(utils).
-export([get_child/2]).

get_child(Mod, Name) ->
  Sup = whereis(Mod),
  Childs = supervisor:which_children(Sup),
  case lists:keyfind(Name, 1, Childs) of
    false ->
      {error, not_found};
    {Name, Pid, _, _} ->
      {ok, Pid}
  end.

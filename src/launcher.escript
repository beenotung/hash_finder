#!/usr/bin/env escript

main(_Args) ->
  Self_Ip = util:get_ip(),
  Server_Ip = util:get_server_ip(),
  if
    Self_Ip == Server_Ip and false ->
      io:fwrite("running as master~n");
    true ->
      io:fwrite("running as worker~n"),
      Host = util:get_host(),
      SName = "worker",
      Name = [SName,"@",Host],
      Cookie = "588a30cfed89e04a2f1f6f3a8d63f94e",
      Cmd_Iolist = lists:foldl(fun(C,Acc)->[Acc," ",C]end,[],[
        "erl"
        , "-name", Name
        , "-setcookie", Cookie
        , "-heart"
      ]),
      Cmd_Str = erlang:iolist_to_binary(Cmd_Iolist),
      io:fwrite("cmd=~p~n",[Cmd_Str]),
      os:cmd(Cmd_Str)
  end,
  io:fwrite("[debug] ~p~n",[{self(), node()}]),
  ok.

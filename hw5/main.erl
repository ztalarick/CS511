-module(main).
-author("Zachary Talarick").
-compile(export_all).

start() ->
  { ok , [ N ] } = io : fread ( " enter number of sensors > " , " ~d " ) ,
  if N =< 1 ->
    io : fwrite ( " setup : range must be at least 2~n " , [ ] ) ;
  true ->
    Num_watchers = 1 + ( N div 10) ,
    setup_loop (N , Num_watchers)
  end.

setup_loop(N, Num_watchers)->
    if
      N - 10 == 0 ->
        spawn(watcher, watcher_loop, [10, N - 10, []]);
      N - 10 > 0 ->
        spawn(watcher, watcher_loop, [10, N - 10, []]),
        setup_loop(N - 10, Num_watchers - 1);
      N - 10 < 0 ->
        spawn(watcher, watcher_loop, [N, 0, []])
    end.

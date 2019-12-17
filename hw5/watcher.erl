-module(watcher).
-author("Zachary Talarick").
%-compile(export_all).
-export([watcher_loop/3]).

watcher_loop(Num_sensors, Id, []) ->
  { Pid, _ } = spawn_monitor(sensor, sensor_loop, [Id, self()]),
  watcher_loop(Num_sensors - 1, Id + 1, [{Id, Pid}]);

watcher_loop(Num_sensors, Id, Sensors) ->
  case Num_sensors > 0 of
    false ->
      io:fwrite("Initial Sensors: ~w~n", [Sensors]),
      watcher_loop(Sensors);
    true ->
      { Pid, _ } = spawn_monitor(sensor, sensor_loop, [Id, self()]),
      watcher_loop(Num_sensors - 1, Id + 1, lists:append([{Id, Pid}], Sensors))
  end.

watcher_loop(Sensors) ->
  receive
     {'DOWN', _Ref, process, Pid, anomalous_reading} ->
      Crashed = lists:keyfind(Pid, 2, Sensors),
      Crashed_Id = element(1, Crashed),
      io:fwrite("~nSensor ~w crashed. anomalous_reading~n", [Crashed_Id]),
      io:fwrite("Sensor Restarting~n"),
      {New_Pid, _} = spawn_monitor(sensor, sensor_loop, [Crashed_Id, self()]),
       New_Sensors = lists:keyreplace(Pid, 2, Sensors, {Crashed_Id, New_Pid}),
       io:fwrite("Restarted: ~w~n~n", [New_Sensors]),
       watcher_loop(New_Sensors);
    {Id, Measurement} ->
      io:fwrite("ID: ~w, ", [Id]),
      io:fwrite("Measurement: ~w~n", [Measurement]),
      watcher_loop(Sensors)
  end.

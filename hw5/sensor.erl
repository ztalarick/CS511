-module(sensor).
-author("Zachary Talarick").
-compile(export_all).

sensor_loop(Id, Watcher)->
  Measurement = rand:uniform(11),
  case Measurement of
    11 ->
      %%Sensor fails
      exit(anomalous_reading);
    _Otherwise ->
      %%sensor good
      Watcher!{Id, Measurement}
  end,
  Sleep_time = rand:uniform(10000),
  timer:sleep(Sleep_time),
  sensor_loop(Id, Watcher).

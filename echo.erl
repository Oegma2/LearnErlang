-module(echo).
-compile(export_all).

myworld() ->
   receive
      magic -> io:format("MAGIC WORLD");
      sword -> io:format("Get sword");
      _	    -> io:format("Everything else")
    end,
    myworld().
    
add() ->
   receive
      {From, Value} ->
         From ! Value*Value,
         add()
    end.
    
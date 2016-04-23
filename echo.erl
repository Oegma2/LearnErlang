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

bank(add, {Accno, Val}) ->
   io:format(">>> Adding to account ~w value ~w~n", [Accno, Val]);
bank(remove, {Accno, Val}) ->
   io:format("<<< Removing to account ~w value ~w~n", [Accno, Val]).
   
age(X) when X > 16, X < 100 ->
   io:format("You can drive any car");
age(_) ->
   io:format("Not the right time for driving").
   
gaurd(X, Y) when X > Y, X > 5 ->
   X*Y+Y;
gaurd(_, _) ->
   bad_value_range.
   
even(X) when X rem 2 == 0 -> true;
even(X) when X rem 2 == 1 -> false.

whatnum(X) when is_integer(X) -> {val, "INT"};
whatnum(X) when is_float(X) -> {val, "FLOAT"};
whatnum(_X) -> {val, "NOCLUE"}.
-module(echo).
-compile(export_all).

newEvent({From, Lookup, Desc, Time}) ->
   From ! {eventAdd_OK, {Lookup, Desc, Time}},
   receive
      {From, Lookup, cancel} ->
         From ! {eventCanceled, Lookup}
       after Time ->
         From ! {eventTimeout, {Lookup, Desc}}
    end;
newEvent(_) ->
   {ok, emptyEvent}.

eventServer() ->
   receive
      {newEvent, Lookup, Desc, Time} ->
         spawn(echo, newEvent, [{self(), Lookup, Desc, Time}]),
         eventServer();
      {eventAdd_OK, {Lookup, Desc, Time}} ->
         io:format("Event added ~w ~w ~w ~n", [Lookup, Desc, Time]),
         eventServer();
      {eventTimeout, {Lookup, Desc}} ->
         io:format("Event timed out ~w ~w ~n", [Lookup, Desc]),
         eventServer();
      WhatYaSay ->
         io:format("Sorry dont know this message ~w~n", [WhatYaSay]),
         eventServer()
    end. 
   

start() ->
   register(echo, spawn(echo, myworld, [])).

blowUpAfter(X) ->
   timer:sleep(X),
   exit("((( BOOM )))").

testBoom() ->
   link(spawn(echo,blowUpAfter, [3000])). 

testMonitorBoom() ->
   erlang:monitor(process, spawn(fun() -> timer:sleep(3000) end)).

start(FoodList) ->
   spawn(?MODULE, fridge, [FoodList]).

fridge(FoodList) ->
   receive
      {From, {store, Food}} ->
         From ! {self(), store, ok},
         fridge([Food|FoodList]);
      {From, {take, Item}} ->
         case lists:member(Item, FoodList) of
         true ->
            From ! {self(), {ok, Item}},
            fridge(lists:delete(Item, FoodList));
         false ->
            From ! {self(), {notfound}},
            fridge(FoodList)
         end;
      stop ->
         io:format("Ok exiting\n")
    end.

store(Pid, Food) ->
    Pid ! {self(), {store, Food}}.

take(Pid, Food) ->
    Pid ! {self(), {take, Food}}.
   
createGameTable() ->
   ets:new(myTable, []).
   
createGameBag() ->
   ets:new(myTab2, [bag]).

addData(GameData, Object) ->
   ets:insert(GameData, Object).
   
getData(GameData, LookupKey) ->
   ets:lookup(GameData, LookupKey).

procFile(File) ->
   {ok, FileHandle} = file:open(File, [read]),
   processFile(FileHandle).
   
processFile(FH) ->
    case io:get_line(FH, "") of
    eof -> 
       ok;
    Line ->
       Line1 = string:tokens(Line, ":"),
       io:format("~s", [Line1]),
       processFile(FH)
    end.

loadTest() ->
   G = fun(X) -> io:format("~p~n", [X]) end,
   [spawn(fun() -> G(X) end) || X <- lists:seq(1,300000)].

loadTest2(S, Timeout, TestCount) ->
   G = fun(X) -> S ! {newEvent, X, "funky chick", Timeout} end,
   [spawn(fun() -> G(X) end) || X <- lists:seq(1,TestCount)].   

myworld() ->
   receive
      magic -> 
         io:format("MAGIC WORLD"),
         myworld();
      sword -> 
         io:format("Get sword"),
         myworld();
      stop ->
         io:format("Ok done with the world"),
         true;
       _ -> 
          io:format("EVERYH ELSE"),
          myworld()
    end.

testTimeout() ->
   receive
      stop ->
         true;
      blockA ->
         io:format("Block A"),
         testTimeout();
      blockB ->
         io:format("Block B"),
         testTimeout()
    after 10000 ->
       io:format("Waited long enough")
    end.
    
add() ->
   receive
      {stop, From} ->
         From ! {ok, "Exiting"},
         true;
      {From, Value} ->
         From ! {ok, Value*Value},
         add()
	end.

sendDelayToMyWorld(magic, Pid, TimeToWait) ->
   erlang:send_after(TimeToWait, Pid, magic);
sendDelayToMyWorld(sword, Pid, TimeToWait) ->
   erlang:send_after(TimeToWait, Pid, sword);
sendDelayToMyWorld(X, Pid, TimeToWait) ->
   sendDelayToMyWorld(X, Pid, TimeToWait).

snooze(X) ->
   timer:sleep(X).
   
getRND() ->
   random:uniform().

roundVal(X) ->
   round(X).

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

logToFile(Data) ->
   file:write_file("mylog.txt",Data, [append]).
   
readLogFile() ->
   file:read_file("mylog.txt").

readF(FileToRead) ->
   file:read_file(FileToRead).

wakeMeUp(X) ->
   erlang:send_after(X, self(), wakeup).

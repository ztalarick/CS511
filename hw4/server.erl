-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to leave a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    case maps:is_key(ChatName, State#serv_st.chatrooms) of
      false ->
        C = spawn(chatroom, start_chatroom, [ChatName]),
        New_registrations = maps:put(ChatName, [ClientPID], State#serv_st.registrations);
      true ->
        case maps:get(ChatName, State#serv_st.registrations) == [] of
          true ->
            New_registrations = maps:put(ChatName, lists:append(maps:get(ChatName, State#serv_st.registrations), [ClientPID]), State#serv_st.registrations);
          false ->
            New_registrations = maps:put(ChatName, [ClientPID], State#serv_st.registrations)
        end,
        C = maps:get(ChatName, State#serv_st.chatrooms)
      end,
      ClientNick = maps:get(ClientPID, State#serv_st.nicks),
      C!{self(), Ref, register, ClientPID, ClientNick},

      NewState = #serv_st {
      nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
      registrations = New_registrations,
      chatrooms = maps:put(ChatName, C, State#serv_st.chatrooms)
      },
      %io:format("New State: ~p~n", [NewState]),
    NewState.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    ChatPID = maps:get(ChatName, State#serv_st.chatrooms),
    NewState = State#serv_st{registrations = maps:put(ChatName, lists:delete(ClientPID, maps:get(ChatName, State#serv_st.registrations)), State#serv_st.registrations)},
    ChatPID!{self(), Ref, unregister, ClientPID},
    ClientPID!{self(), Ref, ack_leave},
    NewState.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    case lists:member(NewNick, maps:values(State#serv_st.nicks)) of
      true ->
        NewState = State,
        ClientPID!{self(), Ref, err_nick_used};
      false->
        NewState = State#serv_st{nicks = maps:put(ClientPID, NewNick, State#serv_st.nicks)},
        lists:map(fun(X) -> X!{self(), Ref, update_nick, ClientPID, NewNick} end, maps:values(State#serv_st.chatrooms)),
        ClientPID!{self(), Ref, ok_nick}
    end,
    NewState.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    lists:map(fun(X)-> maps:get(X, State#serv_st.chatrooms)!{self(), Ref, unregister, ClientPID} end, maps:keys(maps:filter(fun(_K,V)-> lists:member(ClientPID, V) end, State#serv_st.registrations))),
    ClientPID!{self(), Ref, ack_quit},
    NewState = State#serv_st{nicks = maps:remove(ClientPID, State#serv_st.nicks),
    registrations = maps:map(fun(K2, _V2)-> maps:update(K2, lists:delete(ClientPID, maps:get(K2, State#serv_st.registrations)), State#serv_st.registrations) end, maps:filter(fun(_K,V)-> lists:member(ClientPID, V) end, State#serv_st.registrations) ) },
    NewState.

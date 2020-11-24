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
	%% client requests to join a chat
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
   ChatMap = State#serv_st.chatrooms,
   
   NNMap = State#serv_st.nicks,

	CNickName = maps:get(ClientPID, NNMap),

	RegMap = State#serv_st.registrations,

   case maps:is_key(ChatName,ChatMap) of
   		false -> NewChatPID = spawn(?MODULE, start_chatroom, [ChatName]),
   				 NewChatMap = maps:put(ChatName,NewChatPID,ChatMap),   				

				NewChatPID!{self(),Ref,register,ClientPID,CNickName},

				NewRegMap = maps:put(ChatName, [ClientPID], RegMap),

				State#serv_st{registrations=NewRegMap, chatrooms=NewChatMap};

   		true -> ChatPID = maps:get(ChatName, ChatMap),

				ChatPID!{self(),Ref,register,ClientPID,CNickName},

				NewRoomRegList =  [ClientPID] ++ maps:get(ChatName,RegMap),

				NewRegMap = maps:update(ChatName, NewRoomRegList, RegMap),

				State#serv_st{registrations=NewRegMap}
	end.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    ChatMap = State#serv_st.chatrooms,

    ChatPID = maps:get(ChatName, ChatMap),

    RegMap = State#serv_st.registrations,

    ConnClients = maps:get(ChatName, RegMap),

    UpdatedConn = lists:delete(ClientPID,ConnClients),

    NewRegMap = maps:update(ChatName, UpdatedConn, RegMap),

    ChatPID!{self(),Ref,unregister, ClientPID},

    ClientPID!{self(), Ref, ack_leave},

    State#serv_st{registrations=NewRegMap}.


%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    CurrClients = State#serv_st.nicks,

    CurrNicks = maps:values(CurrClients),

    case lists:member(NewNick,CurrNicks) of
    	true -> ClientPID!{self(), Ref, err_nick_used};
    	false -> UpdatedClients = maps:update(ClientPID,NewNick, CurrClients),
    				Pred = fun(_K,V) -> lists:member(ClientPID, V) == true end,
    				AllChats = State#serv_st.registrations,
    				ChatsWithClient = maps:filter(Pred,AllChats),
    				ChatPIDS = maps:keys(ChatsWithClient),

    				Fun = fun(ChatPID) -> ChatPID!{self(), Ref, update_nick, ClientPID, NewNick} end,

    				lists:foreach(Fun,ChatPIDS),

    				ClientPID!{self(),Ref,ok_nick},
    				State#serv_st{nicks=UpdatedClients}
    end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    CurrClients = State#serv_st.nicks,

    UpdatedClients = maps:remove(ClientPID,CurrClients),

    Pred = fun(_K,V) -> lists:member(ClientPID, V) == true end,
    AllChats = State#serv_st.registrations,
    ChatsWithClient = maps:filter(Pred,AllChats),
    ChatPIDS = maps:keys(ChatsWithClient),

    Fun = fun(ChatPID) -> ChatPID!{self(), Ref, unregister, ClientPID} end,

    lists:foreach(Fun,ChatPIDS),


    %% PENDING: Remove clients from server registration list

    ClientPID!{self(),Ref,ack_quit}.


-module(restauth_connection).

-behavior(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2]).

%% @doc Starts a gen_server which represents a connetion 
%%      to an RestAuth server.
start_link(Url, User, Password) ->
        gen_server:start_link(?MODULE, {Url, User, Password}, []).

init({Url, User, Password}) ->
    {ok, {Url, User, Password}}.

handle_call(_, _From, State) ->
    {reply, none, State}.

handle_cast(_, State) ->
    {noreply, State}.


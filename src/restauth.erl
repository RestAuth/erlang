-module(restauth).

-export([connect/3]).

%% @doc Starts a connection to an RestAuth server.
-spec connect(httpc:url(), string(), string()) -> pid().
connect(Url, User, Password) ->
    {ok, Pid} = restauth_connection:start_link(Url, User, Password),
    Pid.



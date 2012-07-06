-module(restauth).

-export([connect/3, disconnect/1, get/2, get/3]).

-type connection() :: pid().
-type reason() :: any().
-export_type([connection/0, reason/0]).
-export_type([response_code/0, result/0]).

%% @doc Starts a connection to an RestAuth server.
-spec connect(httpc:url(), string(), string()) -> connection().
connect(Url, User, Password) ->
    {ok, Connection} = restauth_connection:start_link(Url, User, Password),
    Connection.

%% @doc Closes a connetion to an RestAuth server.
-spec disconnect(connection()) -> ok | {error, reason()}.
disconnect(Con) ->
    gen_server:call(Con, close).

-type response_code() :: atom().
-type result() :: {response_code(), httpc:headers(), httpc:body()}.

%% @doc Perform a GET request on the connection. 
%% @todo Allow parameters for get request.
-spec get(connection(), string()) -> result().
get(Con, Url) ->
    get(Con, Url, []).
-spec get(connection(), string(), httpc:headers()) -> result().
%% @doc Perform a GET request on the connection. 
get(Con, Url, Headers) ->
    case gen_server:call(Con, {get, Url, Headers}) of
        {error, Reason} ->
            throw({http_error, Reason});
        Result -> Result
    end.


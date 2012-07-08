-module(restauth).
-include_lib("eunit/include/eunit.hrl").

-export([connect/3, disconnect/1]). 
-export([get/2, get/3, post/3, post/4,put/3, put/4,delete/2,delete/3]).

-type connection() :: pid().
-type reason() :: any().
-type body() :: any().
-export_type([connection/0, reason/0, body/0]).
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

%% @doc Perform a POST request on the connection. 
-spec post(connection(), string(), body()) -> result().
post(Con, Url, Body) ->
    post(Con, Url, Body, []).
-spec post(connection(), string(), body(), httpc:headers()) -> result().
%% @doc Perform a POST request on the connection. 
post(Con, Url, Body, Headers) ->
    case gen_server:call(Con, {post, Url, Body, Headers}) of
        {error, Reason} ->
            throw({http_error, Reason});
        Result -> Result
    end.

%% @doc Perform a PUT request on the connection. 
-spec put(connection(), string(), body()) -> result().
put(Con, Url, Body) ->
    put(Con, Url, Body, []).
-spec put(connection(), string(), body(), httpc:headers()) -> result().
%% @doc Perform a put request on the connection. 
put(Con, Url, Body, Headers) ->
    case gen_server:call(Con, {put, Url, Body, Headers}) of
        {error, Reason} ->
            throw({http_error, Reason});
        Result -> Result
    end.

%% @doc Perform a DELETE request on the connection. 
-spec delete(connection(), string()) -> result().
delete(Con, Url) ->
    delete(Con, Url, []).
-spec delete(connection(), string(), httpc:headers()) -> result().
%% @doc Perform a DELETE request on the connection. 
delete(Con, Url, Headers) ->
    case gen_server:call(Con, {delete, Url, Headers}) of
        {error, Reason} ->
            throw({http_error, Reason});
        Result -> Result
    end.

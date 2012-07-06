-module(restauth_connection).
-include_lib("eunit/include/eunit.hrl").

-behavior(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

assemble_url_test_() ->
    R = "foo/bar",
    [?_assertEqual(R,assemble_url("foo", "bar")),
     ?_assertEqual(R,assemble_url("foo/", "bar")),
     ?_assertEqual(R,assemble_url("foo/", "/bar")),
     ?_assertEqual(R,assemble_url("foo", "/bar")),
     ?_assertEqual("foo//bar",assemble_url("foo", "//bar")),
     ?_assertEqual("foo//bar",assemble_url("foo/", "//bar"))].

%% @doc Appends Path to Head by inserting a '/' and removing
%%      one trailing '/' in Head and one leading '/' in Path. 
-spec assemble_url(string(), string()) -> string().
assemble_url(Head, [$/|Path]) -> 
    assemble_url_(Head, Path);
assemble_url(Head, Path) -> 
    assemble_url_(Head, Path).
assemble_url_(Head, Path) ->
    case lists:last(Head) of
        $/ ->
            Head++Path;
        _Else ->
            Head++"/"++Path end.

%% @doc Translates http code to atoms
-spec translate_code(httpc:status_code()) -> restauth:response_code().
translate_code(200) -> ok;
translate_code(201) -> created;
translate_code(204) -> no_content;
translate_code(400) -> bad_request;
translate_code(401) -> unauthorized;
translate_code(403) -> forbidden;
translate_code(404) -> not_found;
translate_code(406) -> not_acceptable;
translate_code(409) -> conflict;
translate_code(411) -> length_required;
translate_code(412) -> precondition_failed;
translate_code(415) -> unsupported_media_type.

%% @doc Starts a gen_server which represents a connetion 
%%      to an RestAuth server.
start_link(Url, User, Password) ->
    gen_server:start_link(?MODULE, {Url, User, Password}, []).

init({Url, User, Password}) ->
    {ok, {Url, User, Password}}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(close, _From, State) ->
    {stop, normal, ok, State};

handle_call({get, Url, Headers}, _From, {Host, User, Password}) ->
    S = {Host, User, Password},
    DefaultHeaders = [{"Accept", "application/json"}],
    case httpc:request(get, {assemble_url(Host, Url), Headers++DefaultHeaders},[],[]) of
        {error, Reason} ->
            {reply, {error, Reason}, S};
        {ok, {{_HttpVer, StatusCode, _Phrase}, NewHeaders, Body}} ->
            {reply, {translate_code(StatusCode), NewHeaders, Body}, S} 
    end.
        

-module(restauth_tests).

-include_lib("eunit/include/eunit.hrl").

restHost() -> "http://localhost:8000".
restUser() -> "vowi".
restPassword() -> "vowi".
allPaths() -> ["/users/", "/users/foo/", "/users/foo/props/", "/users/foo/props/bar",
"/groups/", "/groups/foo/", 
"/groups/foo/users/",  "/groups/foo/users/bar/", 
"/groups/foo/groups/", "/groups/foo/groups/bar",
"/test/users/", "/test/users/foo/props/", "/test/groups/" ].

connect_test() -> _Pid = restauth:connect(restHost(), restUser(), restPassword()).
disconnect_test() -> 
    Pid = restauth:connect(restHost(), restUser(), restPassword()),
    ok = restauth:disconnect(Pid).

request_test_() ->
    {setup, fun() -> application:start(inets) end, [
        {"Wrong URL", fun() -> Pid = restauth:connect("http://foobar", restUser(), restPassword()), 
                ?assertThrow({http_error, _Reason}, restauth:get(Pid, "foo")) end},
        lists:map(fun(P) -> {"Wrong creditionals for: "++P, 
                        fun() -> Pid = restauth:connect(restHost(), "wrong", "credentials"),
                            ?assertMatch({unauthorized, _, _}, restauth:get(Pid, P)) end
                } end, allPaths())
        ]}.


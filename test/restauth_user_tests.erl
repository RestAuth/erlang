-module(restauth_user_tests).

-include_lib("eunit/include/eunit.hrl").

restHost() -> "http://localhost:8000".
restUser() -> "vowi".
restPassword() -> "vowi".

username() -> unicode:characters_to_binary("mati \x{17de}").
password() -> unicode:characters_to_binary("mati \x{17df}").
propKey() -> unicode:characters_to_binary("mati \x{17e0}").
propVal() -> unicode:characters_to_binary("mati \x{17e1}").
groupname() -> unicode:characters_to_binary("group \x{17e2}").

delete_all_users(Pid) ->
    Users = restauth_user:get_all_users(Pid),
    lists:map(fun(U) -> restauth_user:remove(Pid, U) end, Users).

user_test_() ->
    {setup, 
        fun() -> application:start(inets), 
                Pid = restauth:connect(restHost(), restUser(), restPassword()),
                [] = restauth_user:get_all_users(Pid), 
                Pid        
        end, 
        fun(Pid) -> 
                Users = restauth_user:get_all_users(Pid),
                lists:map(fun(U) -> restauth_user:remove(Pid, U) end, Users)
        end, 
        fun(Pid) -> {foreach, fun() -> delete_all_users(Pid) end, [
        {"Create user", fun() -> 
            restauth_user:create_user(Pid, <<"mati">>, <<"password">>),
            ?assertEqual([<<"mati">>], restauth_user:get_all_users(Pid)),
            ?assert(restauth_user:user_exists(Pid, <<"mati">>))
          end},
        {"Vierify password", fun() -> 
            restauth_user:create_user(Pid, <<"mati">>, <<"password">>),
            ?assert(restauth_user:verify_password(Pid, <<"mati">>, <<"password">>)),
            ?assertNot(restauth_user:verify_password(Pid, <<"mati">>, <<"not password">>))
          end},
        {"Create user with space", fun() -> 
            restauth_user:create_user(Pid, <<"mati space">>, <<"password">>),
            ?assertEqual([<<"mati space">>], restauth_user:get_all_users(Pid)),
            ?assert(restauth_user:user_exists(Pid, <<"mati space">>))
          end},
        {"Create user unicode", fun() -> 
            restauth_user:create_user(Pid, username(), <<"password">>),
            ?assertEqual([username()], restauth_user:get_all_users(Pid)),
            ?assert(restauth_user:user_exists(Pid, username()))
          end},
        {"Create user without password", fun() -> 
            restauth_user:create_user(Pid, username(), ""),
            ?assertEqual([username()], restauth_user:get_all_users(Pid)),
            ?assert(restauth_user:user_exists(Pid, username())),
            ?assertEqual(restauth_user:verify_password(Pid, username(), <<"">>), false),
            ?assertEqual(restauth_user:verify_password(Pid, username(), <<"password">>), false),
            ?assertEqual(restauth_user:verify_password(Pid, username(), password()), false)
          end},
        {"Create user with property", fun() -> 
            Pro = {propKey(), propVal()},
            restauth_user:create_user(Pid, username(), password(), [Pro]),
            ?assert(restauth_user:user_exists(Pid, username())),
            ?assertEqual([Pro], restauth_user:get_properties(Pid, username()))
          end},
        {"Create invalid user", fun() -> 
            ?assertEqual(restauth_user:create_user(Pid, <<"invalid/user">>, <<"password">>),
                {error, precondition_failed}),
            ?assertEqual([], restauth_user:get_all_users(Pid))
          end},
        {"Change password", fun() -> 
            restauth_user:create_user(Pid, <<"mati">>, <<"password">>),
            ?assert(restauth_user:verify_password(Pid, <<"mati">>, <<"password">>)),
            ?assertNot(restauth_user:verify_password(Pid, <<"mati">>, <<"not password">>)),
            restauth_user:set_password(Pid, <<"mati">>, <<"not password">>),
            ?assertNot(restauth_user:verify_password(Pid, <<"mati">>, <<"password">>)),
            ?assert(restauth_user:verify_password(Pid, <<"mati">>, <<"not password">>))
          end},
        {"Remove user", fun() ->
            restauth_user:create_user(Pid, username(), password()),
            restauth_user:remove(Pid, username()), 
            ?assertEqual([], restauth_user:get_all_users(Pid)),
            ?assertNot(restauth_user:user_exists(Pid, username())),
            ?assertEqual(false, restauth_user:verify_password(Pid, username(), password()))
          end},
        {"User created twice", fun() -> 
            restauth_user:create_user(Pid, <<"mati">>, password()),
            ?assertEqual({error, conflict}, restauth_user:create_user(Pid, <<"mati">>, <<"new password">>)),
            ?assertEqual([<<"mati">>], restauth_user:get_all_users(Pid)),
            ?assertNot(restauth_user:verify_password(Pid, <<"mati">>, <<"new password">>)),
            ?assert(restauth_user:verify_password(Pid, <<"mati">>, password()))
          end}
        ]}end}.

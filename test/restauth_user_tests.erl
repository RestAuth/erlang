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
            restauth_user:create_user(Pid, username(), <<"">>),
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
            ?assertEqual(ok, restauth_user:remove(Pid, username())), 
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

property_test_() ->
    {setup, 
        fun() -> Pid = restauth:connect(restHost(), restUser(), restPassword()),
                delete_all_users(Pid),
                restauth_user:create_user(Pid, username(), password()),
                Pid        
        end, 
        fun(Pid) ->
            delete_all_users(Pid)
        end,
        fun(Pid) ->
        {foreach, 
            fun() -> 
                Props= restauth_user:get_properties(Pid, username()),
                lists:map(fun({K,_V}) -> restauth_user:remove_property(Pid,username(), K) end, Props)
            end, 
            [{"Create property", fun() -> 
                restauth_user:create_property(Pid, username(), propKey(), propVal()),
                ?assertEqual([{propKey(), propVal()}], restauth_user:get_properties(Pid, username())),
                ?assertEqual(propVal(), restauth_user:get_property(Pid, username(), propKey()))
              end},
            {"Create property twice", fun() ->
                restauth_user:create_property(Pid, username(), propKey(), propVal()),
                ?assertEqual([{propKey(), propVal()}], restauth_user:get_properties(Pid, username())),
                ?assertEqual(propVal(), restauth_user:get_property(Pid, username(), propKey())),
                ?assertEqual({error, conflict}, restauth_user:create_property(Pid, username(), propKey(), <<"newval">>)),
                ?assertEqual([{propKey(), propVal()}], restauth_user:get_properties(Pid, username())),
                ?assertEqual(propVal(), restauth_user:get_property(Pid, username(), propKey()))
              end},
            {"Create invalid property", fun() ->
                ?assertEqual({error, precondition_failed}, restauth_user:create_property(Pid, username(), <<"foo:bar">>, propVal()))
              end},
            {"Set property", fun() ->
                ?assertEqual(ok, restauth_user:set_property(Pid, username(), {propKey(), <<"bla">>})),
                ?assertEqual(<<"bla">>, restauth_user:get_property(Pid, username(), propKey()))
              end},
            {"Set property twice", fun() ->
                restauth_user:create_property(Pid, username(), propKey(), propVal()),
                ?assertEqual(propVal(), restauth_user:get_property(Pid, username(), propKey())),
                ?assertEqual(ok, restauth_user:set_property(Pid, username(), {propKey(), <<"bla">>})),
                ?assertEqual(<<"bla">>,  restauth_user:get_property(Pid, username(), propKey())),
                ?assertEqual(ok, restauth_user:set_property(Pid, username(), {propKey(), <<"bla23">>})),
                ?assertEqual(<<"bla23">>, restauth_user:get_property(Pid, username(), propKey()))
              end},
            {"Remove property", fun() -> 
                restauth_user:create_property(Pid, username(), propKey(), propVal()),
                ?assertEqual([{propKey(), propVal()}], restauth_user:get_properties(Pid, username())),
                ?assertEqual(propVal(), restauth_user:get_property(Pid, username(), propKey())),
                restauth_user:remove_property(Pid, username(), propKey()),
                ?assertEqual([], restauth_user:get_properties(Pid, username())),
                ?assertEqual({error, not_found}, restauth_user:get_property(Pid, username(), propKey()))
              end},
            {"Remove property from wrong user", fun() -> 
                restauth_user:create_user(Pid, <<"User 2">>, password()),
                restauth_user:create_property(Pid, username(), propKey(), propVal()),
                ?assertEqual({error, not_found}, restauth_user:remove_property(Pid, <<"User 2">>, propKey())),
                ?assertEqual(propVal(), restauth_user:get_property(Pid, username(), propKey())),
                ?assertEqual([], restauth_user:get_properties(Pid, <<"User 2">>))
              end}
            ]
        } end}.

group_test_() ->
    {setup, 
        fun() -> Pid = restauth:connect(restHost(), restUser(), restPassword()),
                delete_all_users(Pid),
                restauth_user:create_user(Pid, username(), password()),
                restauth_group:create(Pid, groupname()),
                Pid        
        end, 
        fun(Pid) ->
            Groups = restauth_group:get_all_groups(Pid),
            lists:map(fun(G) -> restauth_group:remove(Pid,G) end, Groups),
            delete_all_users(Pid)
        end,
        fun(Pid) ->
        {foreach, 
            fun() -> 
                Groups = restauth_user:get_groups(Pid, username()),
                lists:map(fun(G) -> restauth_user:remove_group(Pid,username(), G) end, Groups)
            end, 
            [{"Add group", fun() -> 
                ?assertEqual(ok, restauth_user:add_group(Pid, username(), groupname())),
                ?assertEqual([groupname()], restauth_user:get_groups(Pid, username()))
              end},
             {"In group", fun() -> 
                ?assertEqual(false, restauth_user:in_group(Pid, username(), groupname())),
                ?assertEqual(ok, restauth_user:add_group(Pid, username(), groupname())),
                ?assertEqual(true, restauth_user:in_group(Pid, username(), groupname()))
              end},
             {"Remove group", fun() -> 
                ?assertEqual(false, restauth_user:in_group(Pid, username(), groupname())),
                ?assertEqual(ok, restauth_user:add_group(Pid, username(), groupname())),
                ?assertEqual(true, restauth_user:in_group(Pid, username(), groupname())),
                ?assertEqual(ok, restauth_user:remove_group(Pid, username(), groupname())),
                ?assertEqual(false, restauth_user:in_group(Pid, username(), groupname())),
                ?assertEqual([], restauth_user:get_groups(Pid, username()))
              end}
            ]
        } end}.

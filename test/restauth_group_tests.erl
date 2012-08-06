-module(restauth_group_tests).  
-include_lib("eunit/include/eunit.hrl").

restHost() -> "http://localhost:8000".
restUser() -> "vowi".
restPassword() -> "vowi".

username_1() -> unicode:characters_to_binary("mati 1 \x{17de}").
username_2() -> unicode:characters_to_binary("mati 2 \x{17df}").
username_3() -> unicode:characters_to_binary("mati 3 \x{17e0}").
groupname_1() -> unicode:characters_to_binary("group \x{1bc6}").
groupname_2() -> unicode:characters_to_binary("group \x{1bc7}").
groupname_3() -> unicode:characters_to_binary("group \x{1bc8}").


delete_groups(Pid) ->
    Groups = restauth_group:get_all_groups(Pid),
    lists:map(fun(U) -> restauth_group:remove(Pid, U) end, Groups).

cleanup(Pid) ->
    Users = restauth_user:get_all_users(Pid),
    lists:map(fun(U) -> restauth_user:remove(Pid, U) end, Users),
    delete_groups(Pid).

user_test_() ->
    {setup, 
        fun() -> application:start(inets), 
                Pid = restauth:connect(restHost(), restUser(), restPassword()),
                [] = restauth_user:get_all_users(Pid), 
                [] = restauth_group:get_all_groups(Pid), 
                restauth_user:create_user(Pid, username_1(), <<"foobar">>),
                restauth_user:create_user(Pid, username_2(), <<"foobar">>),
                restauth_user:create_user(Pid, username_3(), <<"foobar">>),
                Pid        
        end, 
        fun(Pid) -> cleanup(Pid) end,
        fun(Pid) -> {foreach, fun() -> delete_groups(Pid) end, [
            {"Create group", fun() -> 
                ?assertEqual(ok, restauth_group:create(Pid, groupname_1())),
                ?assertEqual([groupname_1()], restauth_group:get_all_groups(Pid)),
                ?assert(restauth_group:group_exists(Pid, groupname_1()))
              end},
            {"Create group twice", fun() -> 
                restauth_group:create(Pid, groupname_1()),
                ?assertEqual([groupname_1()], restauth_group:get_all_groups(Pid)),
                ?assert(restauth_group:group_exists(Pid, groupname_1())),
                ?assertEqual({error, conflict}, restauth_group:create(Pid, groupname_1())),
                ?assertEqual([groupname_1()], restauth_group:get_all_groups(Pid)),
                ?assert(restauth_group:group_exists(Pid, groupname_1()))
              end},
            {"Create invalid group", fun() -> 
                ?assertEqual({error, precondition_failed}, restauth_group:create(Pid, <<"foo/bar">>)),
                ?assertEqual([], restauth_group:get_all_groups(Pid))
              end},
            {"Add User", fun() -> 
                restauth_group:create(Pid, groupname_1()),
                restauth_group:create(Pid, groupname_2()),
                ?assertEqual([groupname_1(), groupname_2()], restauth_group:get_all_groups(Pid)),

                ?assertEqual(ok, restauth_group:add_user(Pid, groupname_1(), username_1())),
                ?assertEqual(ok, restauth_group:add_user(Pid, groupname_2(), username_2())),
                ?assertEqual(ok, restauth_group:add_user(Pid, groupname_2(), username_3())),

                ?assertEqual([username_1()], restauth_group:get_members(Pid, groupname_1())),
                ?assertEqual([username_3(), username_2()], restauth_group:get_members(Pid, groupname_2())),

                ?assert(restauth_group:is_member(Pid, groupname_1(), username_1())),
                ?assertNot(restauth_group:is_member(Pid, groupname_1(), username_2())),
                ?assertNot(restauth_group:is_member(Pid, groupname_1(), username_3())),
                ?assertNot(restauth_group:is_member(Pid, groupname_2(), username_1())),
                ?assert(restauth_group:is_member(Pid, groupname_2(), username_2())),
                ?assert(restauth_group:is_member(Pid, groupname_2(), username_3()))
              end}
        ]}
        end}.  

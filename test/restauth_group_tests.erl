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

remove_members(Pid) ->
    lists:map(fun(U) -> restauth_user:remove_user(Pid, groupname_1, U) end, 
        restauth_user:get_members(Pid, groupname_1())),
    lists:map(fun(U) -> restauth_user:remove_user(Pid, groupname_2, U) end, 
        restauth_user:get_members(Pid, groupname_2())),
    lists:map(fun(G) -> restauth_user:remove_subgroup(Pid, groupname_1, G) end, 
        restauth_user:get_subgroups(Pid, groupname_1())),
    lists:map(fun(G) -> restauth_user:remove_subgroup(Pid, groupname_2, G) end, 
        restauth_user:get_subgroups(Pid, groupname_2())).

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
            {"Add user", fun() -> 
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
              end},
          {"Add User to invalid group", fun() ->
                ?assertEqual({error, not_found}, restauth_group:add_user(Pid, groupname_1(), username_1())),
                ?assertEqual([], restauth_group:get_all_groups(Pid)) 
              end},
          {"Add invalid user", fun() ->
                restauth_group:create(Pid, groupname_1()),
                ?assertEqual({error, not_found}, restauth_group:add_user(Pid, groupname_1(), <<"notexistent">>)),
                ?assertNot(restauth_user:user_exists(Pid, <<"notexistent">>))
              end},
          {"Is member", fun() ->
                restauth_group:create(Pid, groupname_1()),
                ?assertEqual(false, restauth_group:is_member(Pid, groupname_1(), username_1())),
                ?assertEqual(ok, restauth_group:add_user(Pid, groupname_1(), username_1())),
                ?assert(restauth_group:is_member(Pid, groupname_1(), username_1()))
              end},
          {"Is Member of invalid group", fun() ->
                ?assertEqual(false, restauth_group:is_member(Pid, groupname_1(), username_1()))
            end},
          {"Remove user", fun() ->
                restauth_group:create(Pid, groupname_1()),
                restauth_group:add_user(Pid, groupname_1(), username_1()),
                restauth_group:add_user(Pid, groupname_1(), username_2()),
                ?assertEqual(lists:sort([username_1(), username_2()]), lists:sort(restauth_group:get_members(Pid, groupname_1()))),
                ?assertEqual(ok, restauth_group:remove_user(Pid, groupname_1(), username_1())),
                ?assertEqual([username_2()], restauth_group:get_members(Pid, groupname_1())),
                ?assertEqual(false, restauth_group:is_member(Pid, groupname_1(), username_1())),
                ?assert(restauth_group:is_member(Pid, groupname_1(), username_2())),
                ?assert(restauth_user:user_exists(Pid, username_1()))
            end},
          {"Remove user which is not a member", fun() ->
                restauth_group:create(Pid, groupname_1()),
                ?assertEqual({error, not_found}, restauth_group:remove_user(Pid, groupname_1(), username_1())),
                ?assertNot(restauth_group:is_member(Pid, groupname_1(), username_2())),
                ?assert(restauth_user:user_exists(Pid, username_1()))
            end},
          {"Remove invalid user", fun() ->
                restauth_group:create(Pid, groupname_1()),
                ?assertEqual({error, not_found}, restauth_group:remove_user(Pid, groupname_1(), <<"notexistent">>)),
                ?assertNot(restauth_user:user_exists(Pid, <<"notexistent">>))
              end},
          {"Remove invalid user from invalid group", fun() ->
                ?assertEqual({error, not_found}, restauth_group:remove_user(Pid, groupname_1(), <<"notexistent">>)),
                ?assertNot(restauth_user:user_exists(Pid, <<"notexistent">>)),
                ?assertNot(restauth_group:group_exists(Pid, groupname_1()))
              end},
          {"Remove user from invalid group", fun() ->
                ?assertEqual({error, not_found}, restauth_group:remove_user(Pid, groupname_1(), username_1())),
                ?assert(restauth_user:user_exists(Pid, username_1()))
              end},
          {"Remove group", fun() ->
                restauth_group:create(Pid, groupname_1()),
                ?assertEqual(ok, restauth_group:remove(Pid, groupname_1())),
                ?assertEqual(false, restauth_group:group_exists(Pid, groupname_1())),
                ?assertEqual([], restauth_group:get_all_groups(Pid))
              end},
          {"Remove invalid group", fun() ->
                ?assertEqual({error, not_found}, restauth_group:remove(Pid, groupname_1())),
                ?assertEqual(false, restauth_group:group_exists(Pid, groupname_1())),
                ?assertEqual([], restauth_group:get_all_groups(Pid))
              end}, 
          {"Get members invalid group", fun() ->
                ?assertEqual({error, not_found}, restauth_group:get_members(Pid, groupname_1()))
              end}
        ]}
        end}.  

metagroup_test_() ->
    {setup, 
        fun() -> application:start(inets), 
                Pid = restauth:connect(restHost(), restUser(), restPassword()),
                [] = restauth_user:get_all_users(Pid), 
                [] = restauth_group:get_all_groups(Pid), 
                restauth_user:create_user(Pid, username_1(), <<"foobar">>),
                restauth_user:create_user(Pid, username_2(), <<"foobar">>),
                restauth_user:create_user(Pid, username_3(), <<"foobar">>),
                restauth_group:create(Pid, groupname_1()),
                restauth_group:create(Pid, groupname_2()),
                Pid        
        end, 
        fun(Pid) -> cleanup(Pid) end,
        fun(Pid) -> {foreach, fun() -> remove_members(Pid) end, [
            {"Simple Inheritance test", fun() ->
                restauth_group:add_user(Pid, groupname_1(), username_1()),
                restauth_group:add_user(Pid, groupname_2(), username_2()),
                ?assertEqual([username_1()], restauth_group:get_members(Pid, groupname_1())),
                ?assertEqual([username_2()], restauth_group:get_members(Pid, groupname_2())),
                ?assert(restauth_group:is_member(Pid, groupname_1(), username_1())),
                ?assert(restauth_group:is_member(Pid, groupname_2(), username_2())),

                % make group 2 a subgroup of group 1
                ?assertEqual(ok, restauth_group:add_subgroup(Pid, groupname_1(), groupname_2())),
                ?assertEqual([username_1()], restauth_group:get_members(Pid, groupname_1())),
                ?assertEqual([username_2()], restauth_group:get_members(Pid, groupname_2())),
                ?assertEqual(lists:sort([username_1(), username_2()]), lists:sort(restauth_group:get_members(Pid, groupname_2()))),
                ?assert(restauth_group:is_member(Pid, groupname_2(), username_1())),
                ?assert(restauth_group:is_member(Pid, groupname_2(), username_2())),

                ?assertEqual([groupname_2], restauth_group:get_subgroups(Pid, groupname_1())),
                ?assertEqual([], restauth_group:get_subgroups(Pid, groupname_2()))
              end},
            {"Add invalid group", fun() -> 
                ?assertEqual({error, not_found}, restauth_group:add_subgroup(pid, groupname_1(), groupname_3())),
                ?assertEqual([], restauth_group:get_subgroups(Pid, groupname_1()))
              end}
    ]}end}.


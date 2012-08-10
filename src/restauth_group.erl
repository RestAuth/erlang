-module(restauth_group).
-include_lib("eunit/include/eunit.hrl").

-export([add_subgroup/3, add_user/3, get_subgroups/2, get_members/2, is_member/3, remove/2, remove_subgroup/3, remove_user/3, create/2, group_exists/2, get_all_groups/1]).

%% @doc Add a sub-group to a group.
-spec add_subgroup(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
add_subgroup(Connection, Group, SubGroup) ->
    GroupUrl = urlencode:escape_uri(Group),
    Body = {[{group, SubGroup}]},
    case restauth:post(Connection, "/groups/"++GroupUrl++"/groups/", Body) of
        {no_content, _H, _B} -> ok;
        {R, _H, _B} -> {error, R}
    end.


%% @doc Add a user to a group.
-spec add_user(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
add_user(Connection, Group, User) ->
    GroupUrl = urlencode:escape_uri(Group),
    Body = {[{user, User}]},
    case restauth:post(Connection, "/groups/"++GroupUrl++"/users/", Body) of
        {no_content, _H, _B} -> ok;
        {R, _H, _B} -> {error, R}
    end.

%% @doc Get a list of sub-groups of a group.
-spec get_subgroups(restauth:conection(), unicode:unicode_binary()) -> [unicode:unicode_binary()] | {error, restauth:response_code()}.
get_subgroups(Connection, Group) -> 
    GroupUrl = urlencode:escape_uri(Group),
    case restauth:get(Connection, "/groups/"++GroupUrl++"/groups/") of
        {ok, _Header, Body} ->
            jiffy:decode(Body);
        {Reason, _H, _B} ->
            {error, Reason}
    end.

%% @doc Get a list of users which are members of a group.
-spec get_members(restauth:conection(), unicode:unicode_binary()) -> [unicode:unicode_binary()] | {error, restauth:response_code()}.
get_members(Connection, Group) ->
    GroupUrl = urlencode:escape_uri(Group),
    case restauth:get(Connection, "/groups/"++GroupUrl++"/users/") of
        {ok, _Header, Body} ->
            jiffy:decode(Body);
        {Reason, _H, _B} ->
            {error, Reason}
    end.

%% @doc Check if the user is a member of a group.
-spec is_member(restauth:conection(), unicode:unicode_binary(), unicode:unicode_binary()) -> boolean() | {error, restauth:response_code()}.
is_member(Connection, Group, User) ->
    GroupUrl = urlencode:escape_uri(Group),
    UserUrl = urlencode:escape_uri(User),
    case restauth:get(Connection, "/groups/"++GroupUrl++"/users/"++UserUrl++"/") of
        {not_found, _H, _B} -> false;
        {no_content, _H, _B} -> true;
        {R, _H, _B} -> {error, R}
    end.

%% @doc Delete a group.
-spec remove(restauth:connection(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
remove(Connection, Group) ->
    GroupUrl = urlencode:escape_uri(Group),
    case restauth:delete(Connection, "/groups/"++GroupUrl++"/") of
        {no_content, _H, _B} -> ok;
        {R, _H, _B} -> {error, R}
    end.

%% @doc Delete a sub-group from a group.
-spec remove_subgroup(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
remove_subgroup(Connection, Group, SubGroup) -> 
    GroupUrl = urlencode:escape_uri(Group),
    SubGroupUrl = urlencode:escape_uri(SubGroup),
    case restauth:delete(Connection, "/groups/"++GroupUrl++"/groups/"++SubGroupUrl++"/") of
        {no_content, _H, _B} -> ok;
        {R, _H, _B} -> {error, R}
    end.

%% @doc Delete a user from a group.
-spec remove_user(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
remove_user(Connection, Group, User) ->
    GroupUrl = urlencode:escape_uri(Group),
    UserUrl = urlencode:escape_uri(User),
    case restauth:delete(Connection, "/groups/"++GroupUrl++"/users/"++UserUrl++"/") of
        {no_content, _H, _B} -> ok;
        {R, _H, _B} -> {error, R}
    end.

%% @doc Create a new group.
-spec create(restauth:connection(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
create(Connection, Group) ->
    Body = {[{group, Group}]},
    case restauth:post(Connection, "/groups/", Body) of
        {ok, _H, B} -> ok;
        {created, _H, B} -> ok;
        {R, _H, _B} -> {error, R}
    end.
    
%% @doc Tests if a group exists.
-spec group_exists(restauth:connection(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
group_exists(Connection, Group) -> 
    GroupUrl = urlencode:escape_uri(Group),
    case restauth:get(Connection, "/groups/"++GroupUrl++"/") of
        {not_found, _H, _B} -> false;
        {no_content, _H, _B} -> true;
        {R, _H, _B} -> {error, R}
    end.

%% @doc Get a list of all groups.
-spec get_all_groups(restauth:conection()) -> [unicode:unicode_binary()] | {error, restauth:response_code()}.
get_all_groups(Connection) ->
   case restauth:get(Connection, "/groups/") of
        {ok, _Header, Body} ->
            jiffy:decode(Body);
        {Reason, _H, _B} ->
            {error, Reason}
    end.
    

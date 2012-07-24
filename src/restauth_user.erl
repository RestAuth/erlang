-module(restauth_user).
-include_lib("eunit/include/eunit.hrl").

-export([add_group/3, create_property/4, get_groups/2, get_properties/2, get_property/3, in_group/3, remove/2,
        remove_group/3, remove_property/3, set_password/3, set_property/3, verify_password/3, create_user/3,
        create_user/4, get_all_users/1, user_exists/2]).
-type property() :: {unicode:unicode_binary(), unicode:unicode_binary()}.
-export_type([property/0]).

%% @doc Make the user a member if the given group.
-spec add_group(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
add_group(Connection, User, Group) -> not_implemented.

%% @doc Create a new property for the user. 
%%      This method fails if the property already exists (returning {error, conflict}).
-spec create_property(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
create_property(Connection, User, Property, Value) -> 
    UserUrl = urlencode:escape_uri(User),
    Body = {[{prop, Property},{value, Value}]},
    case restauth:post(Connection, "/users/"++UserUrl++"/props/", Body) of
        {ok, _H, B} -> ok;
        {R, _H, _B} -> {error, R}
    end.

%% @doc Get all groups that the user is a member of.
-spec get_groups(restauth:connection(), unicode:unicode_binary()) -> list(unicode:unicode_binary()) | {error, restauth:response_code()}.
get_groups(Connection, User) -> not_implemented.

%% @doc Get all properties defined for the user.
-spec get_properties(restauth:connection(), unicode:unicode_binary()) -> list(property()) | {error, restauth:response_code()}.
get_properties(Connection, User) ->
    UserUrl = urlencode:escape_uri(User),
    case restauth:get(Connection, "/users/"++UserUrl++"/props/") of
        {ok, _H, B} -> 
            {List} = jiffy:decode(B),
            List;
        {R, _H, _B} -> {error, R}
    end.


%% @doc Get the value for given property for the user.
-spec get_property(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> unicode:unicode_binary() | {error, restauth:response_code()}.
get_property(Connection, User, Property) -> 
    UserUrl = urlencode:escape_uri(User),
    PropertyUrl = urlencode:escape_uri(Property),
    case restauth:get(Connection, "/users/"++UserUrl++"/props/"++PropertyUrl++"/") of
        {ok, _H, B} -> 
            [Str] = jiffy:decode(B),
            Str;
        {R, _H, _B} -> {error, R}
    end.

%% @doc Check if the user is a member in the given group.
-spec in_group(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> boolean() | {error, restauth:response_code()}.
in_group(Connection, User, Group) -> not_implemented.

%% @doc Remove the user.
-spec remove(restauth:connection(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
remove(Connection, User) -> 
    UserUrl = urlencode:escape_uri(User),
    case restauth:delete(Connection, "/users/"++UserUrl++"/") of
        {no_content, _H, _B} -> ok;
        {R, _H, _B} -> {error, R}
    end.

%% @doc Remove the users membership from the given group.
-spec remove_group(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
remove_group(Connection, User, Group) -> not_implemented.

%% @doc Delete the given property from the user.
-spec remove_property(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
remove_property(Connection, User, Property) ->
    UserUrl = urlencode:escape_uri(User),
    PropertyUrl = urlencode:escape_uri(Property),
    case restauth:delete(Connection, "/users/"++UserUrl++"/props/"++PropertyUrl++"/") of
        {no_content, _H, _B} -> ok;
        {R, _H, _B} -> {error, R}
    end.

%% @doc Set the password of the user. If empty unicode:unicode_binary, the user is effectively disabled.
-spec set_password(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
set_password(Connection, User, Password) ->
    UserUrl  = urlencode:escape_uri(User),
    Body = {[{password, Password}]},
    case restauth:put(Connection, "/users/"++UserUrl++"/", Body) of
        {no_content, _H, _B} -> ok;
        {Reason, _H, _B} ->
            {error, Reason}
    end.

%% @doc Set a property for the user. This method overwrites any previous entry.
-spec set_property(restauth:connection(), unicode:unicode_binary(), property()) -> ok | {error, restauth:response_code()}.
set_property(Connection, User, {Key, Value}) ->
    UserUrl  = urlencode:escape_uri(User),
    PropertyUrl  = urlencode:escape_uri(Key),
    Body = {[{value, Value}]},
    case restauth:put(Connection, "/users/"++UserUrl++"/props/"++PropertyUrl++"/", Body) of
        {ok, _H, _B} -> ok;
        {created, _H, _B} -> ok;
        {Reason, _H, _B} ->
            {error, Reason}
    end.

%% @doc Verify the given password.  'true' if the password is correct, 
%%      'false' if the password is wrong or the user does not exist.
-spec verify_password(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> boolean() | {error, restauth:response_code()}.
verify_password(Connection, User, Password) -> 
    UserUrl  = urlencode:escape_uri(User),
    Body = {[{password, Password}]},
    case restauth:post(Connection, "/users/"++UserUrl++"/", Body) of
        {no_content, _H, _B} -> true;
        {not_found, _H, _B} -> false;
        {Reason, _H, _B} ->
            {error, Reason}
    end.


%% @doc Create a new user in the RestAuth database.
-spec create_user(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary()) -> ok | {error, restauth:response_code()}.
create_user(Connection, UserName, Password) -> create_user(Connection, UserName, Password, []).

%% @doc Create a new user in the RestAuth database.
-spec create_user(restauth:connection(), unicode:unicode_binary(), unicode:unicode_binary(), [property()]) -> ok | {error, restauth:response_code()}.
create_user(Connection, UserName, Password, Properties) ->
    User = {[{user, UserName},{password, Password}, {properties, {Properties}}]},
    case restauth:post(Connection, "/users/", User) of
        {created, _H, _B} -> ok;
        {Reason, _H, _B} ->
            {error, Reason}
    end.

%% @doc Returns a list with all user names.
-spec get_all_users(restauth:connection()) -> list(unicode:unicode_binary()) | {error, restauth:response_code()}.
get_all_users(Connection) -> 
    case restauth:get(Connection, "/users/") of
        {ok, _Header, Body} ->
            jiffy:decode(Body);
        {Reason, _H, _B} ->
            {error, Reason}
    end.


%% @doc Tests if a user exists.
-spec user_exists(restauth:connection(), unicode:unicode_binary()) -> boolean() | {error, restauth:response_code()}.
user_exists(Connection, User) -> 
    UserUrl = urlencode:escape_uri(User),
    case restauth:get(Connection, "/users/"++UserUrl++"/") of
        {not_found, _H, _B} -> false;
        {no_content, _H, _B} -> true;
        {R, _H, _B} -> {error, R}
    end.

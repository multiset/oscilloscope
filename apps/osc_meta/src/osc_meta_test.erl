-module(osc_meta_test).

-export([
    user_basics/0,
    test_org/0
]).

user_basics() ->
    {ok, UserID} = osc_meta_user:create(<<"ben">>, <<"passwd">>),
    ok = osc_meta_user:add_email(UserID, <<"ben@ben.com">>),
    ok = osc_meta_user:add_email(UserID, <<"other@ben.com">>),
    {ok, UserProps} = osc_meta_user:lookup(UserID),
    {id, UserID} = lists:keyfind(id, UserProps),
    {name, <<"ben">>} = lists:keyfind(name, UserProps),
    {password, _Hash} = lists:keyfind(password, UserProps),
    ok = osc_meta_user:delete(UserID),
    not_found = osc_meta_user:lookup(UserID).

test_org() ->
    % user member of multiple teams of multiple orgs
    {ok, UserID1} = osc_meta_user:create(<<"ben1">>, <<"passwd">>),
    ok = osc_meta_user:add_email(UserID1, <<"ben@ben.com">>),
    ok = osc_meta_user:add_email(UserID1, <<"other@ben.com">>),
    {ok, UserID2} = osc_meta_user:create(<<"ben2">>, <<"passwd">>),
    {ok, UserID3} = osc_meta_user:create(<<"ben3">>, <<"passwd">>),
    {ok, UserID4} = osc_meta_user:create(<<"ben4">>, <<"passwd">>),
    {ok, UserID5} = osc_meta_user:create(<<"ben5">>, <<"passwd">>),
    {ok, OrgID1} = osc_meta_org:create(<<"orgz1">>, UserID1),
    {ok, OrgID2} = osc_meta_org:create(<<"orgz2">>, UserID1),
    {ok, OrgID3} = osc_meta_org:create(<<"orgz3">>, UserID2),

    true = osc_meta_org:is_owner(OrgID1, UserID1),
    true = osc_meta_org:is_owner(OrgID2, UserID1),
    false = osc_meta_org:is_owner(OrgID3, UserID1),
    false = osc_meta_org:is_owner(OrgID1, UserID2),
    false = osc_meta_org:is_owner(OrgID2, UserID2),
    true = osc_meta_org:is_owner(OrgID3, UserID2),

    true = osc_meta_org:is_member(OrgID1, UserID1),
    true = osc_meta_org:is_member(OrgID2, UserID1),
    false = osc_meta_org:is_member(OrgID3, UserID1),
    false = osc_meta_org:is_member(OrgID1, UserID2),
    false = osc_meta_org:is_member(OrgID2, UserID2),
    true = osc_meta_org:is_member(OrgID3, UserID2),

    ok = osc_meta_org:add_member(OrgID1, UserID2),
    ok = osc_meta_org:add_member(OrgID1, UserID3),
    ok = osc_meta_org:add_member(OrgID2, UserID3),
    ok = osc_meta_org:add_member(OrgID1, UserID4),
    ok = osc_meta_org:add_member(OrgID1, UserID5),

    true = osc_meta_org:is_member(OrgID1, UserID2),
    true = osc_meta_org:is_member(OrgID1, UserID3),
    true = osc_meta_org:is_member(OrgID2, UserID3),
    true = osc_meta_org:is_member(OrgID1, UserID4),
    false = osc_meta_org:is_member(OrgID3, UserID4),
    false = osc_meta_org:is_member(OrgID3, UserID5),

    {ok, TeamID1} = osc_meta_team:create(OrgID1, <<"team1">>),
    {ok, TeamID2} = osc_meta_team:create(OrgID1, <<"team2">>),
    {ok, TeamID3} = osc_meta_team:create(OrgID2, <<"team1">>),

    ok = osc_meta_team:add_member(OrgID1, TeamID1, UserID1),
    ok = osc_meta_team:add_member(OrgID1, TeamID2, UserID1),
    ok = osc_meta_team:add_member(OrgID2, TeamID3, UserID1),
    ok = osc_meta_team:add_member(OrgID1, TeamID1, UserID2),
    ok = osc_meta_team:add_member(OrgID1, TeamID2, UserID2),
    ok = osc_meta_team:add_member(OrgID1, TeamID3, UserID2),
    ok = osc_meta_team:add_member(OrgID1, TeamID1, UserID3),
    ok = osc_meta_team:add_member(OrgID1, TeamID1, UserID4),

    true = osc_meta_team:is_member(OrgID1, TeamID1, UserID1),
    true = osc_meta_team:is_member(OrgID1, TeamID2, UserID1),
    true = osc_meta_team:is_member(OrgID2, TeamID3, UserID1),
    true = osc_meta_team:is_member(OrgID1, TeamID1, UserID2),
    true = osc_meta_team:is_member(OrgID1, TeamID2, UserID2),
    true = osc_meta_team:is_member(OrgID1, TeamID3, UserID2),
    true = osc_meta_team:is_member(OrgID1, TeamID1, UserID3),
    true = osc_meta_team:is_member(OrgID1, TeamID1, UserID4),

    false = osc_meta_team:is_member(OrgID1, TeamID1, UserID5),
    false = osc_meta_team:is_member(OrgID1, TeamID2, UserID5),
    false = osc_meta_team:is_member(OrgID2, TeamID3, UserID5),
    false = osc_meta_team:is_member(OrgID2, TeamID3, UserID3),

    ok = osc_meta_team:remove_member(OrgID1, TeamID1, UserID4),
    false = osc_meta_team:is_member(OrgID1, TeamID1, UserID4),
    ok = osc_meta_team:add_member(OrgID1, TeamID1, UserID4),
    true = osc_meta_team:is_member(OrgID1, TeamID1, UserID4),

    ok = osc_meta_org:remove_member(OrgID1, UserID4),
    false = osc_meta_team:is_member(OrgID1, TeamID1, UserID4),
    false = osc_meta_org:is_member(OrgID1, UserID4),

    {ok, Props} = osc_meta_org:lookup(OrgID1),
    {id, OrgID1} = lists:keyfind(id, 1, Props),

    ok = osc_meta_org:delete(OrgID1),
    not_found = osc_meta_org:lookup(OrgID1),
    ok.

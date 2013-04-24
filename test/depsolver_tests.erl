%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sx=4 et
%%
%%-------------------------------------------------------------------
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @author Eric Merritt <ericbmerritt@gmail.com>
%%-------------------------------------------------------------------
-module(depsolver_tests).

-include_lib("eunit/include/eunit.hrl").

%%============================================================================
%% Tests
%%============================================================================

first_test() ->
    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1", [{app2, "0.2+build.33"},
                                                                           {app3, "0.2", '>='}]},
                                                                  {"0.2", []},
                                                                  {"0.3", []}]},
                                                          {app2, [{"0.1", []},
                                                                  {"0.2+build.33",[{app3, "0.3"}]},
                                                                  {"0.3", []}]},
                                                          {app3, [{"0.1", []},
                                                                  {"0.2", []},
                                                                  {"0.3", []}]}]),


    case depsolver:solve(Dom0, [{app1, "0.1"}]) of
        {ok,[{app3,{{0,3},{[],[]}}},
             {app2,{{0,2},{[],[<<"build">>,33]}}},
             {app1,{{0,1},{[],[]}}}]} ->
            ok;
        E ->
            erlang:throw({invalid_result, E})
    end.

second_test() ->

    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1", [{app2, "0.1", '>='},
                                                       {app4, "0.2"},
                                                       {app3, "0.2", '>='}]},
                                              {"0.2", []},
                                              {"0.3", []}]},
                                      {app2, [{"0.1", [{app3, "0.2", gte}]},
                                              {"0.2", [{app3, "0.2", gte}]},
                                              {"0.3", [{app3, "0.2", '>='}]}]},
                                      {app3, [{"0.1", [{app4, "0.2", '>='}]},
                                              {"0.2", [{app4, "0.2"}]},
                                              {"0.3", []}]},
                                      {app4, [{"0.1", []},
                                              {"0.2", [{app2, "0.2", gte},
                                                       {app3, "0.3"}]},
                                              {"0.3", []}]}]),

    X = depsolver:solve(Dom0, [{app1, "0.1"},
                     {app2, "0.3"}]),

    ?assertMatch({ok, [{app3,{{0,3},{[],[]}}},
                       {app2,{{0,3},{[],[]}}},
                       {app4,{{0,2},{[],[]}}},
                       {app1,{{0,1},{[],[]}}}]},
                 X).

third_test() ->

    Pkg1Deps = [{app2, "0.1.0", '>='},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", Pkg1Deps},
                                              {"0.2", Pkg1Deps},
                                              {"3.0", Pkg1Deps}]},
                                      {app2, [{"0.0.1", Pkg2Deps},
                                              {"0.1", Pkg2Deps},
                                              {"1.0", Pkg2Deps},
                                              {"3.0", Pkg2Deps}]},
                                      {app3, [{"0.1.0", Pkg3Deps},
                                              {"0.1.3", Pkg3Deps},
                                              {"2.0.0", Pkg3Deps},
                                              {"3.0.0", Pkg3Deps},
                                              {"4.0.0", Pkg3Deps}]},
                                      {app4, [{"0.1.0", Pkg4Deps},
                                              {"0.3.0", Pkg4Deps},
                                              {"5.0.0", Pkg4Deps},
                                              {"6.0.0", Pkg4Deps}]},
                                      {app5, [{"0.1.0", []},
                                              {"0.3.0", []},
                                              {"2.0.0", []},
                                              {"6.0.0", []}]}]),

    ?assertMatch({ok, [{app5,{{6,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{6,0,0},{[],[]}}},
                       {app2,{{3,0},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])),


    ?assertMatch({ok, [{app5,{{6,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{6,0,0},{[],[]}}},
                       {app2,{{3,0},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [app1])).

fail_test() ->
    Dom0 = depsolver:add_packages(depsolver:new_graph(),
                                  [{app1, [{"0.1", [{app2, "0.2"},
                                                    {app3, "0.2", gte}]},
                                           {"0.2", []},
                                           {"0.3", []}]},
                                   {app2, [{"0.1", []},
                                           {"0.2",[{app3, "0.1"}]},
                                           {"0.3", []}]},
                                   {app3, [{"0.1", []},
                                           {"0.2", []},
                                           {"0.3", []}]}]),

    Ret = depsolver:solve(Dom0, [{app1, "0.1"}]),
    %% We do this to make sure all errors can be formated.
    _ = depsolver:format_error(Ret),
    ?assertMatch({error,
                  [{[{[{app1,{{0,1},{[],[]}}}],
                      [{app1,{{0,1},{[],[]}}},[[{app2,{{0,2},{[],[]}}}]]]}],
                    [{{app2,{{0,2},{[],[]}}},[{app3,{{0,1},{[],[]}}}]},
                     {{app1,{{0,1},{[],[]}}},[{app3,{{0,2},{[],[]}},gte}]}]}]},
                 Ret).

conflicting_passing_test() ->
    Pkg1Deps = [{app2, "0.1.0", '>='},
                {app5, "2.0.0"},
                {app4, "0.3.0", "5.0.0", between},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "3.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],

    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", Pkg1Deps},
                                              {"0.1.0", Pkg1Deps},
                                              {"0.2", Pkg1Deps},
                                              {"3.0", Pkg1Deps}]},
                                      {app2, [{"0.0.1", Pkg2Deps},
                                              {"0.1", Pkg2Deps},
                                              {"1.0", Pkg2Deps},
                                              {"3.0", Pkg2Deps}]},
                                      {app3, [{"0.1.0", Pkg3Deps},
                                              {"0.1.3", Pkg3Deps},
                                              {"2.0.0", Pkg3Deps},
                                              {"3.0.0", Pkg3Deps},
                                              {"4.0.0", Pkg3Deps}]},
                                      {app4, [{"0.1.0", [{app5, "0.1.0"}]},
                                              {"0.3.0", [{app5, "0.3.0"}]},
                                              {"5.0.0", [{app5, "2.0.0"}]},
                                              {"6.0.0", [{app5, "6.0.0"}]}]},
                                      {app5, [{"0.1.0", []},
                                              {"0.3.0", []},
                                              {"2.0.0", []},
                                              {"6.0.0", []}]}]),

    ?assertMatch({ok, [{app5,{{2,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{5,0,0},{[],[]}}},
                       {app2,{{3,0},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])),

    ?assertMatch({ok, [{app5,{{2,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{5,0,0},{[],[]}}},
                       {app2,{{3,0},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [app1, app2, app5])).



circular_dependencies_test() ->
    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", [app2]}]},
                                      {app2, [{"0.0.1", [app1]}]}]),

    ?assertMatch({ok, [{app1,{{0,1,0},{[],[]}}},{app2,{{0,0,1},{[],[]}}}]},
                 depsolver:solve(Dom0, [{app1, "0.1.0"}])).

conflicting_failing_test() ->
    Pkg1Deps = [app2,
                {app5, "2.0.0", '='},
                {app4, "0.3.0", "5.0.0", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "6.0.0"}],


    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"3.0", Pkg1Deps}]},
                                      {app2, [{"0.0.1", Pkg2Deps}]},
                                      {app3, [{"0.1.0", Pkg3Deps}]},
                                      {app4, [{"5.0.0", [{app5, "2.0.0"}]}]},
                                      {app5, [{"2.0.0", []},
                                              {"6.0.0", []}]}]),
    Ret = depsolver:solve(Dom0, [app1, app3]),
    _ = depsolver:format_error(Ret),
    ?assertMatch({error,
                   [{[{[app1],
                       [{app1,{{3,0},{[],[]}}},
                        [[{app4,{{5,0,0},{[],[]}}}],
                         [{app2,{{0,0,1},{[],[]}}},[[{app4,{{5,0,0},{[],[]}}}]]]]]},
                      {[app3],
                       [{app3,{{0,1,0},{[],[]}}},[[{app5,{{6,0,0},{[],[]}}}]]]}],
                     [{{app4,{{5,0,0},{[],[]}}},[{app5,{{2,0,0},{[],[]}}}]},
                      {{app1,{{3,0},{[],[]}}},[{app5,{{2,0,0},{[],[]}},'='}]}]}]},
                 Ret).


pessimistic_major_minor_patch_test() ->

    Pkg1Deps = [{app2, "2.1.1", '~>'},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", Pkg1Deps},
                                              {"0.2", Pkg1Deps},
                                              {"3.0", Pkg1Deps}]},
                                      {app2, [{"0.0.1", Pkg2Deps},
                                              {"0.1", Pkg2Deps},
                                              {"1.0", Pkg2Deps},
                                              {"2.1.5", Pkg2Deps},
                                              {"2.2", Pkg2Deps},
                                              {"3.0", Pkg2Deps}]},
                                      {app3, [{"0.1.0", Pkg3Deps},
                                              {"0.1.3", Pkg3Deps},
                                              {"2.0.0", Pkg3Deps},
                                              {"3.0.0", Pkg3Deps},
                                              {"4.0.0", Pkg3Deps}]},
                                      {app4, [{"0.1.0", Pkg4Deps},
                                              {"0.3.0", Pkg4Deps},
                                              {"5.0.0", Pkg4Deps},
                                              {"6.0.0", Pkg4Deps}]},
                                      {app5, [{"0.1.0", []},
                                              {"0.3.0", []},
                                              {"2.0.0", []},
                                              {"6.0.0", []}]}]),
    ?assertMatch({ok, [{app5,{{6,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{6,0,0},{[],[]}}},
                       {app2,{{2,1,5},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])).

pessimistic_major_minor_test() ->

    Pkg1Deps = [{app2, "2.1", '~>'},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

     Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", Pkg1Deps},
                                              {"0.2", Pkg1Deps},
                                              {"3.0", Pkg1Deps}]},
                                      {app2, [{"0.0.1", Pkg2Deps},
                                              {"0.1", Pkg2Deps},
                                              {"1.0", Pkg2Deps},
                                              {"2.1.5", Pkg2Deps},
                                              {"2.2", Pkg2Deps},
                                              {"3.0", Pkg2Deps}]},
                                      {app3, [{"0.1.0", Pkg3Deps},
                                              {"0.1.3", Pkg3Deps},
                                              {"2.0.0", Pkg3Deps},
                                              {"3.0.0", Pkg3Deps},
                                              {"4.0.0", Pkg3Deps}]},
                                      {app4, [{"0.1.0", Pkg4Deps},
                                              {"0.3.0", Pkg4Deps},
                                              {"5.0.0", Pkg4Deps},
                                              {"6.0.0", Pkg4Deps}]},
                                      {app5, [{"0.1.0", []},
                                              {"0.3.0", []},
                                              {"2.0.0", []},
                                              {"6.0.0", []}]}]),
    ?assertMatch({ok, [{app5,{{6,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{6,0,0},{[],[]}}},
                       {app2,{{2,2},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])).

filter_packages_with_deps_test() ->
    Packages = [{app1, [{"0.1", [{app2, "0.2"},
                                 {app3, "0.2", '>='},
                                 {app4, "0.2", '='}]},
                        {"0.2", [{app4, "0.2"}]},
                        {"0.3", [{app4, "0.2", '='}]}]},
                {app2, [{"0.1", []},
                        {"0.2",[{app3, "0.3"}]},
                        {"0.3", []}]},
                {app3, [{"0.1", []},
                        {"0.2", []},
                        {"0.3", []}]}],

    %% constrain app1 and app3
    Cons = [{app1, "0.1", '='},
            {app3, "0.1", '>'}],
    ?assertEqual({ok, [{app1, [{"0.1", [{app2, "0.2"},
                                        {app3, "0.2", '>='},
                                        {app4, "0.2", '='}]}]},
                       {app2, [{"0.1", []},
                               {"0.2",[{app3, "0.3"}]},
                               {"0.3", []}]},
                       {app3, [{"0.2", []},
                               {"0.3", []}]}]},
                 depsolver:filter_packages_with_deps(Packages, Cons)),
    %% a constraint that doesn't matter
    ?assertEqual({ok, Packages},
                 depsolver:filter_packages_with_deps(Packages, [{appX, "4.0"}])),
    %% no constraints
    ?assertEqual({ok, Packages},
                 depsolver:filter_packages_with_deps(Packages, [])),

    %% remove everything constraints
    ?assertEqual({ok, []},
                 depsolver:filter_packages_with_deps(Packages,
                                                     [{app1, "40.0"},
                                                      {app2, "40.0"},
                                                      {app3, "40.0"}])),

    Ret = depsolver:filter_packages_with_deps(Packages, [{<<"ick">>, "1.0.0", '~~~~'}]),
    Expect = {error, {invalid_constraints, [{<<"ick">>, {{1, 0, 0}, {[], []}}, '~~~~'}]}},
    ?assertEqual(Expect, Ret).

filter_versions_test() ->

    Cons = [{app2, "2.1", '~>'},
            {app3, "0.1.1", "0.1.5", between},
            {app4, "5.0.0", gte},
            {app5, "2.0.0", '>='},
            app5],

    Packages = [{app1, "0.1.0"},
                {app1, "0.2"},
                {app1, "0.2"},
                {app1, "3.0"},
                {app2, "0.0.1"},
                {app2, "0.1"},
                {app2, "1.0"},
                {app2, "2.1.5"},
                {app2, "2.2"},
                {app2, "3.0"},
                {app3, "0.1.0"},
                {app3, "0.1.3"},
                {app3, "2.0.0"},
                {app3, "3.0.0"},
                {app3, "4.0.0"},
                {app4, "0.1.0"},
                {app4, "0.3.0"},
                {app4, "5.0.0"},
                {app4, "6.0.0"},
                {app5, "0.1.0"},
                {app5, "0.3.0"},
                {app5, "2.0.0"},
                {app5, "6.0.0"}],

    ?assertMatch({ok, [{app1,"0.1.0"},
                       {app1,"0.2"},
                       {app1,"0.2"},
                       {app1,"3.0"},
                       {app2,"2.1.5"},
                       {app2,"2.2"},
                       {app3,"0.1.3"},
                       {app4,"5.0.0"},
                       {app4,"6.0.0"},
                       {app5,"2.0.0"},
                       {app5,"6.0.0"}]},
                 depsolver:filter_packages(Packages, Cons)),

    Ret = depsolver:filter_packages(Packages,
                                    [{"foo", "1.0.0", '~~~~'} | Cons]),
    _ = depsolver:format_error(Ret),
    ?assertMatch({error, {invalid_constraints, [{<<"foo">>,{{1,0,0},{[],[]}},'~~~~'}]}}, Ret).


-spec missing_test() -> ok.
missing_test() ->

    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1", [{app2, "0.2"},
                                                             {app3, "0.2", '>='},
                                                             {app4, "0.2", '='}]},
                                                                  {"0.2", [{app4, "0.2"}]},
                                                                  {"0.3", [{app4, "0.2", '='}]}]},
                                                          {app2, [{"0.1", []},
                                                                  {"0.2",[{app3, "0.3"}]},
                                                                  {"0.3", []}]},
                                                          {app3, [{"0.1", []},
                                                                  {"0.2", []},
                                                                  {"0.3", []}]}]),
    Ret1 = depsolver:solve(Dom0, [{app4, "0.1"}, {app3, "0.1"}]),
    _ = depsolver:format_error(Ret1),
    ?assertMatch({error,{unreachable_package,app4}}, Ret1),

    Ret2 = depsolver:solve(Dom0, [{app1, "0.1"}]),
    _ = depsolver:format_error(Ret2),
    ?assertMatch({error,{unreachable_package,app4}},
                 Ret2).


binary_test() ->

    World = [{<<"foo">>, [{<<"1.2.3">>, [{<<"bar">>, <<"2.0.0">>, gt}]}]},
             {<<"bar">>, [{<<"2.0.0">>, [{<<"foo">>, <<"3.0.0">>, gt}]}]}],
    Ret = depsolver:solve(depsolver:add_packages(depsolver:new_graph(),
                                               World),
                        [<<"foo">>]),

    _ = depsolver:format_error(Ret),
    ?assertMatch({error,
                  [{[{[<<"foo">>],[{<<"foo">>,{{1,2,3},{[],[]}}}]}],
                    [{{<<"foo">>,{{1,2,3},{[],[]}}},
                      [{<<"bar">>,{{2,0,0},{[],[]}},gt}]}]}]}, Ret).

%%
%% We don't have bar cookbook
%%
%% Ruby gives
%% "message":"Unable to satisfy constraints on cookbook bar, which does not
%%   exist, due to run list item (foo >= 0.0.0).  Run list items that may result
%%   in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]",
%%   "unsatisfiable_run_list_item":"(foo >= 0.0.0)",
%% "non_existent_cookbooks":["bar"],"
%% "most_constrained_cookbooks":[]}"
%%
doesnt_exist_test() ->
    Constraints = [{<<"foo">>,[{<<"1.2.3">>, [{<<"bar">>, <<"2.0.0">>, gt}]}]}],
    World = depsolver:add_packages(depsolver:new_graph(), Constraints),
    Ret = depsolver:solve(World, [<<"foo">>]),
    _ = depsolver:format_error(Ret),
    ?assertMatch({error,{unreachable_package,<<"bar">>}}, Ret).

%%
%% We have v 2.0.0 of bar but want > 2.0.0
%%
%% Ruby gives
%% "message":"Unable to satisfy constraints on cookbook bar due to run list item
%% (foo >= 0.0.0).  Run list items that may result in a constraint on bar: [(foo
%% = 1.2.3) -> (bar > 2.0.0)]",
%% "unsatisfiable_run_list_item":"(foo >= 0.0.0)",
%% "non_existent_cookbooks":[],
%% "most_constrained_cookbooks":["bar 2.0.0 -> []"]
%%
not_new_enough_test() ->

    Constraints = [{<<"foo">>, [{<<"1.2.3">>, [{<<"bar">>, <<"2.0.0">>, gt}]}]},
                   {<<"bar">>, [{<<"2.0.0">>, []}]}],
    World = depsolver:add_packages(depsolver:new_graph(), Constraints),
    Ret = depsolver:solve(World, [<<"foo">>]),
    _ = depsolver:format_error(Ret),
    ?assertMatch({error,
                  [{[{[<<"foo">>],[{<<"foo">>,{{1,2,3},{[],[]}}}]}],
                    [{{<<"foo">>,{{1,2,3},{[],[]}}},
                      [{<<"bar">>,{{2,0,0},{[],[]}},gt}]}]}]}, Ret).

%%
%% circular deps are bad
%%
%% Ruby gives
%% "message":"Unable to satisfy constraints on cookbook bar due to run list item (foo >= 0.0.0).
%%            Run list items that may result in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]",
%% "unsatisfiable_run_list_item":"(foo >= 0.0.0)",
%% "non_existent_cookbooks":[],
%% "most_constrained_cookbooks:["bar = 2.0.0 -> [(foo > 3.0.0)]"]
%%
impossible_dependency_test() ->
    World = depsolver:add_packages(depsolver:new_graph(),
                                   [{<<"foo">>, [{<<"1.2.3">>,[{ <<"bar">>, <<"2.0.0">>, gt}]}]},
                                    {<<"bar">>, [{<<"2.0.0">>, [{ <<"foo">>, <<"3.0.0">>, gt}]}]}]),
    Ret = depsolver:solve(World, [<<"foo">>]),
    _ = depsolver:format_error(Ret),
    ?assertMatch({error,
                  [{[{[<<"foo">>],[{<<"foo">>,{{1,2,3},{[],[]}}}]}],
                    [{{<<"foo">>,{{1,2,3},{[],[]}}},
                      [{<<"bar">>,{{2,0,0},{[],[]}},gt}]}]}]}, Ret).

%%
%% Formatting tests
%%
format_test_() ->
      [{"format_version returns iolist",
        [?_assertEqual(["1", [], []], depsolver:format_version({1, {[],[]}})),
         ?_assertEqual(["1", ".", "2", ".", "34", [], []], depsolver:format_version({{1,2,34},{[],[]}}))
        ]
       },
       {"format_version",
        [equal_bin_string(<<"1">>, depsolver:format_version({1, {[],[]}})),
         equal_bin_string(<<"1.2">>, depsolver:format_version({{1,2}, {[],[]}})),
         equal_bin_string(<<"1.2.2">>, depsolver:format_version({{1,2,2}, {[],[]}})),
         equal_bin_string(<<"1.99.2">>, depsolver:format_version({{1,99,2}, {[],[]}})),
         equal_bin_string(<<"1.99.2-alpha">>, depsolver:format_version({{1,99,2}, {["alpha"],[]}})),
         equal_bin_string(<<"1.99.2-alpha.1">>, depsolver:format_version({{1,99,2}, {["alpha",1], []}})),
         equal_bin_string(<<"1.99.2+build.1.a36">>,
                          depsolver:format_version({{1,99,2}, {[], ["build", 1, "a36"]}})),
         equal_bin_string(<<"1.99.2-alpha.1+build.1.a36">>,
                          depsolver:format_version({{1,99,2}, {["alpha", 1], ["build", 1, "a36"]}})),
         equal_bin_string(<<"1">>, depsolver:format_version({1, {[],[]}}))]
       },
       {"format constraint",
        [equal_bin_string(<<"foo">>, depsolver:format_constraint(<<"foo">>)),
         equal_bin_string(<<"foo">>, depsolver:format_constraint(foo)),
         equal_bin_string(<<"(foo = 1.2.0)">>, depsolver:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}})),
         equal_bin_string(<<"(foo = 1.2.0)">>, depsolver:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}, '='})),
         equal_bin_string(<<"(foo > 1.2.0)">>,
                          depsolver:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}, '>'})),
         equal_bin_string(<<"(foo > 1.2.0)">>,
                          depsolver:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}, gt})),
         equal_bin_string(<<"(foo between 1.2.0 and 1.3.0)">>,
                          depsolver:format_constraint({<<"foo">>,{{1,2,0}, {[], []}},
                                                       {{1,3,0}, {[], []}}, between})),
         equal_bin_string(<<"(foo > 1.2.0-alpha.1+build.36)">>,
                          depsolver:format_constraint({<<"foo">>,
                                                       {{1,2,0}, {["alpha", 1], ["build", 36]}}, gt}))
        ]
       },
       {"format roots",
        [equal_bin_string(<<"(bar = 1.2.0)">>,
                          depsolver:format_roots([ [{<<"bar">>, {{1,2,0},{[],[]}}}] ])),
         equal_bin_string(<<"(bar = 1.2.0), foo">>,
                          depsolver:format_roots([[<<"foo">>,
                                                   {<<"bar">>, {{1,2,0},{[],[]}}}]])),
         equal_bin_string(<<"(bar = 1.2.0), foo">>,
                          depsolver:format_roots([[<<"foo">>], [{<<"bar">>, {{1,2,0},{[],[]}}}]]))
        ]
       }
      ].

%%
%% Internal functions
%%
equal_bin_string(Expected, Got) ->
  ?_assertEqual(Expected, erlang:iolist_to_binary(Got)).

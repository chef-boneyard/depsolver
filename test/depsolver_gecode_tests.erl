%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
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
-module(depsolver_gecode_tests).

-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
%%===========================================================================
%% Tests
%%============================================================================
all_test_() ->
    {foreach,
     fun() ->
             error_logger:delete_report_handler(error_logger_tty_h),
             application:start(depsolver)
     end,
     fun(_) -> application:stop(depsolver) end,
     [
      {?MODULE, first},
      {?MODULE, second},
      {?MODULE, third},
      {?MODULE, fail},
      {?MODULE, conflicting_passing},
      {?MODULE, circular_dependencies},
      {?MODULE, conflicting_failing},
      {?MODULE, pessimistic_major_minor_patch},
      {?MODULE, pessimistic_major_minor},
      {?MODULE, binary},
      {?MODULE, doesnt_exist},
      {?MODULE, not_new_enough},
      {?MODULE, impossible_dependency},
      {?MODULE, missing_via_culprit_search}
      %% {generator, ?MODULE, format},
      %% {generator, ?MODULE, missing2}
     ]
    }.

first() ->
    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
                                         [{app1, [{"0.1", [{app2, "0.2+build.33"},
                                                           {app3, "0.2", '>='}]},
                                                  {"0.2", []},
                                                  {"0.3", []}]},
                                          {app2, [{"0.1", []},
                                                  {"0.2+build.33",[{app3, "0.3"}]},
                                                  {"0.3", []}]},
                                          {app3, [{"0.1", []},
                                                  {"0.2", []},
                                                  {"0.3", []}]}]),

    Result = norm({ok,[
                       {app1,{{0,1},{[],[]}}},
                       {app2,{{0,2},{[],[]}}},
                       {app3,{{0,3},{[],[]}}} ] } ) ,

    case norm(depsolver_gecode:solve(Dom0, [{app1, "0.1"}])) of
        Result ->
            ok;
        E ->
            erlang:throw({invalid_result, E})
    end.

second() ->

    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
                                         [{app1, [{"0.1", [{app2, "0.1", '>='},
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

    X = norm(depsolver_gecode:solve(Dom0, [{app1, "0.1"}, {app2, "0.3"}])),

    ?assertEqual(norm({ok, [{app3,{{0,3},{[],[]}}},
                            {app2,{{0,3},{[],[]}}},
                            {app4,{{0,2},{[],[]}}},
                            {app1,{{0,1},{[],[]}}}]}),
                 X).

third() ->

    Pkg1Deps = [{app2, "0.1.0", '>='},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
                                         [{app1, [{"0.1.0", Pkg1Deps},
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

    ExpRes = norm({ok, [{app5,{{6,0,0},{[],[]}}},
                        {app3,{{0,1,3},{[],[]}}},
                        {app4,{{6,0,0},{[],[]}}},
                        {app2,{{3,0},{[],[]}}},
                        {app1,{{3,0},{[],[]}}}]}),

    Result = norm(depsolver_gecode:solve(Dom0, [{app1, "3.0"}])),
    ?assertEqual(ExpRes, Result),

    ExpRes2 = norm({ok, [{app5,{{6,0,0},{[],[]}}},
                         {app3,{{0,1,3},{[],[]}}},
                         {app4,{{6,0,0},{[],[]}}},
                         {app2,{{3,0},{[],[]}}},
                         {app1,{{3,0},{[],[]}}}]}),

    Result2 =depsolver_gecode:solve(Dom0, [app1]),

    ?assertEqual(ExpRes2, Result2).

fail() ->
    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
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

    Ret = depsolver_gecode:solve(Dom0, [{app1, "0.1"}]),
    %% We do this to make sure all errors can be formated.
    _ = depsolver_gecode:format_error(Ret),

    Expected = {error,{no_solution,[{app1,"0.1"}],[{app1,{0,3}}]}},
    ?assertEqual(Expected, Ret).

conflicting_passing() ->
    Pkg1Deps = [{app2, "0.1.0", '>='},
                {app5, "2.0.0"},
                {app4, "0.3.0", "5.0.0", between},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "3.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],

    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
                                         [{app1, [{"0.1.0", Pkg1Deps},
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

    Expected = norm({ok, [{app5,{{2,0,0},{[],[]}}},
                          {app3,{{0,1,3},{[],[]}}},
                          {app4,{{5,0,0},{[],[]}}},
                          {app2,{{3,0},{[],[]}}},
                          {app1,{{3,0},{[],[]}}}]}),

    Result = depsolver_gecode:solve(Dom0, [{app1, "3.0"}]),

    ?assertEqual(Expected, Result),


    Expected2 = norm({ok, [{app5,{{2,0,0},{[],[]}}},
                           {app3,{{0,1,3},{[],[]}}},
                           {app4,{{5,0,0},{[],[]}}},
                           {app2,{{3,0},{[],[]}}},
                           {app1,{{3,0},{[],[]}}}]}),
    Result2 = norm(depsolver_gecode:solve(Dom0, [app1, app2, app5])),

    ?assertEqual(Expected2, Result2).

circular_dependencies() ->
    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
                                         [{app1, [{"0.1.0", [app2]}]},
                                          {app2, [{"0.0.1", [app1]}]}]),

    Expected = norm({ok, [{app1,{{0,1,0},{[],[]}}},{app2,{{0,0,1},{[],[]}}}]}),
    Result = norm(depsolver_gecode:solve(Dom0, [{app1, "0.1.0"}])),

    ?assertEqual(Expected, Result).


conflicting_failing() ->
    Pkg1Deps = [app2,
                {app5, "2.0.0", '='},
                {app4, "0.3.0", "5.0.0", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "6.0.0"}],


    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
                                         [{app1, [{"3.0", Pkg1Deps}]},
                                          {app2, [{"0.0.1", Pkg2Deps}]},
                                          {app3, [{"0.1.0", Pkg3Deps}]},
                                          {app4, [{"5.0.0", [{app5, "2.0.0"}]}]},
                                          {app5, [{"2.0.0", []},
                                                  {"6.0.0", []}]}]),
    Ret = depsolver_gecode:solve(Dom0, [app1, app3]),

    _ = depsolver_gecode:format_error(Ret),
    ?assertMatch(Ret,{error,{no_solution,[app1,app3],[{app5,{6,0,0}}]}} ).

pessimistic_major_minor_patch() ->

    Pkg1Deps = [{app2, "2.1.1", '~>'},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
                                         [{app1, [{"0.1.0", Pkg1Deps},
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
    Expected = norm({ok, [{app5,{{6,0,0},{[],[]}}},
                          {app3,{{0,1,3},{[],[]}}},
                          {app4,{{6,0,0},{[],[]}}},
                          {app2,{{2,1,5},{[],[]}}},
                          {app1,{{3,0},{[],[]}}}]}),
    Result = norm(depsolver_gecode:solve(Dom0, [{app1, "3.0"}])),

    ?assertEqual(Expected, Result).


pessimistic_major_minor() ->

    Pkg1Deps = [{app2, "2.1", '~>'},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
                                         [{app1, [{"0.1.0", Pkg1Deps},
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
    Expected = norm({ok, [{app5,{{6,0,0},{[],[]}}},
                          {app3,{{0,1,3},{[],[]}}},
                          {app4,{{6,0,0},{[],[]}}},
                          {app2,{{2,2},{[],[]}}},
                          {app1,{{3,0},{[],[]}}}]}),
    Result = norm(depsolver_gecode:solve(Dom0, [{app1, "3.0"}])),

    ?assertEqual(Expected, Result).

-spec missing() -> ok.
missing() ->
    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
                                         [{app1, [{"0.1", [{app2, "0.2"},
                                                           {app3, "0.2", '>='},
                                                           {app4, "0.2", '='}]},
                                                  {"0.2", [{app4, "0.2"}]},
                                                  {"0.3", [{app4, "0.2", '='}]}]},
                                          {app2, [{"0.1", []},
                                                  {"0.2", [{app3, "0.3"}]},
                                                  {"0.3", []}]},
                                          {app3, [{"0.1", []},
                                                  {"0.2", []},
                                                  {"0.3", []}]}]),

    Ret1 = depsolver_gecode:solve(Dom0, [{app4, "0.1"}, {app3, "0.1"}]),
    _ = depsolver_gecode:format_error(Ret1),
    ?assertMatch({error,{unreachable_package,app4}}, Ret1),

    %% TODO: Re-enable once we have the abort logic for solver sorted.
    Ret2 = Ret1,%    Ret2 = depsolver_gecode:solve(Dom0, [{app1, "0.1"}]),
    %%    _ = depsolver_gecode:format_error(Ret2),
    ?assertMatch({error,_}, Ret2).

missing_via_culprit_search() ->
    World = [{<<"app1">>,[{"1.1.0",[]}]},
             {<<"app2">>,[{"0.0.1",[{<<"app1::oops">>,<<"0.0.0">>,'>='}]} ]} ],
    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(), World),
    Ret1 = depsolver_gecode:solve(Dom0, [<<"app1">>,<<"app2">>]),
    _ = depsolver_gecode:format_error(Ret1),
    ?assertMatch({error,{unreachable_package,<<"app1::oops">>}}, Ret1).

binary() ->

    World = [{<<"foo">>, [{<<"1.2.3">>, [{<<"bar">>, <<"2.0.0">>, gt}]}]},
             {<<"bar">>, [{<<"2.0.0">>, [{<<"foo">>, <<"3.0.0">>, gt}]}]}],

    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(), World),

    Ret = depsolver_gecode:solve(Dom0, [<<"foo">>]),

    %% Old solver gave
    %% <<"Unable to solve constraints, the following solutions were attempted \n\n    Unable to satisfy goal constraint foo due to constraint on bar\n        (foo = 1.2.3) -> (bar > 2.0.0)\n">>

    Expected = {error,{no_solution,[<<"foo">>], [{<<"foo">>,unused}]}},

    _ = depsolver_gecode:format_error(Ret),

    ?assertEqual(Expected, Ret).

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
doesnt_exist() ->
    Constraints = [{<<"foo">>,[{<<"1.2.3">>, [{<<"bar">>, <<"2.0.0">>, gt}]}]}],
    World = depsolver_gecode:add_packages(depsolver_gecode:new_graph(), Constraints),
    Ret = depsolver_gecode:solve(World, [<<"foo">>]),
    _ = depsolver_gecode:format_error(Ret),
    ?assertMatch({error,_}, Ret).

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
not_new_enough() ->

    Constraints = [{<<"foo">>, [{<<"1.2.3">>, [{<<"bar">>, <<"2.0.0">>, gt}]}]},
                   {<<"bar">>, [{<<"2.0.0">>, []}]}],
    World = depsolver_gecode:add_packages(depsolver_gecode:new_graph(), Constraints),
    Ret = depsolver_gecode:solve(World, [<<"foo">>]),
    _ = depsolver_gecode:format_error(Ret),
    ?assertMatch({error,{no_solution,[<<"foo">>],[{<<"bar">>,{2,0,0}}]}}, Ret).

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
impossible_dependency() ->
    World = depsolver_gecode:add_packages(depsolver_gecode:new_graph(),
                                          [{<<"foo">>, [{<<"1.2.3">>,[{ <<"bar">>, <<"2.0.0">>, gt}]}]},
                                           {<<"bar">>, [{<<"2.0.0">>, [{ <<"foo">>, <<"3.0.0">>, gt}]}]}]),
    Ret = depsolver_gecode:solve(World, [<<"foo">>]),
    _ = depsolver_gecode:format_error(Ret),
    ?assertMatch({error,{no_solution,[<<"foo">>],[{<<"foo">>,unused}]}}, Ret).

%%
%% Formatting tests
%%
format() ->
    [{"format_version returns iolist",
      [?_assertEqual(["1", [], []], depsolver_gecode:format_version({1, {[],[]}})),
       ?_assertEqual(["1", ".", "2", ".", "34", [], []], depsolver_gecode:format_version({{1,2,34},{[],[]}}))
      ]
     },
     {"format_version",
      [equal_bin_string(<<"1">>, depsolver_gecode:format_version({1, {[],[]}})),
       equal_bin_string(<<"1.2">>, depsolver_gecode:format_version({{1,2}, {[],[]}})),
       equal_bin_string(<<"1.2.2">>, depsolver_gecode:format_version({{1,2,2}, {[],[]}})),
       equal_bin_string(<<"1.99.2">>, depsolver_gecode:format_version({{1,99,2}, {[],[]}})),
       equal_bin_string(<<"1.99.2-alpha">>, depsolver_gecode:format_version({{1,99,2}, {["alpha"],[]}})),
       equal_bin_string(<<"1.99.2-alpha.1">>, depsolver_gecode:format_version({{1,99,2}, {["alpha",1], []}})),
       equal_bin_string(<<"1.99.2+build.1.a36">>,
                        depsolver_gecode:format_version({{1,99,2}, {[], ["build", 1, "a36"]}})),
       equal_bin_string(<<"1.99.2-alpha.1+build.1.a36">>,
                        depsolver_gecode:format_version({{1,99,2}, {["alpha", 1], ["build", 1, "a36"]}})),
       equal_bin_string(<<"1">>, depsolver_gecode:format_version({1, {[],[]}}))]
     },
     {"format constraint",
      [equal_bin_string(<<"foo">>, depsolver_gecode:format_constraint(<<"foo">>)),
       equal_bin_string(<<"foo">>, depsolver_gecode:format_constraint(foo)),
       equal_bin_string(<<"(foo = 1.2.0)">>, depsolver_gecode:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}})),
       equal_bin_string(<<"(foo = 1.2.0)">>, depsolver_gecode:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}, '='})),
       equal_bin_string(<<"(foo > 1.2.0)">>,
                        depsolver_gecode:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}, '>'})),
       equal_bin_string(<<"(foo > 1.2.0)">>,
                        depsolver_gecode:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}, gt})),
       equal_bin_string(<<"(foo between 1.2.0 and 1.3.0)">>,
                        depsolver_gecode:format_constraint({<<"foo">>,{{1,2,0}, {[], []}},
                                                            {{1,3,0}, {[], []}}, between})),
       equal_bin_string(<<"(foo > 1.2.0-alpha.1+build.36)">>,
                        depsolver_gecode:format_constraint({<<"foo">>,
                                                            {{1,2,0}, {["alpha", 1], ["build", 36]}}, gt}))
      ]
     },
     {"format roots",
      [equal_bin_string(<<"(bar = 1.2.0)">>,
                        depsolver_gecode:format_roots([ [{<<"bar">>, {{1,2,0},{[],[]}}}] ])),
       equal_bin_string(<<"(bar = 1.2.0), foo">>,
                        depsolver_gecode:format_roots([[<<"foo">>,
                                                        {<<"bar">>, {{1,2,0},{[],[]}}}]])),
       equal_bin_string(<<"(bar = 1.2.0), foo">>,
                        depsolver_gecode:format_roots([[<<"foo">>], [{<<"bar">>, {{1,2,0},{[],[]}}}]]))
      ]
     }
    ].

missing2() ->
    %% noapp is missing, but referenced.
    Dom0 = depsolver_gecode:add_packages(depsolver_gecode:new_graph(), [{app1, [
                                                                                %% direct dep on noapp
                                                                                {"0.1", [{noapp, "0.1", '>='}]},
                                                                                %% exact dep on app2 which depends on noapp
                                                                                {"0.2", [{app2, "0.1"}]},
                                                                                %% will take any version of app2
                                                                                {"0.3", [{app2, "0.1", '>='}]},
                                                                                {"0.4", [{app2, "0.3", '<='}]}
                                                                               ]},
                                                                        {app2, [{"0.1", [{noapp, "0.1"}]},
                                                                                {"0.2",[]},
                                                                                {"0.3", [{app3, "0.2"}]},
                                                                                {"0.4", []}]},
                                                                        {app3, [{"0.1", []},
                                                                                {"0.2", [{noapp, "0.1"}]},
                                                                                {"0.3", []}]},
                                                                        {app4, [{"0.1", [{app3, "100"}]}]}
                                                                       ]),

    [
     %% should fail because direct dep not found
     ?_assertMatch({error, _},
                   depsolver_gecode:solve(Dom0, [{app1, "0.1"}])),

     %% should fail because dep of dep not found
     ?_assertMatch({error, _},
                   depsolver_gecode:solve(Dom0, [{app1, "0.2"}])),

     %% should fail because dep of dep not found
     ?_assertMatch({error, _},
                   depsolver_gecode:solve(Dom0, [{app1, "0.3"}, {non_existent, "0.0.0"}])),

     %% should fail, pkg exists, but not at version
     ?_assertMatch({error, _},
                   depsolver_gecode:solve(Dom0, [{app4, "0.1"}])),

     %% should end up with app1 0.3, app2 0.4
     ?_assertEqual({ok,[{app2,{{0,4},{[],[]}}},{app1,{{0,3},{[],[]}}}]},
                   depsolver_gecode:solve(Dom0, [{app1, "0.3"}])),

     %% Since latest version of app2 is unreachable due to missing dep, we
     %% expect app1 0.4 with app2 0.2.
     %% Current implementation fails on first missing package.  This is a
     %% compromise solution until depsolver can be enhanced to handle this
     %% better.
     ?_assertEqual({ok,[{app2,{{0,2},{[],[]}}},{app1,{{0,4},{[],[]}}}]},
                                                %?_assertEqual({error, {unreachable_package, noapp}},
                   depsolver_gecode:solve(Dom0, [{app1, "0.4"}])),

     %% should end up with app1 0.3, app2 0.4
     ?_assertEqual({ok,[{app2,{{0,2},{[],[]}}},{app1,{{0,4},{[],[]}}}]},
                                                %?_assertEqual({ok,[{app2,{{0,4},{[],[]}}},{app1,{{0,3},{[],[]}}}]},
                   depsolver_gecode:solve(Dom0, [{app1, "0.4", '<='}]))
    ].
%%
%% Internal functions
%%
equal_bin_string(Expected, Got) ->
    ?_assertEqual(Expected, erlang:iolist_to_binary(Got)).



strip_semver({A, {_, _}}) ->
    A;
strip_semver(A) ->
    A.

norm({R, Constraints}) ->
    {R, lists:sort([ {A, strip_semver(V)} ||  {A, V} <- Constraints ])}.

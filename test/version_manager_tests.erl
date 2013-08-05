%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%%
%%-------------------------------------------------------------------
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
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
%% @author Mark Anderson <mark@opscode.com>
%%-------------------------------------------------------------------
-module(version_manager_tests).

-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

-define(NO_MATCH_CONSTRAINT, {-2,-2}).

-define(VERSION_SET_PARSED, [{0,1,0}, %% This is deliberately out of order
                             {0,2,0},
                             {3,0,0},
                             {0,0,1},
                             {1,0,0},
                             {0,1,3},
                             {2,0,0},
                             {4,0,0},
                             {5,0,0},
                             {6,0,0}] ).


-define(PROBLEM,  [{app1, ["0.1",
                           "0.2",
                           "0.3"]},
                   {app2, ["0.1",
                           "0.2",
                           "0.3"]},
                   {app3, ["0.1",
                           "0.2",
                           "0.3"]} ]).

version_map_test_() ->
    {foreach,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [
      {"Simple init and fetch of versions comes out in order",
       fun() ->
               Versions = version_manager:make(?VERSION_SET_PARSED),
               ?assertEqual({0,0,1}, version_manager:int_to_version(0, Versions)),
               ?assertEqual({0,1,0}, version_manager:int_to_version(1, Versions)),
               ?assertEqual({0,1,3}, version_manager:int_to_version(2, Versions)),
               ?assertEqual({0,2,0}, version_manager:int_to_version(3, Versions)),
               ?assertEqual({1,0,0}, version_manager:int_to_version(4, Versions)),
               ?assertEqual({2,0,0}, version_manager:int_to_version(5, Versions)),
               ?assertEqual({3,0,0}, version_manager:int_to_version(6, Versions)),
               ?assertEqual({4,0,0}, version_manager:int_to_version(7, Versions)),
               ?assertEqual({5,0,0}, version_manager:int_to_version(8, Versions)),
               ?assertEqual({6,0,0}, version_manager:int_to_version(9, Versions))
       end}
     ]}.

search_test() ->
    {foreach,
     fun() ->
             %% Quick hack to extract from record
             element(2,version_manager:make(?VERSION_SET_PARSED))
     end,
     fun(_) ->
             ok
     end,
     [
      fun(A) ->
              [
               {"Equality search works",
                fun() ->
                        ?assertEqual(not_found, version_manager:search_eq({0,0,0}, A)),
                        ?assertEqual(0, version_manager:search_eq({0,0,1}, A)),
                        ?assertEqual(4, version_manager:search_eq({1,0,0}, A)),
                        ?assertEqual(not_found, version_manager:search_eq({1,0,1}, A)),
                        ?assertEqual(9, version_manager:search_eq({6,0,0}, A))
                        ?assertEqual(not_found, version_manager:search_eq({6,0,1}, A))
                end},
               {"less_than search works",
                fun() ->
                        ?assertEqual(not_found, version_manager:search_lt({0,0,0}, A)),
                        ?assertEqual(not_found, version_manager:search_lt({0,0,1}, A)),
                        ?assertEqual(3, version_manager:search_lt({1,0,0}, A)),
                        ?assertEqual(4, version_manager:search_lt({1,0,1}, A)),
                        ?assertEqual(8, version_manager:search_lt({6,0,0}, A))
                        ?assertEqual(9, version_manager:search_lt({6,0,1}, A))
                end},
               {"less_than_equals search works",
                fun() ->
                        ?assertEqual(not_found, version_manager:search_lte({0,0,0}, A)),
                        ?assertEqual(0, version_manager:search_lte({0,0,1}, A)),
                        ?assertEqual(4, version_manager:search_lte({1,0,0}, A)),
                        ?assertEqual(5, version_manager:search_lte({1,0,1}, A)),
                        ?assertEqual(9, version_manager:search_lte({6,0,0}, A))
                        ?assertEqual(9, version_manager:search_lte({6,0,1}, A))
                end},
               {"greater_than_equals search works",
                fun() ->
                        ?assertEqual(0, version_manager:search_gte({0,0,0}, A)),
                        ?assertEqual(0, version_manager:search_gte({0,0,1}, A)),
                        ?assertEqual(4, version_manager:search_gte({1,0,0}, A)),
                        ?assertEqual(5, version_manager:search_gte({1,0,1}, A)),
                        ?assertEqual(9, version_manager:search_gte({6,0,0}, A))
                        ?assertEqual(not_found, version_manager:search_gte({6,0,1}, A))
                end},
               {"greater_than search works",
                fun() ->
                        ?assertEqual(0, version_manager:search_lte({0,0,0}, A)),
                        ?assertEqual(1, version_manager:search_gt({0,0,1}, A)),
                        ?assertEqual(5, version_manager:search_gt({1,0,0}, A)),
                        ?assertEqual(5, version_manager:search_gt({1,0,1}, A)),
                        ?assertEqual(not_found, version_manager:search_gt({6,0,0}, A))
                        ?assertEqual(not_found, version_manager:search_gt({6,0,1}, A))
                end}
              ]
      end]}.

constraint_map_test_() ->
    {foreach,
     fun() ->
             version_manager:make(?VERSION_SET_PARSED)
     end,
     fun(_) ->
             ok
     end,
     [
      fun(V) ->
              [
               {"Equality constraints make sense",
                fun() ->
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({{0,0,0}, eq}, V)),
                        ?assertEqual({0,0}, version_manager:constraint_to_range({{0,0,1}, eq}, V)),
                        ?assertEqual({4,4}, version_manager:constraint_to_range({{1,0,0}, eq}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({{1,0,1}, eq}, V)),
                        ?assertEqual({9,9}, version_manager:constraint_to_range({{6,0,0}, eq}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({{6,0,1}, eq}, V))
                end},
               {"LTE constraints make sense",
                fun() ->
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({{0,0,0}, lte}, V)),
                        ?assertEqual({0,0}, version_manager:constraint_to_range({{0,0,1}, lte}, V)),
                        ?assertEqual({0,4}, version_manager:constraint_to_range({{1,0,0}, lte}, V)),
                        ?assertEqual({0,4}, version_manager:constraint_to_range({{1,0,1}, lte}, V)),
                        ?assertEqual({0,9}, version_manager:constraint_to_range({{6,0,0}, lte}, V)),
                        ?assertEqual({0,9}, version_manager:constraint_to_range({{6,0,1}, lte}, V))
                end},
               {"LT constraints make sense",
                fun() ->
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({{0,0,0}, lt}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({{0,0,1}, lt}, V)),
                        ?assertEqual({0,0}, version_manager:constraint_to_range({{0,1,0}, lt}, V)),
                        ?assertEqual({0,3}, version_manager:constraint_to_range({{1,0,0}, lt}, V)),
                        ?assertEqual({0,4}, version_manager:constraint_to_range({{1,0,1}, lt}, V)),
                        ?assertEqual({0,8}, version_manager:constraint_to_range({{6,0,0}, lt}, V)),
                        ?assertEqual({0,9}, version_manager:constraint_to_range({{6,0,1}, lt}, V))
                end},
              {"GTE constraints make sense",
                fun() ->
                        ?assertEqual({0,9}, version_manager:constraint_to_range({{0,0,0}, gte}, V)),
                        ?assertEqual({0,9}, version_manager:constraint_to_range({{0,0,1}, gte}, V)),
                        ?assertEqual({4,9}, version_manager:constraint_to_range({{1,0,0}, gte}, V)),
                        ?assertEqual({5,9}, version_manager:constraint_to_range({{1,0,1}, gte}, V)),
                        ?assertEqual({9,9}, version_manager:constraint_to_range({{6,0,0}, gte}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({{6,0,1}, gte}, V))
                end},
               {"GT constraints make sense",
                fun() ->
                        ?assertEqual({0,9}, version_manager:constraint_to_range({{0,0,0}, gt}, V)),
                        ?assertEqual({1,9}, version_manager:constraint_to_range({{0,0,1}, gt}, V)),
                        ?assertEqual({5,9}, version_manager:constraint_to_range({{1,0,0}, gt}, V)),
                        ?assertEqual({5,9}, version_manager:constraint_to_range({{1,0,1}, gt}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({{6,0,0}, gt}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({{6,0,1}, gt}, V))
                end},
               {"PES (~>) constraints make sense",
                fun() ->
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({{0,0,0}, pes}, V)),
                        ?assertEqual(lo({0,0,1}, {0,0,1}, V),
                                     version_manager:constraint_to_range({{0,0}, pes}, V)),
                        ?assertEqual(lo({0,0,1}, {0,2,0}, V),
                                     version_manager:constraint_to_range({{0}, pes}, V)),
                        ?assertEqual(lo({0,0,1}, {0,0,1}, V),
                                     version_manager:constraint_to_range({{0,0,1}, pes}, V)),
                        ?assertEqual(lo({0,1,0}, {0,1,3}, V),
                                     version_manager:constraint_to_range({{0,1}, pes}, V)),
                        ?assertEqual(lo({0,2,0}, {0,2,0}, V),
                                     version_manager:constraint_to_range({{0,2,0}, pes}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT,
                                     version_manager:constraint_to_range({{0,2,1}, pes}, V)),
                        ?assertEqual(lo({1,0,0}, {1,0,0}, V),
                                     version_manager:constraint_to_range({{1,0}, pes}, V)),
                        ?assertEqual(lo({1,0,0}, {1,0,0}, V),
                                     version_manager:constraint_to_range({{1,0,0}, pes}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT,
                                     version_manager:constraint_to_range({{1,0,1}, pes}, V)),
                        ?assertEqual(lo({6,0,0}, {6,0,0}, V),
                                     version_manager:constraint_to_range({{6,0,0}, pes}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT,
                                     version_manager:constraint_to_range({{6,0,1}, pes}, V))
                end}
              ]
      end
     ]}.

constraint_map_string_test_() ->
    {foreach,
     fun() ->
             version_manager:make(?VERSION_SET_PARSED)
     end,
     fun(_) ->
             ok
     end,
     [
      fun(V) ->
              [
               {"Equality constraints make sense",
                fun() ->
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({"0.0.0", eq}, V)),
                        ?assertEqual({0,0}, version_manager:constraint_to_range({"0.0.1", eq}, V)),
                        ?assertEqual({4,4}, version_manager:constraint_to_range({"1.0.0", eq}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({"1.0.1", eq}, V)),
                        ?assertEqual({9,9}, version_manager:constraint_to_range({"6.0.0", eq}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({"6.0.1", eq}, V))
                end},
               {"LTE constraints make sense",
                fun() ->
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({"0.0.0", lte}, V)),
                        ?assertEqual({0,0}, version_manager:constraint_to_range({"0.0.1", lte}, V)),
                        ?assertEqual({0,4}, version_manager:constraint_to_range({"1.0.0", lte}, V)),
                        ?assertEqual({0,4}, version_manager:constraint_to_range({"1.0.1", lte}, V)),
                        ?assertEqual({0,9}, version_manager:constraint_to_range({"6.0.0", lte}, V)),
                        ?assertEqual({0,9}, version_manager:constraint_to_range({"6.0.1", lte}, V))
                end},
               {"LT constraints make sense",
                fun() ->
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({"0.0.0", lt}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({"0.0.1", lt}, V)),
                        ?assertEqual({0,0}, version_manager:constraint_to_range({"0.1.0", lt}, V)),
                        ?assertEqual({0,3}, version_manager:constraint_to_range({"1.0.0", lt}, V)),
                        ?assertEqual({0,4}, version_manager:constraint_to_range({"1.0.1", lt}, V)),
                        ?assertEqual({0,8}, version_manager:constraint_to_range({"6.0.0", lt}, V)),
                        ?assertEqual({0,9}, version_manager:constraint_to_range({"6.0.1", lt}, V))
                end},
              {"GTE constraints make sense",
                fun() ->
                        ?assertEqual({0,9}, version_manager:constraint_to_range({"0.0.0", gte}, V)),
                        ?assertEqual({0,9}, version_manager:constraint_to_range({"0.0.1", gte}, V)),
                        ?assertEqual({4,9}, version_manager:constraint_to_range({"1.0.0", gte}, V)),
                        ?assertEqual({5,9}, version_manager:constraint_to_range({"1.0.1", gte}, V)),
                        ?assertEqual({9,9}, version_manager:constraint_to_range({"6.0.0", gte}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({"6.0.1", gte}, V))
                end},
               {"GT constraints make sense",
                fun() ->
                        ?assertEqual({0,9}, version_manager:constraint_to_range({"0.0.0", gt}, V)),
                        ?assertEqual({1,9}, version_manager:constraint_to_range({"0.0.1", gt}, V)),
                        ?assertEqual({5,9}, version_manager:constraint_to_range({"1.0.0", gt}, V)),
                        ?assertEqual({5,9}, version_manager:constraint_to_range({"1.0.1", gt}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({"6.0.0", gt}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({"6.0.1", gt}, V))
                end},
               {"PES (~>) constraints make sense",
                fun() ->
                        ?assertEqual(?NO_MATCH_CONSTRAINT, version_manager:constraint_to_range({"0.0.0", pes}, V)),
                        ?assertEqual(lo("0.0.1", "0.0.1", V),
                                     version_manager:constraint_to_range({"0.0", pes}, V)),
%                        ?assertEqual(lo("0.0.1", "0.2.0", V),
%                                     version_manager:constraint_to_range({"0", pes}, V)),
                        ?assertEqual(lo("0.0.1", "0.0.1", V),
                                     version_manager:constraint_to_range({"0.0.1", pes}, V)),
                        ?assertEqual(lo("0.1.0", "0.1.3", V),
                                     version_manager:constraint_to_range({"0.1", pes}, V)),
                        ?assertEqual(lo("0.2.0", "0.2.0", V),
                                     version_manager:constraint_to_range({"0.2.0", pes}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT,
                                     version_manager:constraint_to_range({"0.2.1", pes}, V)),
                        ?assertEqual(lo("1.0.0", "1.0.0", V),
                                     version_manager:constraint_to_range({"1.0", pes}, V)),
                        ?assertEqual(lo("1.0.0", "1.0.0", V),
                                     version_manager:constraint_to_range({"1.0.0", pes}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT,
                                     version_manager:constraint_to_range({"1.0.1", pes}, V)),
                        ?assertEqual(lo("6.0.0", "6.0.0", V),
                                     version_manager:constraint_to_range({"6.0.0", pes}, V)),
                        ?assertEqual(?NO_MATCH_CONSTRAINT,
                                     version_manager:constraint_to_range({"6.0.1", pes}, V))
                end}
              ]
      end
     ]}.


version_manager_make() ->
    version_manager:make(?VERSION_SET_PARSED).

lo(V,A) when element(1,A) =:= cookbook_version_mapper ->
    version_manager:search_eq(V,element(2,A));
lo(V,A) ->
    version_manager:search_eq(V,A).

lo(V1,V2,A) ->
    {lo(V1,A), lo(V2, A)}.

problem_map_test_() ->
    {foreach,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [
      {"Simple init and fetch of apps comes out in order",
       fun() ->
               Problem = make_problem(),
               ?assertEqual(0, version_manager:get_id(app1,Problem)),
               ?assertEqual(1, version_manager:get_id(app2,Problem)),
               ?assertEqual(2, version_manager:get_id(app3,Problem))
       end},
      {"Simple init and fetch of apps, versions comes correct",
       fun() ->
               Problem = make_problem(),
               ?assertEqual({0,0}, version_manager:get_version_id(app1, "0.1",Problem)),
               ?assertEqual({1,0}, version_manager:get_version_id(app2, "0.1",Problem)),
               ?assertEqual({2,1}, version_manager:get_version_id(app3, "0.2",Problem)),
               ?assertEqual({2,2}, version_manager:get_version_id(app3, "0.3",Problem))
       end},
      {"Simple map constraint",
       fun() ->
               Problem = make_problem(),
               ?assertEqual({0, {0,0}}, version_manager:map_constraint(app1, {"0.1", eq}, Problem)),
               ?assertEqual({1, {0,2}}, version_manager:map_constraint(app2, {"0.1", gte} ,Problem)),
               ?assertEqual({2, {0,1}}, version_manager:map_constraint(app3, {"0.2", lte}, Problem))
%% TODO FIXME ?assertEqual({2, {0,2}}, version_manager:map_constraint(app3, {"0", pes} ,Problem))
       end}

     ]}.

make_problem() ->
    Problem = version_manager:new(),
    lists:foldl(fun({N,V},A) -> version_manager:add_package(N,V,A) end,
                           Problem, ?PROBLEM).

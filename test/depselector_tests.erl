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
%% @author Marc Paradise <marc@opscode.com>
%%-------------------------------------------------------------------
-module(depselector_tests).

-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
%%===========================================================================
%% Tests
%%============================================================================

% Each of these tests assumes a fresh solver state and
% that the process is available for use.
clean_test_() ->
    {foreach,
     fun() ->
             error_logger:delete_report_handler(error_logger_tty_h),
             application:start(depsolver)
     end,
     fun(_) -> application:stop(depsolver) end,
     [
       {?MODULE, basic_solve},
       {?MODULE, add_dep},
       {?MODULE, abort_in_problem},
       {?MODULE, too_many_packages},
       {?MODULE, not_enough_packages},
       {?MODULE, error_recovery}
     ]
    }.

basic_solve() ->
    depselector:new_problem("TEST", 1),
    add_packages(1),
    depselector:mark_package_required(0),
    ?assertMatch({solution,{{state,valid},{disabled,0},{packages,[{0,0,0}]}}},
                  depselector:solve()).
add_dep() ->
    depselector:new_problem("TEST", 1),
    add_packages(1),
    % This shouldn't cause error or failure
    depselector:add_version_constraint(0, 0, 0, 0, 0),
    depselector:mark_package_required(0),
    ?assertMatch({solution,{{state,valid},{disabled,0},{packages,[{0,0,0}]}}},
                  depselector:solve()).

abort_in_problem() ->
    depselector:new_problem("TEST", 1),
    depselector:abort(),
    ?assertEqual(depselector:is_ready(), true).


too_many_packages() ->
    depselector:new_problem("TEST", 1),
    add_packages(2),
    ?assertMatch({error, "already added max packages"},
                  depselector:solve()),
    ?assertEqual(depselector:is_ready(), true).

not_enough_packages() ->
    depselector:new_problem("TEST", 2),
    add_packages(1),
    depselector:mark_package_required(0),
    ?assertMatch({error,"package count did not match expected."}, depselector:solve()),
    ?assertEqual(depselector:is_ready(), true).

% TODO essentially we want to test recovery for each possible error
% condition that we're testing elsewhere - let's factor out error
% condition creation.

error_recovery() ->
    depselector:new_problem("TEST", 1),
    add_packages(2),
    depselector:solve(),
    ?assertEqual(depselector:is_ready(), true).

wait_for_ready_test() ->
    application:start(depsolver),
    ?assertEqual(depselector:acquire(), true),
    application:stop(depsolver).


%%%%
%%%
add_packages(0) -> ok;
add_packages(N) ->
    depselector:add_package(0, 0, 0),
    add_packages(N - 1).


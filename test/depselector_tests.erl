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
%%

clean_test_() ->
    common:startup(),
    {foreach,
     fun() ->
        common:add_depselector_pool(1),
        pooler:take_member(depselector)
     end,
     fun(Pid) ->
        % return with "fail" to force the proc to die -
        % our solver may be intentionally left in a hung
        % state by a test.
        pooler:return_member(depselector, Pid, fail),
        pooler:rm_pool(depselector)
     end,
     [
        fun basic_solve/1,
        fun add_dep/1,
        fun abort_in_solve/1,
        fun abort_in_problem/1,
        fun too_many_packages/1,
        fun not_enough_packages/1,
        fun new_problem_from_dirty_state/1,
        fun error_recovery/1
     ]
    }.


basic_solve(Pid) ->
    depselector:new_problem(Pid, "TEST", 1),
    add_packages(Pid, 1),
    depselector:mark_package_required(Pid, 0),
    ?_assertMatch({solution,{{state,valid},{disabled,0},{packages,[{0,0,0}]}}},
                 depselector:solve(Pid)).
add_dep(Pid) ->
    depselector:new_problem(Pid, "TEST", 1),
    add_packages(Pid, 1),
    % This shouldn't cause error or failure
    depselector:add_version_constraint(Pid, 0, 0, 0, 0, 0),
    depselector:mark_package_required(Pid, 0),
    ?_assertMatch({solution,{{state,valid},{disabled,0},{packages,[{0,0,0}]}}},
                  depselector:solve(Pid)).


new_problem_from_dirty_state(Pid) ->
    depselector:new_problem(Pid, "TEST", 1),
    add_packages(Pid, 1),
    % simulate the caller crashing before it can request a solve,
    % then a subsequent use of this FSM for another solve.
    basic_solve(Pid).


abort_in_problem(Pid) ->
    depselector:new_problem(Pid, "TEST", 1),
    depselector:abort(Pid),
    ?_assertEqual(depselector:is_ready(Pid), true).

abort_in_solve(Pid) ->
    depselector:new_problem(Pid, "TEST", 1),
    % This hangs the solver and makes the FSM think it's
    % waiting for a solution response.
    depselector:force_hang(Pid),
    ?_assertEqual(depselector:abort(Pid), {error, busy}).

too_many_packages(Pid) ->
    depselector:new_problem(Pid, "TEST", 1),
    add_packages(Pid, 2),
    ?_assertMatch({error, {solve_fail, "already added max packages"}},
                  depselector:solve(Pid)).

not_enough_packages(Pid) ->
    depselector:new_problem(Pid, "TEST", 2),
    add_packages(Pid, 1),
    depselector:mark_package_required(Pid, 0),
    ?_assertMatch({error,{solve_fail, "package count did not match expected."} }, depselector:solve(Pid)).

error_recovery(Pid) ->
    depselector:new_problem(Pid, "TEST", 1),
    add_packages(Pid, 2),
    depselector:solve(Pid),
    ?_assertEqual(depselector:is_ready(Pid), true).

%% some helpers
add_packages(_Pid, 0) -> ok;
add_packages(Pid, N) ->
    depselector:add_package(Pid, 0, 0, 0),
    add_packages(Pid, N - 1).


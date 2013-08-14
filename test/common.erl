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
%% @doc this module contains some shared setup/teardown functions for
%%      depsolver testing.
%%--------------------------------------------------------------------

-module(common).

-include_lib("eunit/include/eunit.hrl").

-export([startup/0,
         add_depselector_pool/1 ]).

startup() ->
    error_logger:tty(false),
    application:start(pooler).

add_depselector_pool(Count) ->
    PoolerProps = [{name, depselector},
                   {init_count, Count},
                   {max_count, Count},
                   {start_mfa, {depselector, start_link, []}}],
    pooler:new_pool(PoolerProps).



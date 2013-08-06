%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%%
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
%%

-module(depselector_sup).
-behavior(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(depselector_sup, []).

init([]) ->
    PrivDir = priv_dir(),
    ExePath = filename:join([PrivDir, "solver"]),
    {ok, {{one_for_one, 3, 10},
          [{depselector, {depselector, start_link, [ExePath]},
            permanent, 10, worker, [depselector]}]}}.

priv_dir() ->
    priv_dir(code:priv_dir(depsolver)).

% TODO this is a hack that I strongly suspect won't work when depsolver
% itself is a dep
priv_dir({error, bad_name}) ->
    ModDir = filename:dirname(code:which(depsolver)),
    BaseDir = filename:dirname(ModDir),
    filename:join(BaseDir, "priv");
priv_dir(Other) ->
    Other.

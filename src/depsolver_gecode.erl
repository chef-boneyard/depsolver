%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 100 -*-
%% ex: ts=4 sw=4 et
%%
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
%% @author Mark Anderson <mark@opscode.com>
%%
%%%-------------------------------------------------------------------
%%% @doc
%%% This is a dependency constraint solver. You add your 'world' to the
%%% solver. That is the packages that exist, their versions and their
%%% dependencies. Then give the system a set of targets and ask it to solve.
%%%
%%%  Lets say our world looks as follows
%%%
%%%      app1 that has versions "0.1"
%%%        depends on app3 any version greater then "0.2"
%%%       "0.2" with no dependencies
%%%       "0.3" with no dependencies
%%%
%%%      app2 that has versions "0.1" with no dependencies
%%%       "0.2" that depends on app3 exactly "0.3"
%%%       "0.3" with no dependencies
%%%
%%%      app3 that has versions
%%%       "0.1", "0.2" and "0.3" all with no dependencies
%%%
%%% we can add this world to the system all at once as follows
%%%
%%%      Graph0 = depsolver:new_graph(),
%%%      Graph1 = depsolver:add_packages(
%%%             [{app1, [{"0.1", [{app2, "0.2"},
%%%                               {app3, "0.2", '>='}]},
%%%                               {"0.2", []},
%%%                               {"0.3", []}]},
%%%              {app2, [{"0.1", []},
%%%                       {"0.2",[{app3, "0.3"}]},
%%%                       {"0.3", []}]},
%%%              {app3, [{"0.1", []},
%%%                      {"0.2", []},
%%%                      {"0.3", []}]}]).
%%%
%%% We can also build it up incrementally using the other add_package and
%%% add_package_version functions.
%%%
%%% Finally, once we have built up the graph we can ask depsolver to solve the
%%% dependency constraints. That is to give us a list of valid dependencies by
%%% using the solve function. Lets say we want the app3 version "0.3" and all of
%%% its resolved dependencies. We could call solve as follows.
%%%
%%%    depsolver:solve(Graph1, [{app3, "0.3"}]).
%%%
%%% That will give us the completely resolved dependencies including app3
%%% itself. Lets be a little more flexible. Lets ask for a graph that is rooted
%%% in anything greater then or equal to app3 "0.3". We could do that by
%%%
%%%    depsolver:solve(Graph1, [{app3, "0.3", '>='}]).
%%%
%%% Of course, you can specify any number of goals at the top level.
%%% @end
%%%-------------------------------------------------------------------
-module(depsolver_gecode).

-include_lib("eunit/include/eunit.hrl").

%% Public Api
-export([format_error/1,
         format_roots/1,
         format_culprits/1,
         format_constraint/1,
         format_version/1,
         new_graph/0,
         solve/2,
         add_packages/2,
         add_package/3,
         add_package_version/3,
         add_package_version/4]).

-export_type([t/0,
              pkg/0,
              constraint_op/0,
              pkg_name/0,
              vsn/0,
              constraint/0,
              dependency_set/0]).

-export_type([dep_graph/0, constraints/0,
              ordered_constraints/0, fail_info/0,
              fail_detail/0]).
%%============================================================================
%% type
%%============================================================================
-type dep_graph() :: gb_tree().
-opaque t() :: {?MODULE, dep_graph()}.
-type pkg() :: {pkg_name(), vsn()}.
-type pkg_name() :: binary() | atom().
-type raw_vsn() :: ec_semver:any_version().

-type vsn() :: 'NO_VSN'
             | ec_semver:semver().

-type constraint_op() ::
        '=' | gte | '>=' | lte | '<='
      | gt | '>' | lt | '<' | pes | '~>' | between.

-type raw_constraint() :: pkg_name()
                        | {pkg_name(), raw_vsn()}
                        | {pkg_name(), raw_vsn(), constraint_op()}
                        | {pkg_name(), raw_vsn(), vsn(), between}.

-type constraint() :: pkg_name()
                    | {pkg_name(), vsn()}
                    | {pkg_name(), vsn(), constraint_op()}
                    | {pkg_name(), vsn(), vsn(), between}.


-type vsn_constraint() :: {raw_vsn(), [raw_constraint()]}.
-type dependency_set() :: {pkg_name(), [vsn_constraint()]}.

%% Internal Types
-type constraints() :: [constraint()].
-type ordered_constraints() :: [{pkg_name(), constraints()}].
-type fail_info() :: {[pkg()], ordered_constraints()}.
-type fail_detail() :: {fail, [fail_info()]} | {missing, pkg_name()}.

%%============================================================================
%% Macros
%%============================================================================
-define(DEFAULT_TIMEOUT, 2000).
-define(RUNLIST, runlist).
-define(RUNLIST_VERSION, {0,0,0}).
%%============================================================================
%% API
%%============================================================================
%% @doc create a new empty dependency graph
-spec new_graph() -> t().
new_graph() ->
    {?MODULE, gb_trees:empty()}.

%% @doc add a complete set of list of packages to the graph. Where the package
%% consists of the name and a list of versions and dependencies.
%%
%% ``` depsolver:add_packages(Graph,
%%               [{app1, [{"0.1", [{app2, "0.2"},
%%                                 {app3, "0.2", '>='}]},
%%                                 {"0.2", []},
%%                                 {"0.3", []}]},
%%                 {app2, [{"0.1", []},
%%                         {"0.2",[{app3, "0.3"}]},
%%                         {"0.3", []}]},
%%                 {app3, [{"0.1", []},
%%                         {"0.2", []},
%%                         {"0.3", []}]}])
%% '''
-spec add_packages(t(),[dependency_set()]) -> t().
add_packages(Dom0, Info)
  when is_list(Info) ->
    lists:foldl(fun({Pkg, VsnInfo}, Dom1) ->
                        add_package(Dom1, Pkg, VsnInfo)
                end, Dom0, Info).

%% @doc add a single package to the graph, where it consists of a package name
%% and its versions and thier dependencies.
%%  ```depsolver:add_package(Graph, app1, [{"0.1", [{app2, "0.2"},
%%                                              {app3, "0.2", '>='}]},
%%                                              {"0.2", []},
%%                                              {"0.3", []}]}]).
%% '''
-spec add_package(t(),pkg_name(),[vsn_constraint()]) -> t().
add_package(State, Pkg, Versions)
  when is_list(Versions) ->
    lists:foldl(fun({Vsn, Constraints}, Dom1) ->
                        add_package_version(Dom1, Pkg, Vsn, Constraints);
                   (Version, Dom1) ->
                        add_package_version(Dom1, Pkg, Version, [])
                end, State, Versions).

%% @doc add a set of dependencies to a specific package and version.
%% and its versions and thier dependencies.
%%  ```depsolver:add_package(Graph, app1, "0.1", [{app2, "0.2"},
%%                                              {app3, "0.2", '>='}]},
%%                                              {"0.2", []},
%%                                              {"0.3", []}]).
%% '''
-spec add_package_version(t(), pkg_name(), raw_vsn(), [raw_constraint()]) -> t().
add_package_version({?MODULE, Dom0}, RawPkg, RawVsn, RawPkgConstraints) ->
    Pkg = fix_pkg(RawPkg),
    Vsn = parse_version(RawVsn),
    %% Incoming constraints are raw
    %% and need to be fixed
    PkgConstraints = [fix_con(PkgConstraint) ||
                         PkgConstraint <- RawPkgConstraints],
    Info2 =
        case gb_trees:lookup(Pkg, Dom0) of
            {value, Info0} ->
                case lists:keytake(Vsn, 1, Info0) of
                    {value, {Vsn, Constraints}, Info1} ->
                        [{Vsn, join_constraints(Constraints,
                                                PkgConstraints)}
                         | Info1];
                    false ->
                        [{Vsn,  PkgConstraints}  | Info0]
                end;
            none ->
                [{Vsn, PkgConstraints}]
        end,
    {?MODULE, gb_trees:enter(Pkg, Info2, Dom0)}.

%% @doc add a package and version to the dependency graph with no dependency
%% constraints, dependency constraints can always be added after the fact.
%%
%% ```depsolver:add_package_version(Graph, app1, "0.1").'''
-spec add_package_version(t(),pkg_name(),raw_vsn()) -> t().
add_package_version(State, Pkg, Vsn) ->
    add_package_version(State, Pkg, Vsn, []).

%% @doc Given a set of goals (in the form of constrains) find a set of packages
%% and versions that satisfy all constraints. If no solution can be found then
%% an exception is thrown.
%% ``` depsolver:solve(State, [{app1, "0.1", '>='}]).'''
-spec solve( t(),[constraint()]) -> {ok, [pkg()]} | {error, term()}.
solve({?MODULE, DepGraph0}, RawGoals) when erlang:length(RawGoals) > 0 ->
    try
        ?debugVal(depselector:new_problem_with_debug("TEST", gb_trees:size(DepGraph0) + 1)),
        Problem = generate_versions(DepGraph0),
        ?debugFmt("~p~n", [Problem]),
        generate_constraints(DepGraph0, RawGoals, Problem),
        Solution = depselector:solve(),
        extract_constraints(Solution, Problem)
    catch
        throw:{unreachable_package, Name} ->
            {error, {unreachable_package, Name}}
    end.

%% Instantiate versions
generate_versions(DepGraph0) ->
    Versions0 = version_manager:new(),
    %% the runlist is treated as a virtual package.
    Versions1 = version_manager:add_package(?RUNLIST, [?RUNLIST_VERSION], Versions0),
    depselector:add_package(0,0,0),
    depselector:mark_package_required(0),

    %% Add all the other packages
    add_versions_for_package(gb_trees:next(gb_trees:iterator(DepGraph0)), Versions1).

add_versions_for_package(none, Acc) ->
    Acc;
add_versions_for_package({PkgName, VersionConstraints, Iterator}, Acc) ->
    {Versions, _} = lists:unzip(VersionConstraints),
    NAcc = version_manager:add_package(PkgName, Versions, Acc),
    %% -1 denotes the possibility of an unused package.
    %% While the named versions are always in the range 0...N, we
    %% may want to mark a package as not used As soon as a package is mentioned in the dependency
    %% chain, it creates a constraint limiting it to be 0 or greater, but until it is mentioned, it
    %% can be -1, and hence unused.
    MinVersion = -1,
    MaxVersion = length(Versions) - 1,
    depselector:add_package(MinVersion, MaxVersion, MaxVersion),
    add_versions_for_package(gb_trees:next(Iterator), NAcc).


%% Constraints for each version
generate_constraints(DepGraph, RawGoals, Problem) ->
    %% The runlist package is a synthetic package
    add_constraints_for_package(?RUNLIST, [{ ?RUNLIST_VERSION, RawGoals }], Problem),
    gb_trees:map(fun(N,C) -> add_constraints_for_package(N,C,Problem) end, DepGraph).

add_constraints_for_package(PkgName, VersionConstraints, Problem) ->
    AddVersionConstraint =
        fun(Version, Constraints) ->
                ?debugFmt("N:~p P:~p~n", [PkgName, Version]),
                {PkgIndex, VersionId} = version_manager:get_version_id(PkgName, Version, Problem),
                ?debugFmt("N:~p P:~p~n", [PkgIndex, VersionId]),
                [ add_constraint_element(Constraint, PkgIndex, VersionId, Problem) || Constraint <- Constraints]
        end,
    [AddVersionConstraint(PkgVersion, ConstraintList) || {PkgVersion, ConstraintList} <- VersionConstraints].

add_constraint_element({DepPkgName, Version}, PkgIndex, VersionId, Problem) ->
    add_constraint_element({DepPkgName, Version, eq}, PkgIndex, VersionId, Problem);
add_constraint_element({DepPkgName, DepPkgVersion, Type}, PkgIndex, VersionId, Problem) ->
    ?debugFmt("DP: ~p C: ~p ~p~n", [DepPkgName, DepPkgVersion, Type]),
    add_constraint_element_helper(DepPkgName, {DepPkgVersion, Type}, PkgIndex, VersionId, Problem);
add_constraint_element({DepPkgName, DepPkgVersion1, DepPkgVersion2, Type}, PkgIndex, VersionId, Problem) ->
    ?debugFmt("DP: ~p C: ~p ~p ~p~n", [DepPkgName, DepPkgVersion1, DepPkgVersion2, Type]),
    add_constraint_element_helper(DepPkgName, {DepPkgVersion1, DepPkgVersion2, Type},
                                  PkgIndex, VersionId, Problem);
add_constraint_element(DepPkgName, PkgIndex, VersionId, Problem) when not is_tuple(DepPkgName) ->
    ?debugFmt("DP: ~p C: ~p ~n", [DepPkgName, any]),
    add_constraint_element_helper(DepPkgName, any, PkgIndex, VersionId, Problem).

add_constraint_element_helper(DepPkgName, Constraint, PkgIndex, VersionId, Problem) ->
    case version_manager:map_constraint(DepPkgName, Constraint, Problem) of
        {DepPkgIndex, {Min,Max}} ->
            version_manager:map_constraint(DepPkgName, Constraint, Problem),
            depselector:add_version_constraint(PkgIndex, VersionId, DepPkgIndex, Min, Max);
        no_matching_package ->
            throw( {unreachable_package, DepPkgName} )
    end.

extract_constraints({ok, {solution,
                          {{state, invalid},
                           {disabled, _Disabled_Count},
                           {packages, PackageVersionIds}} = Results} = Solution},
                     Problem) ->
    ?debugFmt("~p~n",[Solution]),
    {{state, _}, {disabled, _Disabled_Count}, {packages, PackageVersionIds}} = Results,
    PackageList = unmap_packed_solution(PackageVersionIds, Problem),
    {error, PackageList};
extract_constraints({ok, {solution, Results}} = Solution, Problem) ->
    ?debugFmt("~p~n",[Solution]),
    {{state, _}, {disabled, _Disabled_Count}, {packages, PackageVersionIds}} = Results,
    PackageList = unmap_packed_solution(PackageVersionIds, Problem),
    {ok, PackageList}.


unmap_packed_solution(PackageVersionIds, Problem) ->
    %% The runlist is a synthetic package, and should be filtered out
    [{0,0,0} | PackageVersionIdsReal ] = PackageVersionIds,
    PackageList = [version_manager:unmap_constraint({PackageId, VersionId}, Problem) ||
                      {PackageId, _DisabledState, VersionId} <- PackageVersionIdsReal],
    ?debugFmt("R ~p~n", [PackageList]),
    PackageList.

%% Parse a string version into a tuple based version
-spec parse_version(raw_vsn() | vsn()) -> vsn().
parse_version(RawVsn)
  when erlang:is_list(RawVsn);
       erlang:is_binary(RawVsn) ->
    ec_semver:parse(RawVsn);
parse_version(Vsn)
  when erlang:is_tuple(Vsn) ->
    Vsn.

%% @doc Produce a full error message for a given error condition.  This includes
%% details of the paths taken to resolve the dependencies and shows which dependencies
%% could not be satisfied
-spec format_error({error, {unreachable_package, list()} |
                           {invalid_constraints, [constraint()]} |
                           list()}) -> iolist().
format_error(Error) ->
    depsolver_culprit:format_error(Error).

%% @doc Return a formatted list of roots of the dependency trees which
%% could not be satisified. These may also have versions attached.
%% Example:
%%
%%    ```(foo = 1.2.0), bar```
%%
-spec format_roots([constraints()]) -> iolist().
format_roots(Roots) ->
    depsolver_culprit:format_roots(Roots).


%% @doc Return a formatted list of the culprit depenedencies which led to
%% the dependencies not being satisfied. Example:
%%
%%     ```(foo = 1.2.0) -> (bar > 2.0.0)```
-spec format_culprits([{[constraint()], [constraint()]}]) -> iolist().
format_culprits(Culprits) ->
    depsolver_culprit:format_culprits(Culprits).

%% @doc A formatted version tuple
-spec format_version(vsn()) -> iolist().
format_version(Version) ->
    depsolver_culprit:format_version(Version).

%% @doc A formatted constraint tuple
-spec format_constraint(constraint()) -> iolist().
format_constraint(Constraint) ->
    depsolver_culprit:format_constraint(Constraint).

%% @doc
%% fix the package name. If its a list turn it into a binary otherwise leave it as an atom
fix_pkg(Pkg) when is_list(Pkg) ->
    erlang:list_to_binary(Pkg);
fix_pkg(Pkg) when is_binary(Pkg); is_atom(Pkg) ->
    Pkg.

%% @doc
%% fix package. Take a package with a possible invalid version and fix it.
-spec fix_con(raw_constraint()) -> constraint().
fix_con({Pkg, Vsn}) ->
    {fix_pkg(Pkg), parse_version(Vsn)};
fix_con({Pkg, Vsn, CI}) ->
    {fix_pkg(Pkg), parse_version(Vsn), CI};
fix_con({Pkg, Vsn1, Vsn2, CI}) ->
    {fix_pkg(Pkg), parse_version(Vsn1),
     parse_version(Vsn2), CI};
fix_con(Pkg) ->
    fix_pkg(Pkg).


%% @doc given two lists of constraints join them in such a way that no
%% constraint is duplicated but the over all order of the constraints is
%% preserved. Order drives priority in this solver and is important for that
%% reason.
-spec join_constraints([constraint()], [constraint()]) ->
                              [constraint()].
join_constraints(NewConstraints, ExistingConstraints) ->
    ECSet = sets:from_list(ExistingConstraints),
    FilteredNewConstraints = [NC || NC <- NewConstraints,
                                    not sets:is_element(NC, ECSet)],
    ExistingConstraints ++ FilteredNewConstraints.

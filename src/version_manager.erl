%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 100 -*-
%%% ex: ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @author Mark Anderson <>
%%% @copyright (C) 2013, Mark Anderson
%%% @doc
%%%
%%% @end
%%% Created : 30 Jul 2013 by Mark Anderson <>
%%%-------------------------------------------------------------------
-module(version_manager).
-include_lib("eunit/include/eunit.hrl").
%% API
-export([parse/1,
         new/0,
         make/1, %% deprecated
         add_package/3,
         get_id/2,
         get_version_id/3,
         get_version_max_for_package/2,
         map_constraint/3,
         unmap_constraint/2,
         int_to_version/2,
         version_to_int/2,
         constraint_to_range/2,
         search_eq/2,
         search_gt/2,
         search_gte/2,
         search_lt/2,
         search_lte/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% Valid package versions are between -1 and its max (-1 means don't care, meaning it doesn't need
%% to be assigned). To indicate constraints that match no versions, -2 is used, since it's not a
%% valid assignment of the variable; thus, any branch that assigns -2 will fail.
%%
%% This mechanism is also used when a dependent package has no versions, which only happens if the
%% dependency's package is auto-vivified when creating the parent PackageVersion's dependency but
%% with no corresponding set of PackageVersions (i.e. it's an invalid deendency, because it does not
%% exist in the dependency graph). Again, we won't abort immediately, but we'll add a constraint to
%% the package that makes exploring that portion of the solution space unsatisfiable. Thus it is
%% impossible to find solutions dependent on non-existent packages.
-define(DONT_CARE_CONSTRAINT, -1).
-define(NO_MATCH_CONSTRAINT, {-2, -2}).

-type vsn() :: 'NO_VSN'
             | ec_semver:semver().

-type constraint_op() ::
        '=' | gte | '>=' | lte | '<='
      | gt | '>' | lt | '<' | pes | '~>' | between.

-type constraint() :: {vsn(), constraint_op()}.

%-type vsn_constraint() :: {raw_vsn(), [raw_constraint()]}.
%-type dependency_set() :: {pkg_name(), [vsn_constraint()]}.

-record(cookbook_version_mapper,
        { versionList :: array,
          isMissing = false :: boolean()
        } ).

-record(package,
        { name :: string(),
          index :: integer(),
          versionMapper :: any() }). %% cookbook_version_mapper{} }).


-record(problem,
        { byName :: gb_tree(),
          byId   :: array()} ).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

new() ->
    #problem{byName = gb_trees:empty(), byId = array:new()}.

add_package(PkgName, Versions, #problem{byName=ByName, byId=ById} = Prob) ->
    CVM = make(Versions),
    Index = array:size(ById),
    Package = #package{name = PkgName, index = Index, versionMapper = CVM},
    NById = array:set(Index, Package, ById),
    NByName = gb_trees:enter(PkgName, Package, ByName),
    Prob#problem{byName=NByName, byId = NById}.

get_id(PkgName, #problem{byName=ByName}) ->
    {value, #package{index=Index}} = gb_trees:lookup(PkgName, ByName),
    Index.

get_version_id(PkgName, Version, #problem{byName=ByName}) ->
    {value, #package{index=Index, versionMapper = Mapper}} = gb_trees:lookup(PkgName, ByName),
    VersionId = version_to_int(Version, Mapper),
    {Index, VersionId}.

get_version_max_for_package(PkgName, #problem{byName=ByName}) ->
    {value, #package{versionMapper = Mapper}} = gb_trees:lookup(PkgName, ByName),
    mapper_get_version_max(Mapper).

map_constraint(DependentPackage, Constraint, #problem{byName=ByName}) ->
    case gb_trees:lookup(DependentPackage, ByName) of
        none ->
            %% TODO Check that this is correct (re-read depselector code to discover what we do when
            %% there is a dependency on a missing constraint; I suspect there we need to flag it as
            %% 'suspicious'...
            no_matching_package;
        {value, #package{index=Index, versionMapper = Mapper}} ->
            ConstraintRange = constraint_to_range(Constraint, Mapper),
%            ?debugFmt("PKG: ~p C ~p -> I ~p R ~p~n",
%                      [DependentPackage, Constraint, Index, ConstraintRange]),
            {Index, ConstraintRange}
    end.

-spec unmap_constraint({integer(),integer()}, #problem{}) ->
                              undefined_package |
                              {any(), integer() | unused | out_of_range}.
unmap_constraint({PackageId, VersionId}, #problem{byId=ById}) ->
    case array:size(ById) of
        S when PackageId < 0 orelse PackageId >= S ->
            undefined_package;
        _ ->
            #package{name = PackageName, versionMapper = VersionMapper} = array:get(PackageId, ById),
            Version = int_to_version(VersionId, VersionMapper),
            {PackageName, Version}
    end.

%%
%%
%% Version mapping/unmapping to a dense set
-spec make([vsn()]) -> #cookbook_version_mapper{}.
make([{missing}]) ->
    #cookbook_version_mapper{versionList = array:new(), isMissing = true};
make(Versions) ->
    SortedVersions = lists:sort(fun ec_semver:lt/2, lists:map(fun parse/1, Versions)),
    ArrayVersions = array:from_list(SortedVersions),
    #cookbook_version_mapper{versionList = ArrayVersions, isMissing = false}.

version_to_int(Version, #cookbook_version_mapper{versionList = VersionList}) ->
    search_eq(Version, VersionList).

int_to_version(-1, _)  ->
    unused;
int_to_version(Value, #cookbook_version_mapper{versionList = Versions}) ->
    case array:size(Versions) of
        S when Value >= S ->
            out_of_range;
        _ ->
            {Version, _} = array:get(Value, Versions),
            Version
    end.

mapper_get_version_max(#cookbook_version_mapper{isMissing = true}) ->
    -1;
mapper_get_version_max(#cookbook_version_mapper{versionList = ArrayVersions, isMissing = false}) ->
    array:size(ArrayVersions) - 1.


parse({V, {A, B}}) when is_tuple(V) andalso size(V) < 4 ->
    {V, {A,B}};
parse({X,Y,Z}) when is_integer(X) andalso
                    is_integer(Y) andalso
                    is_integer(Z) ->
    {{X,Y,Z}, {[],[]}};
parse({X,Y}) when is_integer(X) andalso is_integer(Y) ->
    {{X,Y}, {[],[]}};
parse({X}) when not is_tuple(X) ->
    {X, {[],[]}};
parse(X) when is_integer(X) ->
    {X, {[], []}};
parse(X) when is_binary(X) orelse is_list(X) ->
    ec_semver:parse(X).


%% Manage folding a version tuple down to

-spec constraint_to_range(constraint(), #cookbook_version_mapper{}) -> {int, int}.
constraint_to_range(any, #cookbook_version_mapper{versionList = VersionList}) ->
    Max = find_max(VersionList),
    maybe_impossible_constraint(0, Max);

constraint_to_range({Version, '='}, Versions) ->
    constraint_to_range({Version, eq}, Versions);
constraint_to_range({Version, eq},   #cookbook_version_mapper{versionList = VersionList}) ->
    Min = search_eq(Version, VersionList),
    maybe_impossible_constraint(Min,Min);

constraint_to_range({Version, '>='}, Versions) ->
    constraint_to_range({Version, gte}, Versions);
constraint_to_range({Version, gte},   #cookbook_version_mapper{versionList = VersionList}) ->
    Min = search_gte(Version, VersionList),
    Max = find_max(VersionList),
    maybe_impossible_constraint(Min, Max);

constraint_to_range({Version, '>'}, Versions) ->
    constraint_to_range({Version, gt}, Versions);
constraint_to_range({Version, gt},   #cookbook_version_mapper{versionList = VersionList}) ->
    Min = search_gt(Version, VersionList),
    Max = find_max(VersionList),
    maybe_impossible_constraint(Min, Max);

constraint_to_range({Version, '<='}, Versions) ->
    constraint_to_range({Version, lte}, Versions);
constraint_to_range({Version, lte},  #cookbook_version_mapper{versionList = VersionList}) ->
    Max = search_lte(Version, VersionList),
    maybe_impossible_constraint(0, Max);

constraint_to_range({Version, '<'}, Versions) ->
    constraint_to_range({Version, lt}, Versions);
constraint_to_range({Version, lt},  #cookbook_version_mapper{versionList = VersionList}) ->
    Max = search_lt(Version, VersionList),
    maybe_impossible_constraint(0, Max);

constraint_to_range({Version1, Version2, between},
                    #cookbook_version_mapper{versionList = VersionList}) ->
    Min = search_gte(Version1, VersionList),
    Max = search_lte(Version2, VersionList),
    maybe_impossible_constraint(Min, Max);

constraint_to_range({Version, '~>'}, Versions) ->
    constraint_to_range({Version, pes}, Versions);
constraint_to_range({Version, pes},  #cookbook_version_mapper{versionList = VersionList}) ->
    Min = search_gte(Version, VersionList),
    Max = case find_next_version(parse(Version)) of
              version_infinity ->
                  find_max(VersionList);
              Next ->
                  search_lt(Next, VersionList)
          end,
    maybe_impossible_constraint(Min,Max).

maybe_impossible_constraint(not_found, _)->
    ?NO_MATCH_CONSTRAINT;
maybe_impossible_constraint(_, not_found) ->
    ?NO_MATCH_CONSTRAINT;
maybe_impossible_constraint(Min, Max) when Min > Max ->
    ?NO_MATCH_CONSTRAINT;
maybe_impossible_constraint(Min, Max) ->
    {Min, Max}.

find_max(#cookbook_version_mapper{versionList = VersionList}) ->
    find_max(VersionList);
find_max(VersionList) ->
    array:size(VersionList)-1.

%% Expects normalized versions (may need to parse first)
find_next_version({ V, {_,_} }) when is_integer(V) ->
    version_infinity;
find_next_version({{Major, Minor}, {A,B}})
  when is_integer(Major) andalso
       is_integer(Minor) ->
    {{Major+1, 0, 0}, {A,B}};
find_next_version({{Major, Minor, _Patch}, {A,B}}) ->
    {{Major, Minor+1, 0}, {A,B}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

search_eq(Version, VersionArray) ->
    NVersion = parse(Version),
    End = array:size(VersionArray),
    Pos = search_gte(VersionArray, NVersion, 0, End),
    case Pos of
        End -> not_found;
        _ -> %% In bounds, safe to fetch
            FVersion = array:get(Pos, VersionArray),
            case ec_semver:eql(FVersion, NVersion) of
                true -> Pos;
                _ -> not_found
            end
    end.

%% Binary search: Find least element greater than or equal to Version
search_gte(Version, VersionArray) ->
    NVersion = parse(Version),
    End = array:size(VersionArray),
    Pos = search_gte(VersionArray, NVersion, 0, End),
    case Pos of
        End ->
            not_found;
        _ ->
            Pos
    end.

%% Binary search: Find least element greater than Version
search_gt(Version, VersionArray) ->
    NVersion = parse(Version),
    End = array:size(VersionArray),
    Pos = search_gte(VersionArray, NVersion, 0, End),
    case Pos of
        End ->
            not_found;
        _ -> %% In bounds, safe to fetch
            FVersion = array:get(Pos, VersionArray),
            case ec_semver:eql(FVersion, NVersion) of
                true when Pos =:= (End-1) ->
                    not_found;
                true -> Pos+1;
                _ -> Pos
            end
    end.

%% Binary search: Find greatest element less than or equal to Version
search_lte(Version, VersionArray) ->
    NVersion = parse(Version),
    End = array:size(VersionArray),
    %% This returns the least element greater than or equal to version..
    Pos = search_gte(VersionArray, NVersion, 0, End),
    case Pos < End andalso ec_semver:eql(array:get(Pos, VersionArray), NVersion) of
        true ->
            Pos;
        _ when Pos =:= 0 ->
            not_found;
        _ ->
            Pos - 1
    end.

%% Binary search: Find greatest element less than Version
search_lt(Version, VersionArray) ->
    NVersion = parse(Version),
    End = array:size(VersionArray),
    %% This returns the least element greater than or equal to version..
    Pos = search_gte(VersionArray, NVersion, 0, End),
    case Pos of
        0 ->
            not_found;
        _ ->
            Pos - 1
    end.

%% Binary search: Find least element greater than or equal to Version
%% Start start of array
%% End end+1 of array (Valid range Start<= x < End)
%% Return value Pos is in [Start, End] (inclusive)
search_gte(_VersionArray, _Version, Pos, Pos) ->
    Pos;
search_gte(VersionArray, Version, Start, End) ->
    Pos = trunc((Start+End)/2), %% Pos < End if Start < End
    case ec_semver:gte(array:get(Pos, VersionArray), Version) of
        true ->
            %% Start <= Pos < End, so this always shrinks the bounds, maintains invariant Start<End
            search_gte(VersionArray, Version, Start, Pos);
        _ ->
            %% Start < End so this shrinks bounds, but does not break invariant Start<=End
            search_gte(VersionArray, Version, Pos+1, End)
    end.

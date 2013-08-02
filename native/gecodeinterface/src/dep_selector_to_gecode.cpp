//
// Author:: Christopher Walters (<cw@opscode.com>)
// Author:: Mark Anderson (<mark@opscode.com>)
// Copyright:: Copyright (c) 2010-2011 Opscode, Inc.
// License:: Apache License, Version 2.0
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/gist.hh>
#include <gecode/search.hh>

#include "dep_selector_to_gecode.h"

#include <cstdio>
#include <limits>
#include <iostream>
#include <fstream>
#include <vector>

// For dir logging stuff
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

//#define MEMORY_DEBUG
//#define DEBUG
#define DEBUG_STREAM std::cerr
//#define USE_DUMB_BRANCHING
#define VECTOR_CONSTRAIN

const int DEP_SELECTOR_LOG_PATH_LENGTH = 1024;
const char * DEP_SELECTOR_LOG_PATH = "/tmp/dep-selector-logs";
const int DEP_SELECTOR_LOG_DIR_PERMS = S_IRWXU | S_IRGRP | S_IXGRP;


using namespace Gecode;
const int VersionProblem::UNRESOLVED_VARIABLE = INT_MIN;
const int VersionProblem::MIN_TRUST_LEVEL = 0;
const int VersionProblem::MAX_TRUST_LEVEL = 10;
const int VersionProblem::MAX_PREFERRED_WEIGHT = 10;



VersionProblemPool::VersionProblemPool() : elems()
{ }

VersionProblemPool::~VersionProblemPool()
{
    DeleteAll();
}
void VersionProblemPool::Add(VersionProblem * vp)
{
    vp->pool = this;
#ifdef MEMORY_DEBUG
    DEBUG_STREAM << "Pool add\t" << vp << std::endl << std::flush;
#endif // MEMORY_DEBUG
    elems.insert(vp);
}
void VersionProblemPool::Delete(VersionProblem *vp)
{
    if (vp->pool != 0)
        {
#ifdef MEMORY_DEBUG
            DEBUG_STREAM << "Pool del\t" << vp << std::endl << std::flush;
#endif // MEMORY_DEBUG
            elems.erase(vp);
            vp->pool = 0;
        }
}
void VersionProblemPool::ShowAll()
{
    DEBUG_STREAM << "ShowAll =====================================================" << std::endl << std::flush;
    std::set<VersionProblem *>::iterator i;
    for(i = elems.begin(); i != elems.end(); i++) {
#ifdef MEMORY_DEBUG
        DEBUG_STREAM << "ShowAll has\t\t\t" << *i << std::endl << std::flush;
#endif // MEMORY_DEBUG
    }
    DEBUG_STREAM << "ShowAll =====================================================" << std::endl << std::flush;
}

void VersionProblemPool::DeleteAll()
{
#ifdef MEMORY_DEBUG
    ShowAll();
#endif
    std::set<VersionProblem *>::iterator i;
    for(i = elems.begin(); i != elems.end(); i++) {
        VersionProblem *vp = *i;
        vp->pool = 0;
        delete *i;
    }
    elems.clear();
#ifdef MEMORY_DEBUG
    DEBUG_STREAM << "DeleteAll ===================================================" << std::endl << std::flush;
#endif
}
//////////////////////////////////////////////////////////////////////
//
//

SolutionLog::SolutionLog(const char * logFileName)
    : name(logFileName), use_std_error(true), count(0)
{
}

SolutionLog::SolutionLog(const std::string & logFileName)
    : name(logFileName), use_std_error(true), count(0)
{
}

SolutionLog::SolutionLog(SolutionLog & log)
    : name(log.name), use_std_error(true), count(0)
{
}

SolutionLog::~SolutionLog()
{
    log.close();
}


bool mkdir_helper(const char * name)
{
    // std::cerr << "Creating " << name << std::endl << std::flush;
    if (mkdir(name, DEP_SELECTOR_LOG_DIR_PERMS)) {
        switch (errno) {
        case EEXIST:
            // Already exists, don't worry about it
            return true;
        default:
            // fail
            std::cerr << "Failed to create: " << name << " error " << strerror(errno) << std::endl << std::flush;
            return false;
        }
    }
}

void SolutionLog::Setup()
{
    if (!mkdir_helper(DEP_SELECTOR_LOG_PATH)) {
        use_std_error = true;
        return;
    }
    std::string path(DEP_SELECTOR_LOG_PATH);
    path += "/";
    path += name[0];
    path += name[1];
    if (!mkdir_helper(path.c_str())) {
        use_std_error = true;
        return;
    }

    path += "/";
    path += name;
    // std::cerr << "Logging to file " << name << std::endl << std::flush;
    log.open(path.c_str(), std::ios::out | std::ios::app );

    use_std_error = false;
}

std::ostream & SolutionLog::Log()
{
    if (use_std_error) {
        return std::cerr;
    } else {
        return log;
    }
}

////////////////////////////////////////////////////////////////////////
//
//
int VersionProblem::instance_counter = 0;

VersionProblem::VersionProblem(int packageCount, bool dumpStats, bool debug, const char * _logId)
    : size(packageCount), version_constraint_count(0), dump_stats(dumpStats),
      debugLogging(debug),
      finalized(false), cur_package(0), package_versions(*this, packageCount),
      disabled_package_variables(*this, packageCount, 0, 1), total_disabled(*this, 0, packageCount*MAX_TRUST_LEVEL),
      total_required_disabled(*this, 0, packageCount), total_induced_disabled(*this, 0, packageCount),
      total_suspicious_disabled(*this, 0, packageCount),
      is_required(new int[packageCount]),
      is_suspicious(new int[packageCount]),
      at_latest(*this, packageCount, 0, 1),
      // These domains could be narrowed a bit; check later
      total_preferred_at_latest(*this, -packageCount*MAX_PREFERRED_WEIGHT, packageCount*MAX_PREFERRED_WEIGHT),
      total_not_preferred_at_latest(*this, -packageCount, packageCount),
      preferred_at_latest_weights(new int[packageCount]),
      pool(0),
      instance_id(instance_counter++),
      log(_logId)
{

    log.Setup();

    strncpy(logId, _logId, DEBUG_PREFIX_LENGTH);
    char * end = strncpy(debugPrefix, _logId, DEBUG_PREFIX_LENGTH);
    strncat(end, ": ", DEBUG_PREFIX_LENGTH-(debugPrefix-end));
    for (int i = 0; i < packageCount; i++)
        {
            preferred_at_latest_weights[i] = 0;
            is_required[i] = 0;
            is_suspicious[i] = 0;
        }
    if (debugLogging) {
        log.Log() << std::endl;
        log.Log() << debugPrefix << "Creating VersionProblem inst# " << instance_id << " with " << packageCount << " packages, "
                     << dumpStats << " stats, " << debug << " debug" << std::endl;
        log.Log().flush();
    }
}

VersionProblem::VersionProblem(bool share, VersionProblem & s)
    : Space(share, s),
      size(s.size), version_constraint_count(s.version_constraint_count),
      dump_stats(s.dump_stats),
      debugLogging(s.debugLogging),
      finalized(s.finalized), cur_package(s.cur_package),
      disabled_package_variables(s.disabled_package_variables), total_disabled(s.total_disabled),
      total_required_disabled(s.total_required_disabled), total_induced_disabled(s.total_induced_disabled),
      total_suspicious_disabled(s.total_suspicious_disabled),
      is_required(NULL), is_suspicious(NULL),
      at_latest(s.at_latest),
      total_preferred_at_latest(s.total_preferred_at_latest),
      total_not_preferred_at_latest(s.total_preferred_at_latest),
      preferred_at_latest_weights(NULL),
      pool(s.pool),
      instance_id(s.instance_id),
      log(s.log)
{
    strncpy(debugPrefix, s.debugPrefix, DEBUG_PREFIX_LENGTH),
    strncpy(logId, s.logId, DEBUG_PREFIX_LENGTH);
    package_versions.update(*this, share, s.package_versions);
    disabled_package_variables.update(*this, share, s.disabled_package_variables);
    total_disabled.update(*this, share, s.total_disabled);
    total_required_disabled.update(*this, share, s.total_required_disabled);
    total_induced_disabled.update(*this, share, s.total_induced_disabled);
    total_suspicious_disabled.update(*this, share, s.total_suspicious_disabled);
    at_latest.update(*this, share, s.at_latest);
    total_preferred_at_latest.update(*this, share, s.total_preferred_at_latest);
    total_not_preferred_at_latest.update(*this, share, s.total_not_preferred_at_latest);

    pool->Add(this);
#ifdef MEMORY_DEBUG
    log.Log() << "C VersionProblem(bool, VP)\t" << this << std::endl << std::flush;
#endif
}

// Support for gecode
Space* VersionProblem::copy(bool share)
{
    return new VersionProblem(share,*this);
}

VersionProblem::~VersionProblem()
{
    delete[] preferred_at_latest_weights;
    delete[] is_required;
    delete[] is_suspicious;

    if (pool!= 0) {
        pool->Delete(this);
    }
#ifdef MEMORY_DEBUG
    log.Log() << "D VersionProblem\t\t" << this << std::endl << std::flush;
#endif
}

int VersionProblem::Size()
{
    return size;
}

int VersionProblem::PackageCount()
{
    return cur_package;
}

int
VersionProblem::AddPackage(int minVersion, int maxVersion, int currentVersion)
{
    if (cur_package == size) {
        return -1;
    }

    if (debugLogging) {
        sprintf(outputBuffer, "%sDepSelector inst# %d - Adding package id %d/%d: min = %d, max = %d, current version %d",
                debugPrefix, instance_id, cur_package, size, minVersion, maxVersion, currentVersion);
        log.Log() << outputBuffer << std::endl;
//        log.Log() << debugPrefix << "DepSelector inst# " << instance_id
//                     << " - Adding package id " << cur_package << '/' << size << ": min = " << minVersion << ", max = " << maxVersion << ", current version " << currentVersion << std::endl;
        log.Log().flush();
    }
    int index = cur_package;
    cur_package++;
    //  IntVar version(*this, minVersion, maxVersion);
    package_versions[index] = IntVar(*this, minVersion, maxVersion);

    // register the binding of package to version that corresponds to the package's latest
    rel(*this, package_versions[index], IRT_EQ, maxVersion, at_latest[index]);

    return index;
}

bool
VersionProblem::AddVersionConstraint(int packageId, int version,
                                     int dependentPackageId, int minDependentVersion, int maxDependentVersion)
{
    BoolVar version_match(*this, 0, 1);
    BoolVar depend_match(*this, 0, 1);
    BoolVar predicated_depend_match(*this, 0, 1);

    version_constraint_count++;
    if (debugLogging) {
        sprintf(outputBuffer, "%sDepSelector inst# %d - Adding VC for %d @ %d depPkg %d [%d, %d]",
                debugPrefix, instance_id, packageId, version, dependentPackageId, minDependentVersion, maxDependentVersion);
        log.Log() << outputBuffer << std::endl;
//        log.Log() << debugPrefix << "DepSelector inst# " << instance_id
//                     << " - Adding VC for " << packageId << " @ " << version << " depPkg " << dependentPackageId
//                     << " [ " << minDependentVersion << ", " << maxDependentVersion << " ]" << std::endl;
        log.Log().flush();
    }


    //version_flags << version_match;
    // Constrain pred to reify package @ version
    rel(*this, package_versions[packageId], IRT_EQ, version, version_match);
    // Add the predicated version constraints imposed on dependent package

    // package_versions[dependendPackageId] in domain [minDependentVersion,maxDependentVersion] <=> depend_match
    dom(*this, package_versions[dependentPackageId], minDependentVersion, maxDependentVersion, depend_match);

    // disabled_package_variables[dependentPackageId] OR depend_match <=> predicated_depend_match
    // rel(*this, disabled_package_variables[dependentPackageId], BOT_OR, depend_match, version_match);

    rel(*this, disabled_package_variables[dependentPackageId], BOT_OR, depend_match, predicated_depend_match);
    rel(*this, version_match, BOT_IMP, predicated_depend_match, 1);
}

void
VersionProblem::MarkPackageSuspicious(int packageId)
{
    is_suspicious[packageId] = 1;

    if (debugLogging) {
        sprintf(outputBuffer, "%sDepSelector inst# %d - Marking Package Suspicious %d",
                debugPrefix, instance_id, packageId);
        log.Log() << outputBuffer << std::endl;
//        log.Log() << debugPrefix << "DepSelector inst# " << instance_id
//                     << " - Marking Package Suspicious " << packageId << std::endl;
        log.Log().flush();
    }
}

void
VersionProblem::MarkPackageRequired(int packageId)
{
    is_required[packageId] = 1;

    if (debugLogging) {
        sprintf(outputBuffer, "%sDepSelector inst# %d - Marking Package Required %d", debugPrefix, instance_id, packageId);
        log.Log() << debugPrefix << "DepSelector inst# " << instance_id
                     << " - Marking Package Required " << packageId << std::endl;
        log.Log().flush();
    }
}

void
VersionProblem::MarkPackagePreferredToBeAtLatest(int packageId, int weight)
 {
    preferred_at_latest_weights[packageId] = std::max(MAX_PREFERRED_WEIGHT, std::min(0, weight));

    if (debugLogging) {
        sprintf(outputBuffer, "%sDepSelector inst# %d - Marking Package Preferred Latest %d weight %d",
                debugPrefix, instance_id, packageId, weight);
        log.Log() << debugPrefix << "DepSelector inst# " << instance_id
                     << " - Marking Package Preferred Latest " << packageId << " weight " << weight << std::endl;
        log.Log().flush();
    }
}

void VersionProblem::Finalize()
{
    if (debugLogging) {
        log.Log() << debugPrefix << "Finalization Started for inst# " << instance_id << std::endl;
        log.Log().flush();
    }
    finalized = true;

    // Setup constraint for cost
    // We wish to minimize the total number of disabled packages, by priority ranks
    IntArgs disabled_required_weights(size, is_required);
    linear(*this, disabled_required_weights, disabled_package_variables,  IRT_EQ, total_required_disabled);
    if (debugLogging) {
        log.Log() << debugPrefix << "    disabled_required_weights:            " << disabled_required_weights << std::endl;
        log.Log() << debugPrefix << "    total_required_disabled:              " << total_required_disabled << std::endl;
    }

    IntArgs disabled_induced_weights(size);
    for (int i = 0; i < size; i++) {
        disabled_induced_weights[i] = !(is_required[i] || is_suspicious[i]);
    }
    linear(*this, disabled_induced_weights, disabled_package_variables,  IRT_EQ, total_induced_disabled);

    if (debugLogging) {
        log.Log() << debugPrefix << "    disabled_induced_weights:             " << disabled_induced_weights << std::endl;
        log.Log() << debugPrefix <<"    total_induced_disabled:               " << total_induced_disabled << std::endl;
    }

    IntArgs disabled_suspicious_weights(size, is_suspicious);
    linear(*this, disabled_suspicious_weights, disabled_package_variables,  IRT_EQ, total_suspicious_disabled);

    if (debugLogging) {
        log.Log() << debugPrefix << "    disabled_suspicious_weights:          " << disabled_suspicious_weights << std::endl;
        log.Log() << debugPrefix << "    total_suspicious_disabled:            " << total_suspicious_disabled << std::endl;
    }

    linear(*this, disabled_package_variables,  IRT_EQ, total_disabled);
    if (debugLogging) {
        log.Log() <<  debugPrefix <<"    total_disabled:                       " << total_disabled << std::endl;
    }

    // Setup computation for total_preferred_at_latest
    // We wish to maximize the total number of packages at their latest versions in the preferred tier of packages
    // We negate the weights in the cost function to make it fit into the context of a minimization problem.
    for (int i = 0; i < size; i++) {
        preferred_at_latest_weights[i] = -preferred_at_latest_weights[i];
    }
    IntArgs preferred_at_latest_weights_args(size, preferred_at_latest_weights);
    linear(*this, preferred_at_latest_weights_args, at_latest, IRT_EQ, total_preferred_at_latest);
    if (debugLogging) {
        log.Log() << debugPrefix << "    preferred_at_latest_weights_args:     " << preferred_at_latest_weights_args << std::endl;
        log.Log() << debugPrefix << "    total_preferred_at_latest:            " << total_preferred_at_latest << std::endl;
    }

    // Setup computation for remaining variables
    // We wish to maximize the total number of packages at their latest version in the non-preferred tier of packages
    // We negate the weights in the cost function to make it fit into the context of a minimization problem.
    IntArgs not_preferred_at_latest_weights_args = IntArgs::create(size, 0, 0);
    for (int i = 0; i < size; i++) {
        if (preferred_at_latest_weights[i] == 0) {
            not_preferred_at_latest_weights_args[i] = -1;
        }
    }
    linear(*this, not_preferred_at_latest_weights_args, at_latest, IRT_EQ, total_not_preferred_at_latest);
    if (debugLogging) {
        log.Log() << debugPrefix << "    not_preferred_at_latest_weights_args: " << not_preferred_at_latest_weights_args << std::endl;
        log.Log() << debugPrefix << "    total_not_preferred_at_latest:        " << total_not_preferred_at_latest << std::endl;
    }


    // Cleanup
    // Assign a dummy variable to elements greater than actually used.
    for (int i = cur_package; i < size; i++) {
        package_versions[i] = IntVar(*this, -1, -1);
        disabled_package_variables[i] = BoolVar(*this, 1, 1);
    }

#ifdef USE_DUMB_BRANCHING
    if (debugLogging) {
        log.Log() << debugPrefix << "    Adding branching (POOR)" << std::endl;
        log.Log().flush();
    }
    // This branching starts as far as possible from the solution, in order to exercise the optimization functions.
    branch(*this, disabled_package_variables, INT_VAR_SIZE_MIN, INT_VAL_MAX);
    branch(*this, package_versions, INT_VAR_SIZE_MIN, INT_VAL_MIN);
    branch(*this, total_required_disabled, INT_VAL_MAX);
    branch(*this, total_induced_disabled, INT_VAL_MAX);
    branch(*this, total_suspicious_disabled, INT_VAL_MAX);
    branch(*this, total_disabled, INT_VAL_MAX);
    branch(*this, at_latest, INT_VAR_SIZE_MIN, INT_VAL_MIN);
    branch(*this, total_preferred_at_latest, INT_VAL_MIN);
    branch(*this, total_not_preferred_at_latest, INT_VAL_MIN);
#else // USE_DUMB_BRANCHING
    if (debugLogging) {
        log.Log() << debugPrefix << "    Adding branching (BEST)" << std::endl;
        log.Log().flush();
    }
    // This branching is meant to start with most probable solution
    branch(*this, disabled_package_variables, INT_VAR_SIZE_MIN, INT_VAL_MIN);
    branch(*this, package_versions, INT_VAR_SIZE_MIN, INT_VAL_MAX);
    branch(*this, total_required_disabled, INT_VAL_MIN);
    branch(*this, total_induced_disabled, INT_VAL_MIN);
    branch(*this, total_suspicious_disabled, INT_VAL_MIN);
    branch(*this, total_disabled, INT_VAL_MIN);
    branch(*this, at_latest, INT_VAR_SIZE_MIN, INT_VAL_MAX);
    branch(*this, total_preferred_at_latest, INT_VAL_MAX);
    branch(*this, total_not_preferred_at_latest, INT_VAL_MAX);
#endif // USE_DUMB_BRANCHING

    if (debugLogging) {
        log.Log() << debugPrefix << "Finalization Done" << std::endl;
        log.Log().flush();
    }
}

////////////////////////////////////////////////////////////////////////
// A general note about constrain functions
////////////////////////////////////////////////////////////////////////
//
// Constrain functions take a space ('best_known_solution') that is has an assignment of variables
// and operate in the context of a fresh space, not yet fully assigned. Their purpose is to add
// constraints such that the assignments in the fresh space will either yield a better solution, or
// none at all if the best_known_solution is the best possible.
//

#ifdef TOTAL_DISABLED_COST
//
// Very simple constraint function that only minimizes total disabled packages. This is left here
// for debugging purposes. Turn this on to test that the basic system can be solved.
//
void VersionProblem::constrain(const Space & _best_known_solution)
{
    const VersionProblem& best_known_solution = static_cast<const VersionProblem &>(_best_known_solution);

    // add first-level objective function minimization (failing packages, weighted)
    // new constraint: total_disabled < best_known_total_disabled_value)
    int best_known_total_disabled_value = best_known_solution.total_disabled.val();
    rel(*this, total_disabled, IRT_LE, best_known_total_disabled_value);
    if (debugLogging) {
        log.Log() << debugPrefix;
        PrintVarAligned("Con  strain: total_disabled: ", total_disabled);
    }
}
#endif // TOTAL_DISABLED_COST

// _best_known_soln is the most recent satisfying assignment of
// variables that Gecode has found. This method examines the solution
// and adds additional constraints that are applied after restarting
// the search, which means that the next time a solution that's found
// must be strictly better than the current best known solution.
//
// Our model requires us to have a series of objective functions where
// each successive objective function is evaluated if and only if all
// higher precedent objective functions are tied.
//
// [TODO: DESCRIBE WHAT THE ACTUAL SERIES OF OBJECTIVE FUNCTIONS IS]
//
// Lower precedent objective functions are modeled as the consequent
// of an implication whose antecedent is the conjunction of all the
// higher precedent objective functions being assigned to their best
// known value; thus, the optimal value of an objection function
// "activates" the next highest objective function. This has the
// effect of isolating the logic of each objective function such that
// it is only applied to the set of equally preferable solutions under
// the higher precedent objective functions. The objective function
// then applies its constraints, the solution space is restarted and
// walks the space until it finds another, more constrained solution.

#ifdef VECTOR_CONSTRAIN
//
// The vector constrain function assembles multiple cost functions into a vector cost, and then
// constrains the vector cost to be less than the vector cost of the current best_known_solution.
// The less than operation here is a pairwise comparison in order of decreasing precedence; only if
// higher precedence elements are tied will the lower precedence elements be consulted. The elements
// are in increasing order of precedence.
//
// In this case the lowest precedence cost is total_not_preferred_at_latest, followed by total_preferred_at_latest
// and finally total_disabled.
//
void VersionProblem::constrain(const Space & _best_known_solution)
{
    const VersionProblem& best_known_solution = static_cast<const VersionProblem &>(_best_known_solution);

    IntVarArgs current(5);
    IntVarArgs best(5);
    BuildCostVector(current);
    best_known_solution.BuildCostVector(best);
    ConstrainVectorLessThanBest(current, best);
}
#endif // VECTOR_CONSTRAIN

void VersionProblem::BuildCostVector(IntVarArgs & costVector) const {
    costVector[0] = total_not_preferred_at_latest;
    costVector[1] = total_preferred_at_latest;
    costVector[2] = total_suspicious_disabled;
    costVector[3] = total_induced_disabled;
    costVector[4] = total_required_disabled;
}



IntVar & VersionProblem::GetPackageVersionVar(int packageId)
{
    if (packageId < cur_package) {
        return package_versions[packageId];
    } else {
        if (debugLogging) {
            log.Log() << debugPrefix << "Bad package Id " << packageId << " >= " << cur_package << std::endl;
            log.Log().flush();
        }
        //     return 0;
    }
}

int VersionProblem::GetPackageVersion(int packageId)
{
    IntVar & var = GetPackageVersionVar(packageId);
    if (1 == var.size()) return var.val();
    return UNRESOLVED_VARIABLE;
}
bool VersionProblem::GetPackageDisabledState(int packageId)
{
    return disabled_package_variables[packageId].val() == 1;
}

int VersionProblem::GetMax(int packageId)
{
    return GetPackageVersionVar(packageId).max();
}
int VersionProblem::GetMin(int packageId)
{
    return GetPackageVersionVar(packageId).min();
}

int VersionProblem::GetDisabledVariableCount()
{
    if (total_disabled.min() == total_disabled.max()) {
        return total_disabled.min();
    } else {
        return UNRESOLVED_VARIABLE;
    }
}


// Utility
void VersionProblem::Print(std::ostream & out)
{
    out << debugPrefix << "Version problem dump:                   " << cur_package << "/" << size << " packages used/allocated" << std::endl;
    out << debugPrefix << "Disabled Variables:                     " << disabled_package_variables << std::endl;
    out << debugPrefix << "Total Disabled variables (required):    " << total_required_disabled << std::endl;
    out << debugPrefix << "Total Disabled variables: (induced):    " << total_induced_disabled << std::endl;
    out << debugPrefix << "Total Disabled variables: (suspicious): " << total_suspicious_disabled << std::endl;
    out << debugPrefix << "Total Disabled variables:               " << total_disabled << std::endl;
    out << debugPrefix << "at_latest:                              " << at_latest << std::endl;
    out << debugPrefix << "total_preferred_at_latest:              " << total_preferred_at_latest << std::endl;
    out << debugPrefix << "total_not_preferred_at_latest:          " << total_not_preferred_at_latest << std::endl;
    for (int i = 0; i < cur_package; i++) {
        out << debugPrefix << "\t";
        PrintPackageVar(out, i);
        out << std::endl;
    }
    out.flush();
}

// TODO: Validate package ids !

void VersionProblem::PrintPackageVar(std::ostream & out, int packageId)
{
    IntVar & var = GetPackageVersionVar(packageId);
    out << "PackageId: " << packageId <<  " Sltn: " << var << " disabled: " << disabled_package_variables[packageId] << " at latest: " << at_latest[packageId];
}

bool VersionProblem::CheckPackageId(int id)
{
    return (id < size);
}

// We want to sort vectors
// This constrains current to be less than best by a process analogous to subtraction
// we compute current - best, pairwise with borrows from less significant elements. We require it to be less than zero by requiring the most
// significant element to generate a borrow.
//
void VersionProblem::ConstrainVectorLessThanBest(IntVarArgs & current, IntVarArgs & best) {
    BoolVarArray borrow(*this, current.size()+1, 0, 1);

    // No borrows can happen at the least significant element.
    rel(*this, borrow[0], IRT_EQ, 0);

    for (int i = 0; i < current.size(); i++) {
        // If best+borrow is greater than current (equivalently current-(best+borrow) is < 0) then a more significant element
        // must have decreased, so we propagate a borrow to the next most significant element.
        int best_val = best[i].val();
        IntVar delta = expr(*this, current[i] - best_val - borrow[i]);
        // (delta < 0) <=> borrow[i+1]
        rel(*this, delta, IRT_LE, 0, borrow[i+1]);
        if (debugLogging) {
            log.Log() << debugPrefix << "      ConstrainVector: borrow[" << i+1 << "] " << borrow[i+1] << ",\tdelta " << delta << std::endl;
            log.Log() << debugPrefix << "      ConstrainVector: current[" << i << "] " << current[i] << ",\tbest_val " << best_val << std::endl;
        }
    }

    // must borrow off past the most significant element.
    rel(*this, borrow[current.size()], IRT_EQ, 1);
}

VersionProblem * VersionProblem::InnerSolve(VersionProblem * problem, int &itercount)
{
    Gecode::Support::Timer timer;
    timer.start();
    problem->log.Setup();
#ifdef MEMORY_DEBUG
    problem->log.Log() << "Creating solver" << std::endl << std::flush;
#endif
    VersionProblem *best_solution = NULL;
    Restart<VersionProblem> solver(problem);

#ifdef MEMORY_DEBUG
    problem->log.Log() << "Starting Solve" << std::endl << std::flush;
#endif

    while (VersionProblem *solution = solver.next())
        {
#ifdef MEMORY_DEBUG
            problem->log.Log() << "Solver Next " << solution << std::endl << std::flush;
#endif
            if (best_solution != NULL)
                {
                    delete best_solution;
                }
            best_solution = solution;
            ++itercount;
            if (problem->debugLogging) {
                problem->log.Log() << problem->debugPrefix << "Trial Solution #" << itercount << "===============================" << std::endl;
                const Search::Statistics & stats = solver.statistics();
                problem->log.Log() << problem->debugPrefix << "Solver stats: Prop:" << stats.propagate << " Fail:" << stats.fail << " Node:" << stats.node;
                problem->log.Log() << " Depth:" << stats.depth << " memory:" << stats.memory << std::endl;
                solution->Print(problem->log.Log());
            }
        }

    double elapsed_time = timer.stop();

    if (problem->dump_stats) {
        bool solved  = (best_solution != 0);
        const Search::Statistics & final_stats = solver.statistics();
        problem->LogStats(problem->log.Log(), solved, elapsed_time, itercount, final_stats);
        //problem->LogStats(std::cerr, solved, elapsed_time, itercount, final_stats);
    }


    return best_solution;
}

void VersionProblem::LogStats(std::ostream & o, bool solved, double elapsed_time, int itercount,
                              const Search::Statistics & final_stats)
{
    if (debugLogging) o << debugPrefix;
    o << "dep_selector solve: ";
    o << (solved ? "SOLVED" : "FAILED") << " ";
    o << size << " packages, " << version_constraint_count << " constraints, ";
    o << "Time: " << elapsed_time << "ms ";
    o << "Stats: " << itercount << " steps, ";
    o << final_stats.memory << " bytes, ";
    o << final_stats.propagate << " props, " << final_stats.node << " nodes, " << final_stats.depth << " depth ";
    o << std::endl << std::flush;
}



VersionProblem * VersionProblem::Solve(VersionProblem * problem)
{

    SolutionLog log(problem->logId);
    log.Setup();

    problem->Finalize();
    problem->status();

    VersionProblemPool *pool = new VersionProblemPool();
    problem->pool = pool;

    if (problem->debugLogging) {
        log.Log() << problem->DebugPrefix() << "      Before solve" << std::endl;
        problem->Print(log.Log());
    }
    int itercount = 0;

    VersionProblem *best_solution = InnerSolve(problem, itercount);

    if (problem->debugLogging) {
        std::cerr << "XXXXXXXXXXXXXXXXXXXXXXX " << log.use_std_error << std::endl;
        log.Log() << problem->DebugPrefix() << "Solver Best Solution " << best_solution << std::endl << std::flush;
        best_solution->Print(log.Log());
    }

    pool->Delete(best_solution);
    problem->pool = 0;

    pool->DeleteAll();
    delete pool;

    return best_solution;
}

//
// Debug output
//
template <class T> void PrintVarAligned(std::ostream & log, const char * message, T & var)
{
    log.width(40);
    log << std::left << message << var << std::endl;
    log.width(0);
}

//template void PrintVarAligned<int>(const char * message, int & var);



//
// Version Problem
//
//
//
//

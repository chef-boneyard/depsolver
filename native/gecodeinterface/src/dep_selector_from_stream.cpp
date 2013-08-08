//
// Author:: Mark Anderson (<mark@opscode.com>)
// Author:: Marc Paradise (<marc@opscode.com>)
// Copyright:: Copyright (c) 2013-2013 Opscode, Inc.
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

#include "dep_selector_to_gecode.h"
#include "dep_selector_from_stream.h"

#include <iostream>

using namespace std;

void dump_solution(VersionProblem * solution);

VersionProblem * dep_selector_from_stream(std::istream & f) {
    string cmd;
    string guid;
    int dumpStats, debug;
    int packageCount, packageId;
    f >> guid >> packageCount >> dumpStats >> debug;
    VersionProblem *problem = new VersionProblem(packageCount, dumpStats, debug, guid.c_str());
    while (true) {
      f >> cmd;
      if ((f.rdstate() & (std::istream::failbit | std::istream::eofbit) ) > 0) {
        // If input fails, the best we can do is terminate, so that our
        // client can clean up immediately.
        delete problem;
        exit(128);
      }

      if (cmd.compare("P") == 0) {
        int minVersion, maxVersion, currentVersion;
        f >> minVersion >> maxVersion >> currentVersion;
        int id = problem->AddPackage(minVersion, maxVersion, currentVersion);
        if (id == -1) {
          cout << "ERROR" << endl << "already added max packages" << endl;
          return problem;
        }
      } else if (cmd.compare("C") == 0) {
        // TODO here down, make sure we have 'problem'...
        int version, dependentPackageId, minDependentVersion, maxDependentVersion;
        f >> packageId >> version >> dependentPackageId
          >> minDependentVersion >> maxDependentVersion;
        problem->AddVersionConstraint(packageId, version, dependentPackageId,
                                      minDependentVersion, maxDependentVersion);
      } else if (cmd.compare("S") == 0) {
        f >> packageId;
        problem->MarkPackageSuspicious(packageId);
      } else if (cmd.compare("R") == 0) {
        f >> packageId;
        problem->MarkPackageRequired(packageId);
      } else if (cmd.compare("L") == 0) {
        int weight;
        f >> packageId >> weight;
        problem->MarkPackagePreferredToBeAtLatest(packageId, weight);
      } else if (cmd.compare("X") == 0) {
        VersionProblem * ret;
        if (problem->Size() == problem->PackageCount()) {
          VersionProblem * solution = VersionProblem::Solve(problem);
          if (solution) {
            dump_solution(solution);
          } else {
            cout << "NOSOL" << endl;
          }
          // Don't need the original problem anymore.
          delete problem;
          ret = solution;
        } else {
          cout << "ERROR" << endl << "package count did not match expected." << endl;
          ret = problem;
        }
        return ret;
      } else if (cmd.compare("0") == 0) {
        // This means that  our input is j
        // % TODO we should be able to prevent this from occurring.
        // Currently we have
        // no capture of when we time out waiting for a reply from the
        // port - hying to make sure we're
        // reset and ensuring we're not expecting furtherinputs for any
        // command. Ignore it - a reset will be following.
      } else {
        // Any out-of-sequence or unexpected input simply resets our
        // state.  This requires that the caller be aware of this, and
        // manage its state accordingly.
        cout <<  "RESET" << endl;
        return problem;
      }
    }
    return problem;
}

void dump_solution(VersionProblem * solution) {
  int count = solution->PackageCount();
  // Solution start indicator, along with valid indicator.
  cout << "SOL" << endl << solution->GetDisabledVariableCount() << endl;

  for (int id = 0; id < count; id++) {
    // ids are sequential, so there's no reason to dump them - the caller can infer them.
    cout << solution->GetPackageDisabledState(id) << " " << solution->GetPackageVersion(id) << endl;
  }
  // End-of-solution indicator
  cout << "EOS" << endl;
}

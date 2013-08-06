//
// Author:: Mark Anderson (<mark@opscode.com>)
// Author:: Mark Paradise (<marc@opscode.com>)
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


const int BUFSIZE = 1024;

void dump_solution(VersionProblem *);

VersionProblem * dep_selector_from_stream(std::istream & f) {
    string cmd;
    string guid;
    int dumpStats, debug;
    int packageCount, packageId;
    bool replySent = false;
    f >> guid >> packageCount >> dumpStats >> debug;
    cout << "OK" << endl << flush;

    VersionProblem *problem = new VersionProblem(packageCount, dumpStats, debug, guid.c_str());
    while (true) {
      f >> cmd;

      if ((f.rdstate() & (std::istream::failbit | std::istream::eofbit) ) > 0) {
        cout << "ERROR" << endl << "input failure" << endl;
        cout.flush();
        return problem;
      }

      if (cmd.compare("P") == 0) {
        int minVersion, maxVersion, currentVersion;
        f >> minVersion >> maxVersion >> currentVersion;
        int id = problem->AddPackage(minVersion, maxVersion, currentVersion);
        if (id == -1) {
          cout << "ERROR" << endl << "already added max packages" << endl;
          return problem;
        }
        cout << "PID" << endl << id << endl;
        replySent = true;
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
        if (problem->Size() == problem->PackageCount()) {
          VersionProblem * solution = VersionProblem::Solve(problem);
          if (solution) {
            dump_solution(solution);
          } else {
            cout << "NOSOL" << endl;
          }
          return problem;
        } else {
          cout << "ERROR" << endl << "package count did not match expected." << endl;
          return problem;
        }
      } else {
         cout << "ERROR" << endl << "unexpected input: " << cmd << endl;
         return problem;
      }
      if (replySent) {
        replySent = false;
      } else {
        cout << "OK" << endl;
      }
      cout.flush();
    }
    return problem;
}

bool check_state(std::istream & f) {
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
  cout << "X"<< endl;
}

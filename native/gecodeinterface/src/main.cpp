//
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
#include "dep_selector_from_stream.h"
#include "dep_selector_to_gecode.h"
#include <iostream>

using namespace std;

int main(int argc, char * argv[])
{
  // A simple command loop
  // NEW means the next problem is incoming, which has its own expected syntax.
  // EXIT means that we should terminate.
  string cmd;

  while (1) {
    cin >> cmd;
    if ((cin.rdstate() & (std::istream::failbit | std::istream::eofbit) ) > 0) {
      return 0;
    }
    // WE'll possibly want to add some commands to log capture, etc.
    if (cmd.compare("NEW") == 0) {
      VersionProblem * problem = dep_selector_from_stream(cin);
      // We're not doing anything with this for now, so just clean up.
      delete problem;
      cout.flush();
    } else if (cmd.compare("EXIT") == 0) {
      return 0;
    } else {
      cout << "ERROR" << endl << "Unknown command " << cmd << endl;
      cout.flush();
    }

  }
  return 0;
}


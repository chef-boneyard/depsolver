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
#include <unistd.h>
#include <sys/resource.h>

using namespace std;

// Low 1MB limit by default - caller should override.
const int MEM_LIMIT = 1024*1024;

int main(int argc, char * argv[])
{
  // Ensure a runaway solve or bad behavior from the caller (such as
  // adding preposterously high number of constraints) will cause a
  // hard failure instead of consuming all of our memory.
  // TODO do we want to make these limits parameters, configured by
  // the invoker?
  struct rlimit limits;
  if (argc < 2) {
    limits.rlim_max = MEM_LIMIT;
    limits.rlim_cur = MEM_LIMIT;
  } else {
    int limit = atoi(argv[1]);
    limits.rlim_max = limit;
    limits.rlim_cur = limit;
  }
  // NOTE: this limit does NOT appear effective under OS X
  setrlimit(RLIMIT_AS, &limits);

  // A simple command loop
  // NEW means the next problem is incoming, which has its own expected syntax.
  // EXIT means that we should terminate.
  string cmd;
  while (1) {
    cin >> cmd;
    if ((cin.rdstate() & (std::istream::failbit | std::istream::eofbit) ) > 0) {
      return 128;
    }
    // WE'll possibly want to add some commands to log capture, etc.
    if (cmd.compare("NEW") == 0) {
      VersionProblem * problem = dep_selector_from_stream(cin);
      // We're not doing anything with this for now, so just clean up.
      cout.flush();
      delete problem;
    } else if (cmd.compare("EXIT") == 0) {
      return 0;
    } else if (cmd.compare("LEAK") == 0) {
      char * data = new char[1024*1024*100];
      if (!data) {
        cerr << "leak failed";
      } else {
        cerr << "leak succeeded";
      }
    } else if (cmd.compare("HANG") == 0) {
      while (true) {
        sleep(1);
      }
    } else if (cmd.compare("SEGFAULT") == 0) {
      int *x = 0;
      *x = 0;
    } else if (cmd.compare("0") == 0) {
      // Caller is attempting to flush any pending command input. Ignore
      // the noise.
    } else {
      // Any unexpected input simply resets our state.
      cout << "RESET" << endl;
    }

  }
  return 0;
}


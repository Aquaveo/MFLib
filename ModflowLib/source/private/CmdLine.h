//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef CMDLINE_H
#define CMDLINE_H
#include <ostream>

bool ProcessCmdLineArgs(int argc,
                        const char **argv,
                        std::ostream &a_out);

#endif
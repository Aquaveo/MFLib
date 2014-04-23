//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef DBLPREC_H
#define DBLPREC_H

// use "Real" instead of "float" or "double" so switching is easy
#ifndef Real
  #ifdef DBLPREC // DBLPREC would be defined in the project settings
    #define Real double
  #else
    #define Real float
  #endif
#endif

#endif
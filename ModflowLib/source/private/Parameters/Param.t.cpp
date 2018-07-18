//------------------------------------------------------------------------------
// FILE      Param.t.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#ifdef CXX_TEST

#include <private/Parameters/Param.t.h>
#include <private/Parameters/Param.h>

//------------------------------------------------------------------------------
void ParamT::testCreateClass ()
{
  Param *p = new Param();
  TS_ASSERT(p);
  if (p)
  {
    TS_ASSERT(p->m_name == "");
    TS_ASSERT(p->m_type == "");
    TS_ASSERT(p->m_key == 0.0);
    TS_ASSERT(p->m_value == 0.0);
    TS_ASSERT(p->m_min == 0.0);
    TS_ASSERT(p->m_max == 0.0);
    TS_ASSERT(p->m_logMinVal == 0.0);
    TS_ASSERT(p->m_pilotPoints == false);
    TS_ASSERT(p->m_logInterp == false);
    delete(p);
  }
}

#endif

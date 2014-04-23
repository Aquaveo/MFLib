//------------------------------------------------------------------------------
// FILE      ObsHd.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\Packages\ObsHd.h>

//------------------------------------------------------------------------------
/// \brief holds a global variable.
//------------------------------------------------------------------------------
std::vector<HdObs>& GetHOB ()
{
  static std::vector<HdObs> m_hob;
  return m_hob;
} // GetHOB
//------------------------------------------------------------------------------
/// \brief holds a global variable.
//------------------------------------------------------------------------------
FlowObs& GetFLOB ()
{
  static FlowObs m_hob;
  return m_hob;
} // GetFLOB

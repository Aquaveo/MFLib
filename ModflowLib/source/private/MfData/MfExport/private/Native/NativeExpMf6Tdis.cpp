//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Tdis.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMf6Tdis.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>


using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Tdis::NativeExpMf6Tdis (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Tdis::MfNativeExpMf6Tdis
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Tdis::~NativeExpMf6Tdis ()
{
} // MfNativeExpMf6Tdis::~MfNativeExpMf6Tdis
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Tdis::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, comments;
  // get the time units
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;
  // comments
  lines.push_back(MfExportUtil::GetMf6CommentHeader());
  
  // FORMAT

  //BEGIN OPTIONS
  //  [TIME_UNITS <time_units>]
  //  [START_DATE_TIME <start_date_time>]
  //END OPTIONS
  lines.push_back("BEGIN OPTIONS");
  {
    int timeUnit = g->TimeUnit();
    CStr units[6] = { "unknown", "seconds", "minutes", "hours", "days", "years" };
    std::stringstream ss;
    if (timeUnit > -1 && timeUnit < 6)
    {
      ss << " TIME_UNITS " << units[timeUnit];
      lines.push_back(ss.str());
    }
  }
  lines.push_back("END OPTIONS");
  lines.push_back("");

  //BEGIN DIMENSIONS
  //  NPER <nper>
  //END DIMENSIONS
  int nPer = g->NumPeriods();
  lines.push_back("BEGIN DIMENSIONS");
  {
    std::stringstream ss;
    ss << "  NPER " << nPer;
    lines.push_back(ss.str());
  }
  lines.push_back("END DIMENSIONS");
  lines.push_back("");

  //BEGIN PERIODDATA
  //  <perlen> <nstp> <tsmult>
  //  <perlen> <nstp> <tsmult>
  //  ...
  //END PERIODDATA
  using namespace MfData::Packages;
  const Real *perLen(0), *tsMult(0);
  const int *nstps(0);
  if (m_pack->GetPackage()->GetField(DisPack::PERLEN, &perLen) && perLen &&
      m_pack->GetPackage()->GetField(DisPack::NSTP, &nstps) && nstps &&
      m_pack->GetPackage()->GetField(DisPack::TSMULT, &tsMult) && tsMult)
  {
    lines.push_back("BEGIN PERIODDATA");
    for (int i=0; i<nPer; ++i)
    {
      std::stringstream ss;
      ss << "  " << STR(perLen[i]) << " " << nstps[i] << " " << STR(tsMult[i]);
      lines.push_back(ss.str());
    }
    lines.push_back("END PERIODDATA");
  }

  comments.assign(lines.size(), "");
  TmpPackageNameChanger tmp(m_pack->GetPackage(), "TDIS");
  m_pack->AddToStoredLinesDesc(lines, comments);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Tdis::ExportMf6Tdis


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
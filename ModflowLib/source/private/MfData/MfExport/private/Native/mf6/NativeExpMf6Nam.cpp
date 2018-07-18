//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Nam.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Nam.h>

#include <sstream>

#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExporterImpl.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Nam::NativeExpMf6Nam (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Nam::MfNativeExpMf6Nam
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Nam::~NativeExpMf6Nam ()
{
} // MfNativeExpMf6Nam::~MfNativeExpMf6Nam
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Nam::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, comments;  
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;
  Mf2kNative* n = m_pack->GetNative();
  if (!n) return false;

  // comments
  lines.push_back(MfExportUtil::GetMf6CommentHeader());

  CStr baseName;
  util::StripPathFromFilename(n->FileName(), baseName);
  util::StripExtensionFromFilename(baseName, baseName);

  lines.push_back("BEGIN OPTIONS");
  lines.push_back("  LIST " + baseName + ".out");
  lines.push_back("  PRINT_INPUT");
  lines.push_back("  PRINT_FLOWS");
  int saveFlows(0);
  g->GetIntVar("MF6_SAVE_FLOWS", saveFlows);
  if (saveFlows) lines.push_back("  SAVE_FLOWS");
  //If the NWT Solver or the SMS Solver with the NWT option is being used
  if (DoNewton()) lines.push_back("  NEWTON UNDER_RELAXATION"); 
  lines.push_back("END OPTIONS");
  lines.push_back("");

  lines.push_back("BEGIN PACKAGES");  
  std::vector<CStr>& lineRef(m_pack->GetPackage()->StringsToWrite());
  std::vector<CStr>& descRef(m_pack->GetPackage()->StringDescriptions());
  std::vector<CStr> lines1(lineRef);

  lineRef.clear();
  descRef.clear();

  std::vector<CStr> ftype(lines1.size(), ""), fname(lines1.size(), "");
  std::vector<int> unit(lines1.size(), 0);
  for (size_t i=0; i<lines1.size(); ++i)
  {
    std::stringstream ss;
    ss << lines1[i];
    ss >> ftype[i] >> unit[i] >> fname[i];    
  }

  int arraysLayered(1);
  m_pack->GetGlobal()->GetIntVar("ARRAYS_LAYERED", arraysLayered);

  bool chdExists = false;
  for (size_t i=0; i<fname.size(); ++i)
  {
    CStr ft = ftype[i];
    CStr fn = fname[i];
    if (ft == "ETS")
    {
      ft = "EVT";
      util::StripExtensionFromFilename(fn, fn);
      fn += ".evt";
    }
    if (!ValidPackage(ft)) continue;

    if (ft == "DISU" && arraysLayered)
    {
      ft = "DISV";
      util::StripExtensionFromFilename(fn, fn);
      fn += ".disv";
    }
    if (ft == "CHD") chdExists = true;

    util::StripAllButExtension(fn, fn);
    fn = baseName + "." + fn;

    lines.push_back("  " + ft + "6  " + fn);
  }
  lines.push_back("  NPF6 " + baseName + ".npf");
  if (n->GetExp()->AtLeastOneTransientSPExists())
    lines.push_back("  STO6 " + baseName + ".sto");
  lines.push_back("  IC6 " + baseName + ".ic");

  CStr chdStr;
  g->GetStrVar("IBOUND_TO_CHD", chdStr);
  if (!chdExists && !chdStr.empty())
  {
    lines.push_back("  CHD6 " + baseName + ".chd");
    WriteChdFile();
  }

  lines.push_back("END PACKAGES");
  comments.assign(lines.size(), ""); 
  m_pack->AddToStoredLinesDesc(lines, comments);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Nam::ExportMf6Gwfmodelname
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Nam::DoNewton ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, comments;  
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;

  if (g->GetPackage(Packages::NWT)) return true;

  if (g->GetPackage(Packages::SMS))
  {
    MfPackage* p = g->GetPackage(Packages::SMS);

    const int* NONLINMETH(0);
    if (p->GetField(Packages::SmsPack::NONLINMETH, &NONLINMETH) && NONLINMETH)
    {
      if (*NONLINMETH > 0) return true;
    }
  }

  return false;
} // NativeExpMf6Nam::DoNewton
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Nam::ValidPackage (const CStr& a_ftype)
{
  const int NUMTYPES = 21;
  std::string types[NUMTYPES] =
    { "DIS", "DISV", "DISU", "IC", "OC", "NPF", "HFB", "STO", "CHD", "WEL",
      "DRN", "RIV", "GHB", "RCH", "EVT", "MAW", "SFR", "LAK", "UZF", "MVR",
      "GNC" };
  std::set<std::string> setTypes(&types[0], &types[NUMTYPES]);
  if (setTypes.find(a_ftype) != setTypes.end())
  {
    return true;
  }
  CStr msg;
  msg.Format("WARNING: Package %s is not supported for conversion to MF6.", a_ftype);
  printf("%s\n", msg.c_str());
  return false;
} // NativeExpMf6Nam::ValidPackage
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Nam::WriteChdFile ()
{
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return;
  Mf2kNative* n = m_pack->GetNative();
  if (!n) return;
  CStr baseName = n->FileName();
  util::StripExtensionFromFilename(baseName, baseName);
  baseName += ".chd";

  std::vector<CStr> fieldStrings;
  int MAXBOUND(0);
  CStr chdStr = MfExportUtil::Mf6IboundToChd(g, MAXBOUND, fieldStrings);

  std::vector<CStr> lines;

  lines.push_back(MfExportUtil::GetMf6CommentHeader());
  lines.push_back("BEGIN OPTIONS");
  int saveFlows(0);
  if (g->GetIntVar("MF6_SAVE_FLOWS", saveFlows) && saveFlows)
  {
    lines.push_back("  SAVE_FLOWS");
  }
  lines.push_back("END OPTIONS");
  lines.push_back("");

  lines.push_back("BEGIN DIMENSIONS");
  CStr dimStr;
  dimStr.Format("  MAXBOUND %d", MAXBOUND);
  lines.push_back(dimStr);
  lines.push_back("END DIMENSIONS");
  lines.push_back("");

  lines.push_back("BEGIN PERIOD 1");
  lines.push_back(chdStr);
  lines.push_back("END PERIOD 1");

  FILE *fp = fopen(baseName.c_str(), "w");
  if (fp)
  {
    for (size_t i=0; i<lines.size(); ++i)
    {
      fprintf(fp, "%s\n", lines[i].c_str());
    }
    fclose(fp);
  }
} // NativeExpMf6Nam::WriteChdFile

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
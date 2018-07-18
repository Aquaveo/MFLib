//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Sto.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Sto.h>

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
NativeExpMf6Sto::NativeExpMf6Sto (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Sto::MfNativeExpMf6Sto
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Sto::~NativeExpMf6Sto ()
{
} // MfNativeExpMf6Sto::~MfNativeExpMf6Sto
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Sto::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, comments;
  // get the time units
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;
  Mf2kNative* nat = m_pack->GetNative();
  if (!nat) return false;

  if (!nat->GetExp()->AtLeastOneTransientSPExists()) return false;

  // comments
  lines.push_back(MfExportUtil::GetMf6CommentHeader());

  lines.push_back("BEGIN OPTIONS");
  if (SaveFlows())
  {
    g->SetIntVar("MF6_SAVE_FLOWS", 1);
    lines.push_back("  SAVE_FLOWS"); 
  }
  lines.push_back("END OPTIONS"); 
  lines.push_back("");

  int layered(true);
  g->GetIntVar("ARRAYS_LAYERED", layered);
  std::string str;
  lines.push_back("BEGIN GRIDDATA");

  str = "  ICONVERT";
  if (layered) str += " LAYERED";
  lines.push_back(str);
  lines.push_back(GetIconvertLine(g->NumLay()));

  // Storage and yield coefficient code goes here
  str = "  SS";
  if (layered) str += " LAYERED";
  lines.push_back(str);
  lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_LPF_SS));

  str = "  SY";
  if (layered) str += " LAYERED";
  lines.push_back(str);
  lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_LPF_SY));  

  lines.push_back("END GRIDDATA");
  lines.push_back("");

  CStr field = MfData::Packages::DisPack::ISSFLG;
  MfPackage* disPack = g->GetPackage(Packages::DIS);
  if (!disPack)
  {
    disPack = g->GetPackage(Packages::DISU);
    field = MfData::Packages::Disu::ISSFLG;
  }
  ASSERT(disPack);
  if (!disPack) return false;
  const int* issFlag(0);
  disPack->GetField(field, &issFlag);
  ASSERT(issFlag);
  if (!issFlag) return false;

  for(int i=0; i<g->NumPeriods();++i)
  {
    std::stringstream ss;
    ss << "BEGIN PERIOD " << i+1;
    lines.push_back(ss.str());
    if (issFlag[i]) lines.push_back("  STEADY-STATE");
    else            lines.push_back("  TRANSIENT");
    lines.push_back("END PERIOD");
    lines.push_back("");
  }

  comments.assign(lines.size(), "");
  TmpPackageNameChanger tmp(m_pack->GetPackage(), "STO");
  m_pack->AddToStoredLinesDesc(lines, comments);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Sto::ExportMf6Sto
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Sto::SaveFlows ()
{
  const int* ILPFCB(0);
  if (Packages::UPW == m_pack->GetPackage()->PackageName())
  {
    m_pack->GetPackage()->GetField(Packages::UPWpack::IUPWCB, &ILPFCB);
  }
  else
  {
    m_pack->GetPackage()->GetField(Packages::LPFpack::ILPFCB, &ILPFCB);
  }
  if (ILPFCB && *ILPFCB != 0)
    return true;
  return false;
} // NativeExpMf6Sto::SaveFlows
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMf6Sto::GetIconvertLine (int a_numLay)
{
  CStr rval;
  const int* LTYPE(0);
  m_pack->GetPackage()->GetField(Packages::LPFpack::LAYTYP, &LTYPE);
  if (!LTYPE) return rval;

  MfGlobal *g = m_pack->GetGlobal();
  int layered(true);
  g->GetIntVar("ARRAYS_LAYERED", layered);

  if (layered)
  {
    for (int i=0; i<a_numLay; ++i)
    {
      rval += "    ";
      if (LTYPE[i] == 0) rval += "CONSTANT 0";
      else               rval += "CONSTANT -1";
      if (i+1 < a_numLay) rval += "\n";
    }
  }
  else
  {
    Mf2kNative* nat = m_pack->GetNative();
    std::map<CStr, std::vector< std::vector<Real> > >& mymap(nat->SavedRealArrays());
    std::map<CStr, std::vector<Real> >& mymapMult(nat->SavedRealArraysMult());
    //std::map<CStr, std::vector<int> >& mymapJj(nat->SavedRealArraysJj());

    std::vector<std::vector<Real>> origArray(mymap[ARR_LPF_HK]);
    std::vector<Real> origMult(mymapMult[ARR_LPF_HK]);
    std::vector<std::vector<Real>> tArray(mymap[ARR_LPF_HK]);
    std::vector<Real> tMult(mymapMult[ARR_LPF_HK]);

    for (int i=0; i<a_numLay; ++i)
    {
      size_t n = tArray[i].size();
      if (LTYPE[i] == 0) tArray[i].assign(n, 0);
      else               tArray[i].assign(n, -1);
    }
    mymap[ARR_LPF_HK] = tArray;
    tMult.assign(tMult.size(), 1);
    mymapMult[ARR_LPF_HK] = tMult;

    MfPackage* p1 = g->GetPackage(ARR_LPF_HK);
    p1->StringsToWrite().push_back("");
    p1->StringDescriptions().push_back("");
    CStr str = MfExportUtil::GetMf6ArrayString(g, nat, ARR_LPF_HK);
    str.Replace(".0", "");
    rval = str;

    mymap[ARR_LPF_HK] = origArray;
    mymapMult[ARR_LPF_HK] = origMult;
  }
  return rval;
} // NativeExpMf6Sto::GetIconvertLine

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
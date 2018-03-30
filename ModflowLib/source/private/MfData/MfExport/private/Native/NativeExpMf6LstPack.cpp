//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6LstPack.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMf6LstPack.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativeExpLstPack.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>


using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6LstPack::NativeExpMf6LstPack (NativeExpLstPack* a_) :
m_pack(a_)
{
} // MfNativeExpMf6LstPack::MfNativeExpMf6LstPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6LstPack::~NativeExpMf6LstPack ()
{
} // MfNativeExpMf6LstPack::~MfNativeExpMf6LstPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6LstPack::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, comments;
  // get the time units
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;

  if (1 == g->GetCurrentPeriod())
  {
    // comments
    lines.push_back(MfExportUtil::GetMf6CommentHeader());

    lines.push_back("BEGIN OPTIONS");
    lines.push_back(GetAuxLine());
    if (CbFieldExists()) lines.push_back("  SAVE_FLOWS");
    lines.push_back("END OPTIONS");
    lines.push_back("");

    lines.push_back("BEGIN DIMENSIONS");
    lines.push_back(GetMaxBoundLine());
    lines.push_back("END DIMENSIONS");
    lines.push_back("");
  }

  // write stress period data
  const int *itmp(0);
  if (!m_pack->GetPackage()->GetField(Packages::ListPack::ITMP, &itmp) || !itmp || *itmp < 0)
  {
  }
  else
  {
    std::stringstream ss;
    ss << "BEGIN PERIOD " << g->GetCurrentPeriod();
    lines.push_back(ss.str());
    lines.push_back(GetStressPeriodLine(*itmp));
    lines.push_back("END PERIOD");
    lines.push_back("");
  }

  comments.assign(lines.size(), "");
  m_pack->AddToStoredLinesDesc(lines, comments);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6LstPack::ExportMf6Tdis
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMf6LstPack::GetAuxLine ()
{
  CStr rval("  AUXILIARY");
  int start = (int)m_pack->m_fieldStrings.size() - *m_pack->m_nAux;
  for (int i=0; i<*m_pack->m_nAux; ++i)
  {
    rval += " ";
    rval += m_pack->m_fieldStrings[start+i];
  }
  return rval;
} // NativeExpMf6LstPack::GetAuxLine
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6LstPack::CbFieldExists ()
{
  CStr cbField = m_pack->CbFieldName();
  if (cbField.empty()) return false;
  const int* cb(0);
  m_pack->GetPackage()->GetField(cbField, &cb);
  
  if (cb && *cb != 0)
    return true;
  return false;
} // NativeExpMf6LstPack::CbFieldExists
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMf6LstPack::GetMaxBoundLine ()
{
  int maxNumBc(0);
  const int *maxBc(0);
  if (!m_pack->GetPackage()->GetField(Packages::ListPack::MAXBC, &maxBc) || !maxBc)
  {
  }
  else
  {
    maxNumBc = *maxBc;
  }
  std::stringstream ss;
  ss << "  " << maxNumBc;
  return ss.str();
} // NativeExpMf6LstPack::GetMaxBoundLine
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMf6LstPack::GetStressPeriodLine (int itmp)
{
  std::string rval;
  for (int i=0; i<itmp; ++i)
  {
    rval += "  ";
    rval += m_pack->IjkToStr(i);
    for (size_t j=3; j<m_pack->m_fieldStrings.size(); ++j)
    {
      rval += m_pack->DataToStr(i, (int)j);
    }
    if (i+1 < itmp) rval += "\n";
  }
  return rval;
} // NativeExpMf6LstPack::GetStressPeriodLine


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
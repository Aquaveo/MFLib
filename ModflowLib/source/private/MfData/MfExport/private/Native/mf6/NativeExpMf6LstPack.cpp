//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6LstPack.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6LstPack.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativeExpLstPack.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>


using namespace MfData::Export;

class NativeExpMf6LstPack::impl
{
public:
  impl() : m_nChds(0) {}

  void ReadIboundChds (NativeExpLstPack* a_pack)
  {
    m_chdStr = "";
    m_nChds = 0;
    CStr nm = a_pack->GetPackage()->PackageName();
    if (MfData::Packages::CHD == nm)
    {
      m_chdStr = MfExportUtil::Mf6IboundToChd(a_pack->GetGlobal(), m_nChds,
        a_pack->m_fieldStrings);
    }
  } // ReadIboundChds

  CStr m_chdStr;
  int m_nChds;
};
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6LstPack::NativeExpMf6LstPack (NativeExpLstPack* a_) :
m_pack(a_)
, m_p(new impl())
{
} // MfNativeExpMf6LstPack::MfNativeExpMf6LstPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6LstPack::~NativeExpMf6LstPack ()
{
  if (m_p) delete(m_p);
  m_p = nullptr;
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

  m_p->ReadIboundChds(m_pack);

  if (1 == g->GetCurrentPeriod())
  {
    // comments
    lines.push_back(MfExportUtil::GetMf6CommentHeader());

    lines.push_back("BEGIN OPTIONS");
    lines.push_back(GetAuxLine());
    if (CbFieldExists())
    {
      g->SetIntVar("MF6_SAVE_FLOWS", 1);
      lines.push_back("  SAVE_FLOWS");
    }
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
  if (m_p->m_nChds > 0)
    maxNumBc += m_p->m_nChds;
  std::stringstream ss;
  ss << "  MAXBOUND " << maxNumBc;
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

  if (!m_p->m_chdStr.empty())
    rval += ("\n" + m_p->m_chdStr);

  return rval;
} // NativeExpMf6LstPack::GetStressPeriodLine


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
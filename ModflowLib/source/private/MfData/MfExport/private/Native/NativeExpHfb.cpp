//------------------------------------------------------------------------------
// FILE      NativeExpHfb.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpHfb.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
//#include <private\MfData\MfExport\private\Native\NativeUtil.h>
//#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
//#include <private\MfData\Packages\MfPackStrings.h>
#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>

const int VAL_ROW_SIZE=6;
const int HFB_ROW_SIZE = 7;

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpHfb::NativeExpHfb ()
{
} // MfNativeExpHfb::MfNativeExpHfb
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpHfb::~NativeExpHfb ()
{
} // MfNativeExpHfb::~MfNativeExpHfb
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpHfb::Export ()
{
  AddToStoredLinesDesc(Line1(), Desc(1));
  Lines2and3();
  std::vector<CStr> l4 = Line4();
  AddToStoredLinesDesc(l4, DescVec(4, (int)l4.size()));
  if (NumPar() > 0)
  {
    CStr l5;
    l5.Format("%d", NumPar());
    AddToStoredLinesDesc(l5, Desc(5));
    std::vector<CStr> l6 = Line6();
    AddToStoredLinesDesc(l6, DescVec(6, (int)l6.size()));
  }
  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpHfb::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int NativeExpHfb::NumPar ()
{
  std::map<CStr, std::vector<Real> >& par=GetNative()->GetExp()->HfbParData();
  return (int)par.size();
} // NativeExpHfb::NumPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHfb::Desc (int a_line)
{
  CStr rval[6] = {" 1. NPHFB MXFB NHFBNP",
                  " 2. PARNAM PARTYP Parval NLST",
                  " 3. Layer IROW1 ICOL1 IROW2  ICOL2 Factor",
                  " 4. Layer IROW1 ICOL1 IROW2 ICOL2 Hydchr",
                  " 5. NACTHFB",
                  " 6. Pname"};
  return rval[a_line-1];
} // MfNativeExpHfb::Desc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpHfb::DescVec (int a_line, int a_num)
{
  std::vector<CStr> rval(a_num, Desc(a_line));
  return rval;
} // NativeExpHfb::DescVec
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHfb::Line1 ()
{ //NPHFB MXFB NHFBNP Option
  CStr rval;

  const int VAL_ROW_SIZE=6;
  const int* nhfbnp(0);
  if (GetPackage()->GetField(Packages::HFBpack::NHFBNP, &nhfbnp) && nhfbnp)
  {
    std::map<CStr, std::vector<Real> >& par=GetNative()->GetExp()->HfbParData();
    int mxfb(0), nphfb = (int)par.size();
    std::map<CStr, std::vector<Real> >::iterator it(par.begin());
    for (; it != par.end(); ++it)
    {
      mxfb += (int)it->second.size()/VAL_ROW_SIZE;
    }
    rval.Format("%d %d %d", nphfb, mxfb, *nhfbnp);
  }

  return rval;
} // NativeExpHfb::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpHfb::Lines2and3 ()
{
  int nPar = NumPar();
  if (nPar < 1) return;

  // 2. PARNAM PARTYP Parval NLST
  // 3. Layer IROW1 ICOL1 IROW2 ICOL2 Factor
  std::map<CStr, std::vector<Real> >& hPar=GetNative()->GetExp()->HfbParData();
  std::map<CStr, std::vector<Real> >::iterator it = hPar.begin();
  for (; it != hPar.end(); ++it)
  {
    CStr pname = it->first;
    std::vector<Real>& values = it->second;
    size_t nlst = values.size()/VAL_ROW_SIZE;

    ParamList *list(0);
    Parameters::GetParameterList(&list);
    Param p;
    if (list->FindByName(pname.c_str(), &p))
    {
      // Line 2: PARNAM PARTYP Parval NLST
      MfExportUtil::InsertSingleQuotesInName(pname);
      // use m_value in case there is a SEN file with a different value
      // than the parameter value specified in the package file
      CStr s;
      s.Format("%s %s %s %d", pname, p.m_type, STR(p.m_value), 
                nlst);
      AddToStoredLinesDesc(s, Desc(2));
      for (size_t i = 0; i < nlst; ++i)
      {
        int lay = static_cast<int>(values[(i*VAL_ROW_SIZE)+0]);
        int row1 = static_cast<int>(values[(i*VAL_ROW_SIZE)+1]);
        int col1 = static_cast<int>(values[(i*VAL_ROW_SIZE)+2]);
        int row2 = static_cast<int>(values[(i*VAL_ROW_SIZE)+3]);
        int col2 = static_cast<int>(values[(i *VAL_ROW_SIZE)+4]);
        Real factor = values[(i*VAL_ROW_SIZE)+5];

        // Line 3: Layer IROW1 ICOL1 IROW2 ICOL2 Factor
        s.Format("%d %d %d %d %d %s ", lay, row1, col1, row2, col2,
                                        STR(factor));
        AddToStoredLinesDesc(s, Desc(3));
      }
    }
  }
} // NativeExpHfb::Lines2and3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpHfb::Line4 ()
{
  std::vector<CStr> rval;

  const Real *hfb(0);
  const int* nhfbnp(0);
  if (!GetPackage()->GetField(Packages::HFBpack::NHFBNP, &nhfbnp) || !nhfbnp ||
      !GetPackage()->GetField(Packages::HFBpack::HFB, &hfb) || !hfb)
    return rval;

  for (int i = 0; i < *nhfbnp; ++i)
  {
    int lay = static_cast<int>(hfb[(i*HFB_ROW_SIZE)+0]);
    int row1 = static_cast<int>(hfb[(i*HFB_ROW_SIZE)+1]);
    int col1 = static_cast<int>(hfb[(i*HFB_ROW_SIZE)+2]);
    int row2 = static_cast<int>(hfb[(i*HFB_ROW_SIZE)+3]);
    int col2 = static_cast<int>(hfb[(i *HFB_ROW_SIZE)+4]);
    Real hydc = hfb[(i*HFB_ROW_SIZE)+5];

    // Line 4: Layer IROW1 ICOL1 IROW2 ICOL2 Hydchr
    CStr s;
    s.Format("%d %d %d %d %d %s ", lay, row1, col1, row2, col2, STR(hydc));
    rval.push_back(s);
  }

  return rval;
} // NativeExpHfb::Lines4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpHfb::Line6 ()
{
  std::vector<CStr> rval;

  std::map<CStr, std::vector<Real> >& hPar=GetNative()->GetExp()->HfbParData();
  std::map<CStr, std::vector<Real> >::iterator it = hPar.begin();
  for (; it != hPar.end(); ++it)
  {
    CStr pname = it->first;
    MfExportUtil::InsertSingleQuotesInName(pname);
    rval.push_back(pname);
  }
  return rval;
} // NativeExpHfb::Line6
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpHfb.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpHfbT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::HFB);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpHfb*>(p);
} // NativeExpHfbT::setUp
//------------------------------------------------------------------------------
void NativeExpHfbT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpHfbT::tearDown
//------------------------------------------------------------------------------
void NativeExpHfbT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpHfbT::testCreateClass

#endif
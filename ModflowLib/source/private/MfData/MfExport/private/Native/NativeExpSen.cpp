//------------------------------------------------------------------------------
// FILE      NativeExpSen.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpSen.h>

#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/Parameters.h>
#include <private/Parameters/Param.h>
#include <private/Parameters/ParamList.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSen::NativeExpSen ()
{
} // MfNativeExpSen::MfNativeExpSen
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSen::~NativeExpSen ()
{
} // MfNativeExpSen::~MfNativeExpSen
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSen::Export ()
{
  return true;
} // MfNativeExpSen::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSen::Line1 ()
{
  using namespace MfData::Packages;
  CStr rval;
  const int *isenall, *iuhead, *imxsen, *nplist;
  MfPackage* a_pSen = GetGlobal()->GetPackage(Packages::SEN);
  MfPackage* a_pSen1 = GetPackage();
  if (a_pSen->GetField(SENpack::ISENALL, &isenall) && isenall &&
      a_pSen->GetField(SENpack::IUHEAD, &iuhead) && iuhead &&
      a_pSen->GetField(SENpack::MXSEN, &imxsen) && imxsen &&
      a_pSen1->GetField(SEN1pack::NPLIST, &nplist) && nplist)
  {
    int np = (int)Line3().size();
    rval.Format("%d %d %d %d", np, *isenall, *iuhead, *imxsen);
  }
  return rval;
} // NativeExpSen::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSen::Desc1 ()
{
  CStr rval = " 1. NPLIST ISNEALL IUHEAD MXSEN";
  return rval;
} // NativeExpSen::Desc1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSen::Line2 ()
{
  using namespace MfData::Packages;
  CStr rval;
  const int *iprints, *isensu, *isenpu, *isenfm;
  MfPackage* a_pSen = GetGlobal()->GetPackage(Packages::SEN);
  if (a_pSen->GetField(SENpack::IPRINTS, &iprints) && iprints &&
      a_pSen->GetField(SENpack::ISENSU, &isensu) && isensu &&
      a_pSen->GetField(SENpack::ISENPU, &isenpu) && isenpu &&
      a_pSen->GetField(SENpack::ISENFM, &isenfm) && isenfm)
  {
    rval.Format("%d %d %d %d", *iprints, *isensu, *isenpu, *isenfm);
  }
  return rval;
} // NativeExpSen::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSen::Desc2 ()
{
  CStr rval = " 2. IPRINTS ISENSU ISENPU ISENFM";
  return rval;
} // NativeExpSen::Desc2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpSen::Line3 ()
{
  std::vector<CStr> rval;
  using namespace MfData::Packages;
  const int *nplist, *isens, *ln;
  const char *parnam;
  const Real *b, *bl, *bu, *bscal;
  ParamList *list;
  Parameters::GetParameterList(&list);

  MfPackage* a_pSen1 = GetPackage();
  if (a_pSen1->GetField(SEN1pack::NPLIST, &nplist) && nplist &&
      a_pSen1->GetField(SEN1pack::PARNAM, &parnam) && parnam &&
      a_pSen1->GetField(SEN1pack::ISENS, &isens) && isens &&
      a_pSen1->GetField(SEN1pack::LN, &ln) && ln &&
      a_pSen1->GetField(SEN1pack::B, &b) && b &&
      a_pSen1->GetField(SEN1pack::BL, &bl) && bl &&
      a_pSen1->GetField(SEN1pack::BU, &bu) && bu &&
      a_pSen1->GetField(SEN1pack::BSCAL, &bscal) && bscal)
  {
    ParamList *list(0);
    Parameters::GetParameterList(&list);
    Param p;

    for (int i = 0; i < *nplist; ++i)
    {
      CStr parnamStr(parnam + i*10, 10);
      parnamStr.Trim();
      if (SkipPar_Pval_Sen(parnamStr)) continue;

      Real bValue;
      if (list->FindByName(parnamStr.c_str(), &p))
        bValue = (Real)p.m_b;
      else
        bValue = b[i];

      CStr line;
      line.Format("%s %d %d %s %s %s %s", parnamStr, isens[i], ln[i],
                  STR(bValue), STR(bl[i]), STR(bu[i]), STR(bscal[i]));
      rval.push_back(line);

      Param p;
      if (list->FindByName(parnamStr.c_str(), &p))
      {
        p.m_bscal = bscal[i];
        list->UpdateParameter(&p);
      }
    }
  }
  return rval;
} // NativeExpSen::Line3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpSen::Desc3 ()
{
  std::vector<CStr> rval(Line3().size(), " 3. PARNAM ISENS LN B BL BU BSCAL");
  return rval;
} // NativeExpSen::Desc3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSen::LastChanceBeforeWriting ()
{
  if (Line3().empty()) return;
  AddToStoredLinesDesc(Line1(), Desc1());
  AddToStoredLinesDesc(Line2(), Desc2());
  AddToStoredLinesDesc(Line3(), Desc3());

  TmpPackageNameChanger tmp(GetPackage(), "SEN");
  WriteComments();
  WriteStoredLines();
  GetGlobal()->SetIntVar("SEN_Exported", 1);
} // NativeExpSen::LastChanceBeforeWriting

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpSen.t.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpSenT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::SEN1);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpSen*>(p);
} // NativeExpSenT::setUp
//------------------------------------------------------------------------------
void NativeExpSenT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpSenT::tearDown
//------------------------------------------------------------------------------
void NativeExpSenT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpSenT::testCreateClass

#endif
//------------------------------------------------------------------------------
// FILE      NativeExpGnc.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpGnc.h>

#include <sstream>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpGnc::NativeExpGnc ()
: NativePackExp()
{
} // MfNativeExpGnc::MfNativeExpGnc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpGnc::~NativeExpGnc ()
{
} // MfNativeExpGnc::~MfNativeExpGnc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpGnc::Export ()
{
  Line1();
  Line4();

  WriteComments();
  WriteStoredLines();

  return true;
} // MfNativeExpGnc::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpGnc::Line1 ()
{
  CStr desc = " 1. NPGNCn MXGNn NGNCNPn MXADJn I2Kn ISYMGNCn IFLALPHAn [NOPRINT]";

  const int *npgncn(0), *mxgnn(0), *ngncnpn(0), *mxadjn(0), *i2kn(0),
            *isymgncn(0), *iflalphan(0), *iprgncn(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::GNCpack::NPGNCn, &npgncn) || !npgncn ||
      !a_p->GetField(Packages::GNCpack::MXGNn, &mxgnn) || !mxgnn ||
      !a_p->GetField(Packages::GNCpack::NGNCNPn, &ngncnpn) || !ngncnpn ||
      !a_p->GetField(Packages::GNCpack::MXADJn, &mxadjn) || !mxadjn ||
      !a_p->GetField(Packages::GNCpack::I2Kn, &i2kn) || !i2kn ||
      !a_p->GetField(Packages::GNCpack::ISYMGNCn, &isymgncn) || !isymgncn ||
      !a_p->GetField(Packages::GNCpack::IFLALPHAn, &iflalphan) || !iflalphan ||
      !a_p->GetField(Packages::GNCpack::IPRGNCn, &iprgncn) || !iprgncn)
    return;

  CStr ln;
  ln.Format("%5d %5d %5d %5d %5d %5d %5d", *npgncn, *mxgnn, *ngncnpn, *mxadjn,
            *i2kn, *isymgncn, *iflalphan);
  if (iprgncn == 0) {
    ln += " NOPRINT";
  }
  AddToStoredLinesDesc(ln, desc);
} // NativeExpGnc::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpGnc::Line4 ()
{
  using util::ForElement;

  CStr desc = " 4. NodeN NodeM (NodeJ, J=1,MXADJn) (AlphaJ, J=1,MXADJn) AlphaN";
  const int *n1(0), *n2(0), *iflalphan(0);
  const Real *gnc(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::GNCpack::N1, &n1) || !n1 ||
      !a_p->GetField(Packages::GNCpack::N2, &n2) || !n2 ||
      !a_p->GetField(Packages::GNCpack::GNCn, &gnc) || !gnc ||
      !a_p->GetField(Packages::GNCpack::IFLALPHAn, &iflalphan) || !iflalphan)
    return;

  int width = util::RealWidth();
  CStr s1,s2;
  for (int i = 1; i <= *n2; ++i) {
    int nodeN = static_cast<int>(ForElement(gnc,1,i,*n1));
    int nodeM = static_cast<int>(ForElement(gnc,2,i,*n1));
    int nodeJ = static_cast<int>(ForElement(gnc,3,i,*n1));
    Real alphaJ =                ForElement(gnc,4,i,*n1);
    s1.Format("%5d%5d%5d %s",nodeN, nodeM, nodeJ,
                             STR(alphaJ,-1,width,STR_FULLWIDTH));
    for (int j = 5; j < *n1; j += 2) {
      nodeJ = static_cast<int>(ForElement(gnc,j,  i,*n1));
      alphaJ =                 ForElement(gnc,j+1,i,*n1);
      s2.Format(" %5d %s",nodeJ, STR(alphaJ,-1,width,STR_FULLWIDTH));
      s1 += s2;
    }
    if (*iflalphan == 1) {
      Real alphaN = ForElement(gnc,*n1,i,*n1);
      s2.Format(" %s",STR(alphaN,-1,width,STR_FULLWIDTH));
      s1 += s2;
    }
    AddToStoredLinesDesc(s1.c_str(), (i == 1 ? desc : ""));
  }
} // NativeExpGnc::Line4


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpGnc.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpGncT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::RCH);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpGnc*>(p);
} // NativeExpGncT::setUp
//------------------------------------------------------------------------------
void NativeExpGncT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpGncT::tearDown
//------------------------------------------------------------------------------
void NativeExpGncT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpGncT::testCreateClass

#endif
//------------------------------------------------------------------------------
// FILE      NativeExpMnwi.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMnwi.h>

//#include <private\MfData\MfExport\private\Native\NativeExpNam.h>
//#include <private\MfData\MfExport\private\Native\NativeUtil.h>
//#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
//#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMnwi::NativeExpMnwi ()
{
} // MfNativeExpMnwi::MfNativeExpMnwi
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMnwi::~NativeExpMnwi ()
{
} // MfNativeExpMnwi::~MfNativeExpMnwi
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMnwi::Export ()
{
  CStr ln = GetPackage()->GetLineNumber();

  if ("1" == ln)      Line1();
  else if ("2" == ln) Line2();
  else if ("3" == ln) Line3();
  else if ("end" == ln)
  {
    WriteComments();
    WriteStoredLines();
  }
  return true;
} // MfNativeExpMnwi::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnwi::Line1 ()
{
  using namespace MfData::Packages;
  const int* Wel1flag(0);
  const int* QSUMflag(0);
  const int* BYNDflag(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(MNWIpack::Wel1flag, &Wel1flag) || !Wel1flag ||
      !a_p->GetField(MNWIpack::QSUMflag, &QSUMflag) || !QSUMflag ||
      !a_p->GetField(MNWIpack::BYNDflag, &BYNDflag) || !BYNDflag)
  {
    ASSERT(0);
    return;
  }

  CStr line;
  line.Format("%d %d %d", *Wel1flag, *QSUMflag, *BYNDflag);
  CStr desc = "1. Wel1flag,QSUMflag,BYNDflag";
  AddToStoredLinesDesc(line, desc);
} // NativeExpMnwi::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnwi::Line2 ()
{
  using namespace MfData::Packages;
  const int* MNWOBS(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(MNWIpack::MNWOBS, &MNWOBS) || !MNWOBS)
  {
    ASSERT(0);
    return;
  }

  CStr desc = "2. MNWOBS";
  CStr line;
  line.Format("%d", *MNWOBS);
  AddToStoredLinesDesc(line, desc);
} // NativeExpMnwi::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnwi::Line3 ()
{
  using namespace MfData::Packages;
  const char* WELLID(0);
  const int* UNIT(0);
  const int* QNDflag(0);
  const int* QBHflag(0);
  const int* CONCflag(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(MNWIpack::WELLID, &WELLID) || !WELLID ||
      !a_p->GetField(MNWIpack::UNIT, &UNIT) || !UNIT ||
      !a_p->GetField(MNWIpack::QNDflag, &QNDflag) || !QNDflag ||
      !a_p->GetField(MNWIpack::QBHflag, &QBHflag) || !QBHflag ||
      !a_p->GetField(MNWIpack::CONCflag, &CONCflag) || !CONCflag)
  {
    ASSERT(0);
    return;
  }

  CStr line, wellid(WELLID);
  if (wellid.find(" ") != -1)
  {
    wellid.Format("'%s'", WELLID);
  }
  line.Format("%s %d %d %d %d", wellid, *UNIT, *QNDflag, *QBHflag, *CONCflag);
  CStr desc = "3. WELLID UNIT QNDflag QBHflag CONCflag";
  AddToStoredLinesDesc(line, desc);
} // NativeExpMnwi::Line3

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpMnwi.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpMnwiT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::MNWI);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpMnwi*>(p);
} // NativeExpMnwiT::setUp
//------------------------------------------------------------------------------
void NativeExpMnwiT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpMnwiT::tearDown
//------------------------------------------------------------------------------
void NativeExpMnwiT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpMnwiT::testCreateClass

#endif
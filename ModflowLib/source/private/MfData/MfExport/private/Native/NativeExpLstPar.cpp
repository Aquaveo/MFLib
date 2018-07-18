//------------------------------------------------------------------------------
// FILE      NativeExpLstPar.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpLstPar.h>

#include <private/MfData/MfExport/private/H5/H5BcList.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfPackageUtil.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLstPar::NativeExpLstPar ()
{
} // MfNativeExpLstPar::MfNativeExpLstPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLstPar::~NativeExpLstPar ()
{
} // MfNativeExpLstPar::~MfNativeExpLstPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpLstPar::Export ()
{
  CStr                listPack;
  if (!MfData::Packages::GetPackNameFromParameter(GetPackage(), listPack))
    return false;

  if (listPack.CompareNoCase(MfData::Packages::HFB) == 0)
  {
    MfData::Packages::SaveHfbParameterData(GetPackage(),
                                           GetNative()->GetExp()->HfbParData());
    return true;
  }

  if (GetH5Flag())
  {
    H5BcList h(this);
    h.LstPar();
  }
#if 0
  CStr                type, listPack, fileType(GetPackage()->PackageName());
  if (!MfData::Packages::GetPackNameFromParameter(a_p, listPack))
    return false;

  //if (listPack.CompareNoCase(MfData::Packages::HFB) == 0)
  //{
  //  saveHFBParameterData(a_p, a_exp);
  //  return;
  //}

  // get the list BC data
  const int          *nBcs, *nAux, *nDataFields;
  int                 nFields;
  const Real*         data;
  std::vector<CStr>   fieldStrings;
  MfData::Packages::GetBcData(a_p, listPack, &nBcs, &nFields,
                              &nAux, &data, &nDataFields, fieldStrings);

  // get some info about the parameter
  int                 start;
  Real                key;
  if (!MfData::Packages::GetParamKeyAndDataStart(GetPackage(), key, start))
    return;

  // first create a vector of indices so we can size the data array
  int i, maxIdx(-1), cellId, ci, cj, ck;

  for (i=start; i<(*nBcs+start); i++)
  {
    ck = static_cast<int>(data[i*(*nDataFields)+0]);
    ci = static_cast<int>(data[i*(*nDataFields)+1]);
    cj = static_cast<int>(data[i*(*nDataFields)+2]);
  }

  int bcIdx, fieldIdx, dataIdx;
  std::map<int, int> srcDestIdxs;
  MfData::Packages::GetParamSrcDestFields(listPack, fieldStrings, srcDestIdxs);
  // fill in the bcData
  int j, nBcFields = nFields - *nAux - 3; // k,i,j
  for (i=start; i<(*nBcs+start); i++)
  {
    for (j=0; j<nBcFields; j++)
    {
      bcIdx = idxs.at(i-start) - minIdx;
      dataIdx = i*(*nDataFields)+3+j;
      fieldIdx = j;
      if (srcDestIdxs.find(j+3) != srcDestIdxs.end())
        fieldIdx = srcDestIdxs[j+3];
      bcData.at(fieldIdx, bcIdx) = static_cast<double>(data[dataIdx]);
      if (j != fieldIdx)
        bcData.at(j, bcIdx) = key;
    }
  }
#endif
  return true;
} // NativeExpLstPar::Export
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpLstPar.t.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpLstParT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::LPRM);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpLstPar*>(p);
} // NativeExpLstParT::setUp
//------------------------------------------------------------------------------
void NativeExpLstParT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpLstParT::tearDown
//------------------------------------------------------------------------------
void NativeExpLstParT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpLstParT::testCreateClass

#endif
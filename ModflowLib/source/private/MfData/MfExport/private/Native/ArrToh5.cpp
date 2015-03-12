//------------------------------------------------------------------------------
// FILE      NativeExpArrToh5.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private\MfData\MfExport\private\Native\ArrToh5.h>

// 3. Standard library headers

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/H5DataReader/H5DataSetWriter.h>
#include <private/H5DataReader/H5DataSetWriterSetup.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
ArrToh5::ArrToh5 (NativePackExp* a_) :
  m_pack(a_)
{
} // ArrToh5::ArrToh5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
ArrToh5::~ArrToh5 ()
{

} // ArrToh5::~ArrToh5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ArrToh5::WriteData ()
{
  bool compress(m_pack->GetNative()->Compress());
  CStr rval, fname(H5Filename()), path(H5Path());
  hid_t datatype((hid_t)GetDataType());
  int nDim(GetNumDim());

  H5DataSetWriterSetup setup(fname, path, datatype, nDim, compress);
  H5DataSetWriter h(&setup);
  h.AllowTypeConversions(true);

  CStr multStr;
  int tmp_iMult, nVal(GetNumValsToWrite());
  MfPackage* p(m_pack->GetPackage());
  const double* dData(0);
  const Real*   rData(0), *rMult(0);
  const int*    iData(0), *iMult(0);
  p->GetField(Packages::Array::MULT, &rMult);
  p->GetField(Packages::Array::MULT, &iMult);
  p->GetField(Packages::Array::ARRAY, &rData);
  p->GetField(Packages::Array::ARRAY, &dData);
  p->GetField(Packages::Array::ARRAY, &iData);

  if ((dData || rData) && rMult)
  {
    if (rData) h.WriteData(rData, nVal);
    else       h.WriteData(dData, nVal);
    multStr = STR(*rMult);
  }
  else 
  {
    if (!iMult && rMult)
    {
      tmp_iMult = (int)(*rMult);
      iMult = &tmp_iMult;
      rMult = nullptr;
    }
    if (iData && iMult)
    {
      h.WriteData(iData, nVal);
      multStr = STR((float)*iMult);
    }
    else ASSERT(0);
  }

  const int* iPRN(0);
  int prn(-1);
  p->GetField(Packages::Array::IPRN, &iPRN);
  if (iPRN) prn = *iPRN;

  CStr f1;
  util::StripPathFromFilename(fname, f1);
  rval.Format("HDF5 %s %d \"%s\" \"%s\" ", multStr, prn, f1, path);
  CStr s;
  s.Format("1 0 %d", nVal);
  rval += s;
  return rval;
} // ArrToh5::WriteData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ArrToh5::H5Filename ()
{
  Mf2kNative* n = m_pack->GetNative();
  CStr rval = n->GetExp()->GetBaseFileName();
  rval += ".h5";
  return rval;
} // ArrToh5::H5Filename
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ArrToh5::H5Path ()
{
  std::map<CStr, CStr> amap(m_pack->GetNative()->GetMapArrays());
  MfPackage* p(m_pack->GetPackage());
  CStr path, packName(p->PackageName());
  const int* layer(0);
  p->GetField(Packages::Array::LAYER, &layer);
  if (layer) path.Format("Arrays/%s%d", amap.find(packName)->second, *layer);
  return path;
} // ArrToh5::H5Path
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int ArrToh5::GetDataType ()
{
  int rval = (int)H5T_NATIVE_DOUBLE;
  MfPackage* p(m_pack->GetPackage());
  const int* iData(0);
  p->GetField(Packages::Array::ARRAY, &iData);
  if (iData) rval = (int)H5T_NATIVE_INT;
  return rval;
} // ArrToh5::GetDataType
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int ArrToh5::GetNumDim ()
{
  return 1;
} // ArrToh5::GetNumDim 
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int ArrToh5::GetNumValsToWrite ()
{
  int nrow = m_pack->GetGlobal()->NumRow();
  int ncol = m_pack->GetGlobal()->NumCol();
  if (m_pack->GetGlobal()->Unstructured())
  {
    nrow = 1;
    const int* JJ(nullptr);
    if (!m_pack->GetPackage()->GetField("JJ", &JJ) || !JJ)
    {
      ASSERT(0);
      return false;
    }
    ncol = *JJ;
  }
  return nrow*ncol;
} // ArrToh5::GetNumValsToWrite 








//------------------------------------------------------------------------------
// FILE      H5ArrayWriter.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private\MfData\MfExport\private\Native\H5ArrayWriter.h>

// 3. Standard library headers

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/H5DataReader/H5DataSetWriter.h>
#include <private/H5DataReader/H5DataSetWriterSetup.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------
class H5ArrayWriter::impl
{
public:
  impl(NativePackExp* a_);
  ~impl();

  CStr WriteData();
  bool ForceToH5File();

  void SetH5DimensionsForWriting(H5DataSetWriter& a_h);
  CStr FileStringRefToh5Data(CStr fname, CStr path);
  CStr H5Filename();
  CStr H5Path();
  void GetPackageData();
  void WriteH5DataSet(H5DataSetWriter* a_h);
  int  GetDataType();
  int  GetNumDim();
  int  GetNumValsToWrite();
  int  GetSubPackageArrayIndex();
  void WriteMultiplierToh5();
  int  GetMultiplierNumDim();
  int  GetMultiplierIdx0();
  int  GetMultiplierIdx1();


  NativePackExp* m_pack;
  const double*  m_dData;
  const Real    *m_rData, *m_rMult;
  const int     *m_iData, *m_iMult, *m_iPRN;
  int            m_nVal, m_tmp_iMult;
  CStr           m_multStr, m_dimStr;
  std::vector<hsize_t> m_start, m_n2write;

};

//------------------------------------------------------------------------------
namespace
{
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool iSubPackData (const CStr& a_)
{
  if (a_.Find("SUB/") != -1) return true;
  return false;
} // iSubPackData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool iRchEtEtsPackData (const CStr& a_)
{
  if (   a_.Find("Recharge/") != -1
      || a_.Find("ET/") != -1
      || a_.Find("ETS/") != -1
     ) return true;
  return false;
} // iRchEtEtsPackData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool iRchEtEtsLayerData (const CStr& a_)
{
  if (   iRchEtEtsPackData(a_)
      && a_.Find("09. Layer") != -1) return true;
  return false;
} // iRchEtEtsLayerData
//------------------------------------------------------------------------------
/// \brief Gets the index of a multidimensional array like RCH or ET so we know
/// where to write the data
//------------------------------------------------------------------------------
static int iPackNameToArrayIndex (const CStr &a_name)//,
//                                  TxtExporter* a_exp,
//                                  int a_sp /* = 0*/)
{
  int rval(0);
  if (a_name.CompareNoCase(ARR_EVT_RATE) == 0 ||
      a_name.CompareNoCase(ARR_ETS_RATE) == 0 ||
      a_name.CompareNoCase(ARR_UZF_ET) == 0)
    rval = 1;
  else if (a_name.CompareNoCase(ARR_EVT_EXT) == 0 ||
           a_name.CompareNoCase(ARR_ETS_EXT) == 0 ||
           a_name.CompareNoCase(ARR_UZF_EXT) == 0)
    rval = 2;
  else if (a_name.CompareNoCase(ARR_UZF_EXTWC) == 0)
    rval = 3;
  //else if (a_name.CompareNoCase(ARR_ETS_PXDP) == 0 ||
  //         a_name.CompareNoCase(ARR_ETS_PETM) == 0 ||
  //         a_name.CompareNoCase(ARR_VDF_DENS) == 0 ||
  //         a_name.CompareNoCase(ARR_VDF_CONC) == 0 ||
  //         a_name.CompareNoCase(ARR_VSC_VSC) == 0 ||
  //         a_name.CompareNoCase(ARR_VSC_CONC) == 0)
  //  rval = iGetIncrementedArrayIndex(a_name, a_sp, a_exp);
  return rval;
} // iPackNameToArrayIndex
} // unnamed namespace


//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5ArrayWriter::H5ArrayWriter (NativePackExp* a_) :
  m_p(new impl(a_))
{
} // H5ArrayWriter::H5ArrayWriter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5ArrayWriter::~H5ArrayWriter ()
{
  if (m_p) delete(m_p);
} // H5ArrayWriter::~H5ArrayWriter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5ArrayWriter::WriteData ()
{
  return m_p->WriteData();
} // H5ArrayWriter::WriteData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool H5ArrayWriter::ForceToH5File ()
{
  return m_p->ForceToH5File();
} // H5ArrayWriter::ForceToH5File
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5ArrayWriter::impl::impl (NativePackExp* a_) :
  m_pack(a_)
, m_dData(nullptr)
, m_rData(nullptr)
, m_rMult(nullptr)
, m_iData(nullptr)
, m_iMult(nullptr)
, m_iPRN(nullptr)
, m_nVal(0)
, m_tmp_iMult(0)
, m_start(2,0)
, m_n2write(2,1)
{
} // H5ArrayWriter::impl::impl
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5ArrayWriter::impl::~impl ()
{

} // H5ArrayWriter::~H5ArrayWriter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool H5ArrayWriter::impl::ForceToH5File ()
{
  CStr  path(H5Path());
  if (path.Find("Arrays/") != -1) return false;
  else                            return true;
} // H5ArrayWriter::impl::ForceToH5File
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5ArrayWriter::impl::WriteData ()
{
  m_nVal = GetNumValsToWrite();
  bool  compress(m_pack->GetNative()->Compress());
  CStr  fname(H5Filename());
  CStr  path(H5Path());
  hid_t datatype((hid_t)GetDataType());
  int   nDim(GetNumDim());

  H5DataSetWriterSetup setup(fname, path, datatype, nDim, compress);
  H5DataSetWriter      h(&setup);
  h.AllowTypeConversions(true);
  SetH5DimensionsForWriting(h);

  GetPackageData();
  WriteH5DataSet(&h);
  WriteMultiplierToh5();

  return FileStringRefToh5Data(fname, path);
} // H5ArrayWriter::impl::WriteData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5ArrayWriter::impl::SetH5DimensionsForWriting (H5DataSetWriter& a_h)
{
  int nDim(GetNumDim());
  std::vector<hsize_t> start(nDim,0), n2write(nDim,1);
  bool useDimArrays(false);
  CStr path(H5Path());
  CStr packName(m_pack->GetPackage()->PackageName());
  int  spIdx(m_pack->GetGlobal()->GetCurrentPeriod()-1);

  if (iSubPackData(path))
  {
    useDimArrays = true;
    n2write[0] = (hsize_t)m_nVal;
    start[1] = (hsize_t)(GetSubPackageArrayIndex());
  }
  else if (iRchEtEtsPackData(path))
  {
    useDimArrays = true;
    start[0] = (hsize_t)(iPackNameToArrayIndex(packName));
    if (!iRchEtEtsLayerData(path))
    {
      start[2] = (hsize_t)spIdx;
      n2write[1] = (hsize_t)m_nVal;
    }
    else
    {
      start[1] = (hsize_t)spIdx;
      n2write[0] = (hsize_t)m_nVal;
    }
  }

  if (!useDimArrays) m_dimStr.Format("1 0 %d", m_nVal);
  else
  {
    H5DSWriterDimInfo dim(start, n2write);
    a_h.SetDimInfoForWriting(&dim);

    m_dimStr.Format("%d ", start.size());
    CStr s;
    for (size_t i=0; i<start.size(); ++i)
    {
      s.Format("%d %d ", (int)start[i], (int)n2write[i]);
      m_dimStr += s;
    }
  }
} // H5ArrayWriter::impl::SetDimensionsForWriting
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5ArrayWriter::impl::FileStringRefToh5Data (CStr a_fname, CStr a_path)
{
  int prn(-1);
  if (m_iPRN) prn = *m_iPRN;

  CStr rval, f1;
  util::StripPathFromFilename(a_fname, f1);
  rval.Format("HDF5 %s %d \"%s\" \"%s\" ", m_multStr, prn, f1, a_path);
  rval += m_dimStr;
  return rval;
} // H5ArrayWriter::impl::FileStringRefToh5Data
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5ArrayWriter::impl::GetPackageData ()
{
  MfPackage* p(m_pack->GetPackage());
  if (!p) return;
  p->GetField(Packages::Array::MULT, &m_rMult);
  p->GetField(Packages::Array::MULT, &m_iMult);
  p->GetField(Packages::Array::ARRAY, &m_rData);
  p->GetField(Packages::Array::ARRAY, &m_dData);
  p->GetField(Packages::Array::ARRAY, &m_iData);
  p->GetField(Packages::Array::IPRN, &m_iPRN);
} // H5ArrayWriter::impl::GetPackageData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5ArrayWriter::impl::WriteH5DataSet (H5DataSetWriter* a_h)
{
  if ((m_dData || m_rData) && m_rMult)
  {
    if (m_rData) a_h->WriteData(m_rData, m_nVal);
    else         a_h->WriteData(m_dData, m_nVal);
    m_multStr = STR(*m_rMult);
  }
  else 
  {
    if (!m_iMult && m_rMult)
    {
      m_tmp_iMult = (int)(*m_rMult);
      m_iMult = &m_tmp_iMult;
      m_rMult = nullptr;
    }
    if (m_iData && m_iMult)
    {
      a_h->WriteData(m_iData, m_nVal);
      m_multStr = STR((float)*m_iMult);
    }
    else ASSERT(0);
  }
} // H5ArrayWriter::impl::WriteH5DataSet
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5ArrayWriter::impl::H5Filename ()
{
  Mf2kNative* n = m_pack->GetNative();
  CStr rval = n->GetExp()->GetBaseFileName();
  rval += ".h5";
  return rval;
} // H5ArrayWriter::impl::H5Filename
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5ArrayWriter::impl::H5Path ()
{
  std::map<CStr, CStr> amap(m_pack->GetNative()->GetMapArrays());
  MfPackage* p(m_pack->GetPackage());
  CStr path, packName(p->PackageName());
  const int* layer(0);
  p->GetField(Packages::Array::LAYER, &layer);

  if (packName.find("ZONE ARRAY:") != -1 ||
      packName.Find("MULT. ARRAY:") != -1)
  {
    packName.Replace("ZONE ARRAY: ", "z");
    packName.Replace("MULT. ARRAY: ", "m");
    layer = nullptr;
  }

  if (amap.find(packName) == amap.end())
  {
    amap.insert(std::make_pair(packName, packName));
  }

  path = amap.find(packName)->second;
  if (path.Find("/") == -1)
  {
    path.Format("Arrays/%s", amap.find(packName)->second);
    if (layer)
    {
      CStr layStr;
      layStr.Format("%d", *layer);
      path += layStr; 
    }
  }
  return path;
} // H5ArrayWriter::impl::H5Path
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int H5ArrayWriter::impl::GetDataType ()
{
  int rval = (int)H5T_NATIVE_DOUBLE;
  MfPackage* p(m_pack->GetPackage());
  const int* iData(0);
  p->GetField(Packages::Array::ARRAY, &iData);
  if (iData) rval = (int)H5T_NATIVE_INT;
  return rval;
} // H5ArrayWriter::impl::GetDataType
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int H5ArrayWriter::impl::GetNumDim ()
{
  int nDim(1);
  CStr path(H5Path());
  if (iSubPackData(path))
  {
    nDim = 2;
  }
  else if (iRchEtEtsPackData(path))
  {
    if (iRchEtEtsLayerData(path)) nDim = 2;
    else                          nDim = 3;
  }
  return nDim;
} // H5ArrayWriter::GetNumDim 
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int H5ArrayWriter::impl::GetNumValsToWrite ()
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
} // H5ArrayWriter::impl::GetNumValsToWrite 
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int H5ArrayWriter::impl::GetSubPackageArrayIndex ()
{
  int rval(0);
  MfPackage* p = m_pack->GetPackage();
  if (p)
  {
    rval = (int)p->StringsToWrite().size();
  }
  else ASSERT(0);
  return rval;
} // H5ArrayWriter::impl::GetSubPackageArrayIndex
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5ArrayWriter::impl::WriteMultiplierToh5 ()
{
  CStr  path(H5Path());
  if (path.Find("Arrays/") != -1) return;

  hid_t datatype((hid_t)GetDataType());
  CStr  fname(H5Filename());

  path += " Multiplier";
  path.Replace("07.", "08.");
  path.Replace("09.", "10.");

  path.Replace("12.", "13.");
  path.Replace("14.", "15.");
  path.Replace("16.", "17.");
  path.Replace("18.", "19.");
  path.Replace("20.", "21.");
  path.Replace("22.", "23.");
  path.Replace("24.", "25.");
  path.Replace("26.", "27.");
  path.Replace("28.", "29.");
  path.Replace("30.", "31.");

  int nDim(GetMultiplierNumDim());
  int multIdx0(GetMultiplierIdx0());
  int multIdx1(GetMultiplierIdx1());
  H5DataSetWriterSetup s(fname, path, datatype, nDim);
  H5DataSetWriter h(&s);
  h.AllowTypeConversions(true);

  std::vector<hsize_t> start(nDim,0), n2write(nDim,1);
  if (nDim == 1)
  {
    start[0] = (hsize_t)multIdx0;
  }
  else if (nDim == 2)
  {
    start[0] = (hsize_t)multIdx0;
    start[1] = (hsize_t)multIdx1;
  }
  H5DSWriterDimInfo dim(start, n2write);
  h.SetDimInfoForWriting(&dim);

  if (m_rMult)       h.WriteData(m_rMult, 1);
  else if (m_iMult)  h.WriteData(m_iMult, 1);
} // H5ArrayWriter::impl::WriteMultiplierToh5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int H5ArrayWriter::impl::GetMultiplierNumDim ()
{
  int rval(1);
  CStr  path(H5Path());
  if (iRchEtEtsPackData(path))
  {
    if (!iRchEtEtsLayerData(path)) rval = 2;
  }
  return rval;
} // H5ArrayWriter::impl::GetMultiplierNumDim
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int H5ArrayWriter::impl::GetMultiplierIdx0 ()
{
  int   rval(1);
  CStr  path(H5Path());

  if (iSubPackData(path))
  {
    rval = GetSubPackageArrayIndex();
  }
  else if (iRchEtEtsPackData(path))
  {
    int spIdx(m_pack->GetGlobal()->GetCurrentPeriod()-1);
    CStr packName(m_pack->GetPackage()->PackageName());
    if (iRchEtEtsLayerData(path)) rval = spIdx;
    else                          rval = iPackNameToArrayIndex(packName);
  }

  return rval;
} // H5ArrayWriter::impl::GetMultiplierIdx
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int H5ArrayWriter::impl::GetMultiplierIdx1 ()
{
  int   rval(m_pack->GetGlobal()->GetCurrentPeriod()-1);
  return rval;
} // H5ArrayWriter::impl::GetMultiplierIdx1





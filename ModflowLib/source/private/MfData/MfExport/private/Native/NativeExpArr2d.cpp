//------------------------------------------------------------------------------
// FILE      NativeExpArr2d.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpArr2d.h>

#include <fstream>
#include <sstream>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>
#include <private\Parameters\PilotPoints.h>

using namespace MfData::Export;
namespace
{
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
template <typename T>
void iArrayDataToStream (std::ostream& a_os, const T* a_data,
                         const int* a_iData, int a_nrow, int a_ncol)
{
  if (a_os.bad())
  {
    ASSERT(0);
    return;
  }

  int w = 0, len;
  int flg = STR_FULLWIDTH;

  int txtCol = a_ncol;
  if (1 == a_nrow) txtCol = 15;
  if (a_data)
  {
    for (int i=0; i<a_nrow*a_ncol; ++i)
    {
      len = STR(a_data[i]).GetLength();
      if (len > w) w = len;
    }
    for (int i=0; i<a_nrow*a_ncol; ++i)
    {
      a_os << STR(a_data[i],-1,w,flg) << " ";
      if (i > 0 && (i+1)%txtCol == 0) a_os << "\n";
    }
  }
  else
  {
    CStr tmp, fmt;
    for (int i=0; i<a_nrow*a_ncol; ++i)
    {
      tmp.Format("%d", a_iData[i]);
      len = tmp.GetLength();
      if (len > w) w = len;
    }
    tmp.Format("%d", w);
    fmt = "%" + tmp + "d";
    for (int i=0; i<a_nrow*a_ncol; ++i)
    {
      tmp.Format(fmt, a_iData[i]);
      a_os << tmp << " ";
      if (i > 0 && (i+1)%txtCol == 0) a_os << "\n";
    }
  }
} // iArrayDataToStream
//------------------------------------------------------------------------------
CStr getIprn2dRel (int a_)
{
  if (a_ < 0 || a_ > 21)
  {
    ASSERT(0);
    return "0";
  }
//   CStr iprn[22] = {"10G11.4","11G10.3","9G13.6","15F7.1","15F7.2","15F7.3",
//                    "15F7.4","20F5.0","20F5.1","20F5.2","20F5.3","20F5.4",
//                    "10G11.4","10F6.0","10F6.1","10F6.2","10F6.3","10F6.4",
//                    "10F6.5","5G12.5","6G11.4","7G9.2"};
  CStr str;
  str.Format("%d", a_);
  return str;
} // getIprn2dRel 
//------------------------------------------------------------------------------
CStr getIprn2dInt (int a_)
{
  if (a_ < 0 || a_ > 9)
  {
    ASSERT(0);
    return "0";
  }
//   CStr iprn[10] = {"10I11","60I1","40I2","30I3","25I4","20I5","10I11","25I2",
//                    "15I4","10I6"};
//   return iprn[a_];
  CStr str;
  str.Format("%d", a_);
  return str;
} // getIprn2dInt
//------------------------------------------------------------------------------
bool ClustersExistForParType (ParamList* a_plist,
                              CStr a_partype)
{
  Param p;
  for (size_t i=0; i<a_plist->Size(); ++i)
  {
    a_plist->At(i, &p);
    if (p.m_type.CompareNoCase(a_partype) == 0)
    {
      if (p.m_clustInParamFile) return true;
    }
  }
  return false;
} // ClustersExistForParType
//------------------------------------------------------------------------------
void KeysValuesForParType(ParamList* a_plist,
                          CStr a_partype,
                          std::set<double>& a_keys,
                          std::vector<double>& a_pilot)
{
  Param p;
  for (size_t i=0; i<a_plist->Size(); ++i)
  {
    a_plist->At(i, &p);
    if (p.m_type.CompareNoCase(a_partype) == 0)
    {
      a_keys.insert(p.m_key);
      if (p.m_pilotPoints) a_pilot.push_back(p.m_key);
    }
  }

} // KeysValuesForParType
//------------------------------------------------------------------------------
MfData::MfPackage* iGetPackage(MfData::MfGlobal* a_global,
                               const CStr& a_pack)
{
  MfData::MfPackage* p = a_global->GetPackage(a_pack);
  if (!p)
  {
    MfData::MfPackage p1(a_pack);
    a_global->AddPackage(&p1);
    p = a_global->GetPackage(a_pack);
  }
  return p;
} // iGetPackage
//------------------------------------------------------------------------------
bool ArealArray (const CStr& a_name)
{
  bool rval = false;
  if (a_name.find(ARR_RCH_RCH) != -1 ||
      a_name.find(ARR_RCH_LAY) != -1 ||
      a_name.find(ARR_EVT_EXT) != -1 ||
      a_name.find(ARR_EVT_LAY) != -1 ||
      a_name.find(ARR_EVT_RATE) != -1 ||
      a_name.find(ARR_EVT_SURF) != -1 ||
      a_name.find(ARR_ETS_EXT) != -1 ||
      a_name.find(ARR_ETS_RATE) != -1 ||
      a_name.find(ARR_ETS_SURF) != -1 ||
      a_name.find(ARR_ETS_PXDP) != -1 ||
      a_name.find(ARR_ETS_PETM) != -1 ||
      a_name.find(ARR_UZF_RCH) != -1 ||
      a_name.find(ARR_UZF_ET) != -1 ||
      a_name.find(ARR_UZF_EXT) != -1 ||
      a_name.find(ARR_UZF_EXTWC) != -1
      )
    rval = true;
  return rval;
} // ArealArray
//------------------------------------------------------------------------------
static void RemoveLastReturn (CStr& line)
{
  // remove the last return
  while (line.at(line.GetLength()-1) == '\n')
  {
    line.pop_back();
  }
} // RemoveLastReturn
} // unnamed namespace


//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpArr2d::NativeExpArr2d () :
  m_name()
, m_lay(0)
, m_iData(0)
, m_iMult(0)
, m_iPRN(0)
, m_dataConst(0)
, m_data(0)
, m_dataD(nullptr)
, m_mult(0)
, m_firstTime(1)
, m_unstructured(0)
, m_stacked(0)
, m_tmp_iMult(1)
{
  bool usg = MfData::MfGlobal::Get().ModelType() == MfData::USG;
  if (usg)
  {
    m_unstructured = MfData::MfGlobal::Get().Unstructured() ? 1 : 0;
    MfPackage* p = MfData::MfGlobal::Get().GetPackage(MfData::Packages::DISU);
    if (p)
    {
      const int* ivsd(0);
      p->GetField(MfData::Packages::Disu::IVSD, &ivsd);
      if (ivsd && -1 == *ivsd) m_stacked = true;
    }
  }
} // MfNativeExpArr2d::MfNativeExpArr2d
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpArr2d::~NativeExpArr2d ()
{
} // MfNativeExpArr2d::~MfNativeExpArr2d
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpArr2d::Export ()
{
  if (!GetData()) return false;

  if (CheckParameters()) return true;

  // see if we can do constant
  if (CanDoConstant()) return true;

  if (WriteInternalArray()) return true;

  // write the array to a file
  WriteToFile();

  return true;
} // MfNativeExpArr2d::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpArr2d::ArrayName ()
{
  CStr name = (GetPackage()->PackageName());
  if (GetGlobal()->GetPackage(Packages::BCF))
  {
    if (name == ARR_LPF_HK) name = ARR_BCF_HY;
    if (name == ARR_LPF_WET) name = ARR_BCF_WET;
  }
  else if (GetGlobal()->GetPackage(Packages::HUF))
  {
    if (name == ARR_LPF_WET) name = ARR_HUF_WET;
  }
  return name;
} // NativeExpArr2d::ArrayName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpArr2d::GetData ()
{
  m_name = ArrayName();
  MfPackage* package = GetPackage();
  if (!package->GetField(Packages::Array::LAYER, &m_lay) || !m_lay)
  {
    ASSERT(0);
    return false;
  }
  const double *dataConstDbl(nullptr);
  package->GetField(Packages::Array::MULT, &m_mult);
  if (!package->GetField(Packages::Array::ARRAY, &m_dataConst) || !m_dataConst ||
      !m_mult)
  {
    package->GetField(Packages::Array::MULT, &m_iMult);
    if (!m_iMult && m_mult)
    {
      m_tmp_iMult = static_cast<int>(*m_mult);
      m_iMult = &m_tmp_iMult;
      m_mult = nullptr;
    }
    if (!package->GetField(Packages::Array::ARRAY, &m_iData) || !m_iData ||
        !m_iMult)
    {
      if (!package->GetField(Packages::Array::ARRAY, &dataConstDbl) || !dataConstDbl ||
          !package->GetField(Packages::Array::MULT, &m_mult) || !m_mult)
      {
        ASSERT(0);
        return false;
      }
    }
  }
  package->GetField(Packages::Array::IPRN, &m_iPRN);
  m_nrow = GetGlobal()->NumRow();
  m_ncol = GetGlobal()->NumCol();
  if (m_unstructured)
  {
    m_nrow = 1;
    const int* JJ(nullptr);
    if (!package->GetField("JJ", &JJ) || !JJ)
    {
      ASSERT(0);
      return false;
    }
    m_ncol = *JJ;
  }
  m_curSp = GetGlobal()->GetCurrentPeriod();
  if (m_dataConst)
  {
    m_data = const_cast<Real*>(m_dataConst);
  }
  if (!m_data && dataConstDbl) {
    m_dataD = const_cast<double*>(dataConstDbl);
  }
  if (m_name == ARR_BAS_IBND) SaveIbound();
  return true;
} // NativeExpArr2d::GetData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::SaveIbound ()
{
  std::vector< std::vector<int> >& ibnd(GetNative()->Ibound());
  if (1 == *m_lay)
  {
    std::vector<int> myVec(m_nrow*m_ncol, 0);
    ibnd.assign(GetGlobal()->NumLay(), myVec);
  }

  for (size_t i=0; i<ibnd.front().size(); ++i)
  {
    ibnd[*m_lay-1][i] = m_iData[i];
  }
} // NativeExpArr2d::SaveIbound
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpArr2d::GetArrayFileName (const CStr& a_name)
{
  // the name will be "modelname"_array_HK1 ...
  std::map<CStr, CStr>::iterator it = GetNative()->GetMapArrays().find(a_name);
  CStr arrName;
  if (it == GetNative()->GetMapArrays().end())
  {
    if (a_name.Find(ARR_ZON) != -1)
    {
      CStr tmp = a_name;
      tmp.Replace(ARR_ZON, "");
      tmp.Trim();
      arrName.Format("ZON_%s", tmp);
    }
    else if (a_name.Find(ARR_MLT) != -1)
    {
      CStr tmp = a_name;
      tmp.Replace(ARR_MLT, "");
      tmp.Trim();
      arrName.Format("MLT_%s", tmp);
    }
    else
    {
      ASSERT(0);
      return CStr();
    }
  }
  else
  {
    CStr name = a_name;
    int lay = *m_lay;
    if (name.Find(ARR_DIS_TOP) != -1) lay = 1;
    CStr layStr;
    layStr.Format("%d", lay);
    if (name.find(ARR_HUF_TOP) != -1 ||
        name.find(ARR_HUF_THCK) != -1)
    {
      layStr = a_name;
      layStr.Replace(ARR_HUF_TOP, "");
      layStr.Replace(ARR_HUF_THCK, "");
      name.Replace(layStr, "");
    }
    else if (ArealArray(name))
    {
      layStr.Format("%d", m_curSp);
      EtsSegnumber(layStr, name);
    }
    else if (ARR_LAK_ID == name ||
             ARR_LAK_LEAK == name)
    {
      CStr tmp;
      tmp.Format("_Sp%d", m_curSp);
      layStr += tmp;
    }
    else if (name.find(ARR_SWI_ZETA) != -1) {
      // Use zeta surface number and layer number
      CStr surfaceNumber = a_name;
      surfaceNumber.Replace(ARR_SWI_ZETA, ""); // Remove all but surface #
      name.Replace(surfaceNumber, ""); // Remove surface number string
      surfaceNumber.Trim();
      std::stringstream ss;
      ss << surfaceNumber << "_" << *m_lay; // "surface#_layer#"
      layStr = ss.str();
    }

    arrName.Format("%s_%s_%s", GetNative()->PackageFromArrayName(name),
                               GetNative()->VarNameFromArrayName(name),
                               layStr);
    if (ARR_UZF_UBND == name || ARR_UZF_RBND == name ||
        ARR_UZF_VKS == name  || ARR_UZF_EPS == name  ||
        ARR_UZF_THTS == name || ARR_UZF_THTS == name)
    {
      arrName.Replace("_0", "");
    }
  }
  CStr fname, fname1 = ArrayFileName(GetNative()->FileName());
  fname.Format("%s_array_%s.txt", fname1, arrName);
  return fname;
} // NativeExpArr2d::GetArrayFileName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::CreateFolderIfNeeded ()
{
  CStr dir, fname = GetNative()->FileName();
  util::StripFileFromFilename(fname.c_str(), dir);
  dir += "arrays";
  ::CreateDirectory(dir, NULL);
} // NativeExpArr2d::CreateFolderIfNeeded
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::EtsSegnumber (CStr& a_layStr, CStr& a_name)
{
  if (a_name != ARR_ETS_PETM && a_name != ARR_ETS_PXDP) return;

  int seg = 0;

  MfGlobal* g = GetGlobal();
  CStr varSp = a_name;
  varSp += " Stress Period";
  int storedSp = 0;
  if (!g->GetIntVar(varSp, storedSp))
  {
    storedSp = m_curSp;
    g->SetIntVar(varSp, storedSp);
  }
  CStr varSeg = a_name;
  varSeg += "Segment";
  int storedSeg = 0;
  g->GetIntVar(varSeg, storedSeg);

  if (storedSp != m_curSp)
  { // we are in a different stress period so reset
    storedSp = m_curSp;
    g->SetIntVar(varSp, storedSp);
    seg = 1;
    g->SetIntVar(varSeg, seg);
  }
  else
  {
    seg = storedSeg + 1;
    g->SetIntVar(varSeg, seg);
  }

  CStr tmp;
  tmp.Format("_Seg%d", seg);
  a_layStr += tmp;
} // NativeExpArr2d::EtsSegnumber
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpArr2d::StressPeriodPar (const CStr& a_type)
{
  if ( a_type.CompareNoCase("rch") == 0
    || a_type.CompareNoCase("evt") == 0
    || a_type.CompareNoCase("ets") == 0) return true;
  return false;
} // NativeExpArr2d::StressPeriodPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpArr2d::CanDoConstant ()
{
  if ((m_mult && 1.0 == *m_mult) ||
      (m_iMult && 1 == *m_iMult))
  {
    bool constant(true);
    if (m_mult && !m_dataD)
    {
      for (int i=1; i<m_nrow*m_ncol && constant; ++i)
      {
        if (m_data[0] != m_data[i]) constant = false;
      }
    }
    else if (m_mult && m_dataD)
    {
      for (int i=1; i<m_nrow*m_ncol && constant; ++i)
      {
        if (m_dataD[0] != m_dataD[i]) constant = false;
      }
    }
    else if (m_iMult)
    {
      for (int i=1; i<m_nrow*m_ncol && constant; ++i)
      {
        if (m_iData[0] != m_iData[i]) constant = false;
      }
    }
    if (constant)
    {
      CStr str;
      if (m_mult && !m_dataD)     str.Format("CONSTANT %s", STR(m_data[0]));
      else if (m_mult && m_dataD) str.Format("CONSTANT %s", STR(m_dataD[0]));
      else                        str.Format("CONSTANT %d", m_iData[0]);
      AddToStoredLinesDesc(str, "");
      
      SubstituteMultArray();

      return true;
    }
  }
  return false;
} // NativeExpArr2d::CanDoConstant
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpArr2d::WriteInternalArray ()
{
  if (!GetNative()->GetArraysInternal()) return false;

  CStr str;
  str.Format("INTERNAL %s (FREE) %s", StrMult(), StrIprn());
  AddToStoredLinesDesc(str, "");
  std::stringstream os;
  if (m_dataD)
  {
    ArrayDataToStreamDbl(os, m_dataD, m_iData);
  }
  else
  {
    ArrayDataToStream(os, m_data, m_iData);
  }

  str = os.str();
  RemoveLastReturn(str);
  AddToStoredLinesDesc(str, "");
  return true;
} // NativeExpArr2d::WriteInternalArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::SubstituteMultArray ()
{
  if (m_name.find(ARR_MLT) == -1) return;

  if (m_dataD)
  {
    Parameters::SubstituteArray(m_dataD, (size_t)(m_nrow*m_ncol), *m_lay, m_name);
  }
  else
  {
    Parameters::SubstituteArray(m_data, (size_t)(m_nrow*m_ncol), *m_lay, m_name);
  }
} // SubstituteMultArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::WriteToFile ()
{
  CStr fname = GetArrayFileName(m_name);
  if (m_dataD)
  {
    ArrayDataToFileDbl(fname, m_dataD, m_iData);
  }
  else
  {
    ArrayDataToFile(fname, m_data, m_iData);
  }
  util::StripPathFromFilename(fname, fname);
  if (GetNative()->GetArraysInFolder())
  {
    fname = ".\\arrays\\" + fname;
  }
  CStr str;
  str.Format("OPEN/CLOSE %s %s (FREE) %s", fname, StrMult(), StrIprn());
  AddToStoredLinesDesc(str, "");
} // NativeExpArr2d::WriteToFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::ArrayDataToFile (const CStr& a_fname,
                                      const Real* a_data,
                                      const int*  a_iData)
{
  std::fstream os;
  os.open((LPCTSTR)a_fname, std::ios_base::out);
  ArrayDataToStream(os, a_data, a_iData);
  os.close();
} // NativeExpArr2d::ArrayDataToFile
//----- OVERLOAD ---------------------------------------------------------------
void NativeExpArr2d::ArrayDataToFileDbl (const CStr& a_fname,
                                      const double* a_data,
                                      const int*  a_iData)
{
  std::fstream os;
  os.open((LPCTSTR)a_fname, std::ios_base::out);
  ArrayDataToStreamDbl(os, a_data, a_iData);
  os.close();
} // NativeExpArr2d::ArrayDataToFiledbl
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::ArrayDataToStream (std::ostream& a_os,
                                        const Real* a_data,
                                        const int*  a_iData)
{
  iArrayDataToStream(a_os, a_data, a_iData, m_nrow, m_ncol);
} // NativeExpArr2d::ArrayDataToStream
//----- OVERLOAD ---------------------------------------------------------------
void NativeExpArr2d::ArrayDataToStreamDbl (std::ostream& a_os,
                                        const double* a_data,
                                        const int*  a_iData)
{
  iArrayDataToStream(a_os, a_data, a_iData, m_nrow, m_ncol);
} // NativeExpArr2d::ArrayDataToStreamDbl
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpArr2d::StrIprn ()
{
  CStr rval;
  if (*m_iPRN < 0) rval = "-1";
  if (rval.IsEmpty())
  {
    if (m_data || m_dataD) rval = getIprn2dRel(*m_iPRN);
    else rval = getIprn2dInt(*m_iPRN);
  }
  return rval;
} // NativeExpArr2d::StrIprn
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpArr2d::StrMult ()
{
  CStr rval;
  if (m_data || m_dataD) rval = STR(*m_mult);
  else rval.Format("%d", *m_iMult);
  return rval;
} // NativeExpArr2d::StrMult
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpArr2d::ArrayFileName (const CStr& a_fname)
{
  CStr fileName = a_fname;
  util::StripExtensionFromFilename(fileName, fileName);
  if (GetNative()->GetArraysInFolder())
  {
    CreateFolderIfNeeded();
    CStr path, baseF, tStr = fileName;
    util::StripFileFromFilename(tStr, path);
    util::StripPathFromFilename(tStr, baseF);
    fileName.Format("%sarrays\\%s", path, baseF);
  }
  return fileName;
} // NativeExpArr2d::ArrayFileName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpArr2d::CheckParameters ()
{
  ParamList* pList;
  Parameters::GetParameterList(&pList);
  if (!pList) return false;

  // see if we have parameters for this array
  CStr partype = GetNative()->ParamTypeFromArrayName(m_name);
  if (!pList->ParamOfTypeExists(partype)) return false;

  // if clusters exist for the parameters then we don't need to do anything
  if (ClustersExistForParType(pList, partype)) return false;

  // just using key values for parameters so we need to do some work

  if (Packages::BCF == GetNative()->PackageFromArrayName(m_name))
  {
    if (m_dataD)
    {
      Parameters::SubstituteArray(m_dataD, (size_t)(m_nrow*m_ncol), *m_lay, m_name);
    }
    else
    {
      Parameters::SubstituteArray(m_data, (size_t)(m_nrow*m_ncol), *m_lay, m_name);
    }
    return false;
  }
  // get the key values for all parameters of this type
  std::set<double> keys;
  KeysValuesForParType(pList, partype, keys, m_pilotKeys);
  // make a zone array for this layer
  WriteZoneMultArrays(keys, pList);

  return false;
} // NativeExpArr2d::CheckParameters
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::WriteZoneMultArrays (std::set<double>& a_keys,
                                          ParamList* a_list)
{
  std::vector<int> ibnd(m_nrow*m_ncol, 1);
  if (*m_lay > 0 && !GetNative()->Ibound().empty())
    ibnd = GetNative()->Ibound()[*m_lay-1];
  std::vector<int> zone(m_nrow*m_ncol, 0);
  std::vector<Real> mlt(m_nrow*m_ncol, 0);

  bool addParam(0);
  int val(0);
  bool constant(true), rConst(true), first(true);
  Real rVal(0);
  std::set<int> foundKeys;
  for (int i=0; i<m_nrow*m_ncol; ++i)
  {
    if (ibnd[i]) // only worry about active cells
    {
      if (a_keys.find(m_dataD ? m_dataD[i] : (double)m_data[i]) == a_keys.end())
      {
        if (!addParam) EnsureParCluster(-999, a_list);
        addParam = true;
        zone[i] = -999;
        mlt[i] = (Real)(m_dataD ? m_dataD[i] : m_data[i]);
      }
      else
      {
        zone[i] = (int)(m_dataD ? m_dataD[i] : m_data[i]);
        mlt[i] = 1.0;
        if (foundKeys.find(zone[i]) == foundKeys.end())
        {
          foundKeys.insert(zone[i]);
          // make sure a cluster exists for this parameter in this layer
          EnsureParCluster((Real)(m_dataD ? m_dataD[i] : m_data[i]), a_list);
        }
      }

      if (first)
      {
        val = zone[i];
        rVal = mlt[i];
        first = false;
      }
      if (val != zone[i]) constant = false;
      if (rVal != mlt[i]) rConst = false;
    }
  }
  if (m_unstructured && !m_stacked)
  { // need to use MXNODLAY for the size of the array
    MfPackage* pp = iGetPackage(GetGlobal(), MfData::Packages::DISU);
    if (pp)
    {
      const int* nodlay(0);
      pp->GetField("NODLAY", &nodlay);
      if (nodlay)
      {
        int mxnodlay(0);
        for (int i=0; i<GetGlobal()->NumLay(); ++i)
        {
          if (nodlay[i] > mxnodlay) mxnodlay = nodlay[i];
        }
        m_ncol = mxnodlay;
        zone.resize(mxnodlay);
        mlt.resize(mxnodlay);
      }
    }
  }

  CStr pName = GetPackage()->PackageName();
  MfPackage* p = iGetPackage(GetGlobal(), MfData::Packages::ZON);
  MfPackage* pMlt = iGetPackage(GetGlobal(), MfData::Packages::MLT);
  p->StringsToWrite().push_back(ParArrayName());
  pMlt->StringsToWrite().push_back(ParArrayName());
  // the code above may create new packages which can move the pointer to our
  // current package so the next line makes sure we have it again.
  SetData(GetNative(), GetGlobal(), GetGlobal()->GetPackage(pName));

  CStr fname, fname1 = GetNative()->FileName();
  util::StripExtensionFromFilename(fname1, fname1);

  bool internalArrays = GetNative()->GetArraysInternal();
  CStr line;
  if (1.0 == *m_mult && constant)
  {
    line.Format("CONSTANT %d", val);
    p->StringsToWrite().push_back(line);
  }
  else if (internalArrays)
  {
    p->StringsToWrite().push_back("INTERNAL 1 (FREE) -1");
    std::stringstream os;
    ArrayDataToStream(os, NULL, &zone[0]);
    line = os.str();
    RemoveLastReturn(line);
    p->StringsToWrite().push_back(line);
  }
  else
  {
    fname1 = ArrayFileName(fname1);
    fname.Format("%s_array_ZON_%s.txt",
                 fname1, ParArrayName());
    ArrayDataToFile(fname, NULL, &zone[0]);
    util::StripPathFromFilename(fname, fname);
    line.Format("OPEN/CLOSE %s 1 (FREE) -1", fname);
    p->StringsToWrite().push_back(line);
  }

  if (1.0 == *m_mult && rConst)
  {
    line.Format("CONSTANT %s", STR(rVal));
    pMlt->StringsToWrite().push_back(line);
  }
  else if (internalArrays)
  {
    line.Format("INTERNAL %s (FREE) -1", STR(*m_mult));
    pMlt->StringsToWrite().push_back(line);
    std::stringstream os;
    ArrayDataToStream(os, &mlt[0], NULL);
    line = os.str();
    RemoveLastReturn(line);
    pMlt->StringsToWrite().push_back(line);
  }
  else
  {
    fname1 = ArrayFileName(fname1);
    fname.Format("%s_array_MLT_%s.txt",
                 fname1, ParArrayName());
    ArrayDataToFile(fname, &mlt[0], NULL);
    util::StripPathFromFilename(fname, fname);
    line.Format("OPEN/CLOSE %s %s (FREE) -1", fname, STR(*m_mult));
    pMlt->StringsToWrite().push_back(line);
  }
} // NativeExpArr2d::WriteZoneMultArrays
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::EnsureParCluster (Real a_key, ParamList* a_list)
{
  int lay = *m_lay;
  Param p;
  CStr type = GetNative()->ParamTypeFromArrayName(m_name);
  if (-999.0 == a_key)
  {
    CStr name(type);
    name += "_NoKey";
    if (!a_list->FindByName(name, &p))
    {
      Param p1(name, a_list->UnusedParamKey(), type, 1);
      a_list->PushBack(&p1);
      p = p1;
    }
  }
  else if (!a_list->FindByKey((double)a_key, &p)) return;
  if (StressPeriodPar(type)) lay = m_curSp;
  CStr zone;
  zone.Format("%s_%d", type, lay);
  PClust clust(zone, zone, lay);
  clust.m_iz.push_back((int)a_key);
  for (size_t i=0; i<p.m_clust.size(); ++i)
  {
    if (p.m_clust[i].m_mlt == clust.m_mlt &&
        p.m_clust[i].m_zon == clust.m_zon &&
        p.m_clust[i].m_lay == clust.m_lay &&
        p.m_clust[i].m_iz  == clust.m_iz  &&
        p.m_clust[i].m_hgu == clust.m_hgu)
    {
      return;
    }
  }
  if (p.m_pilotPoints &&
      (p.m_clust.empty() || (m_unstructured && !m_stacked)) )
  { // write the pilot point multiplier arrays
    WritePilotPointMultArrays(a_list, p, lay);
  }
  p.m_clust.push_back(clust);
  // add instance for stress period par
  if (StressPeriodPar(type))
  {
    CStr iName; iName.Format("SP_%d", m_curSp);
    p.m_instNames.push_back(iName);
    p.m_instStress[iName].push_back(m_curSp);
  }
  a_list->UpdateParameter(&p);
} // NativeExpArr2d::EnsureParCluster
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::WritePilotPointMultArrays (ParamList* a_list,
                                                Param& a_p,
                                                int a_layer)
{
  if (!a_list) return;
  std::vector<double> ppVals;
  if (!a_list->GetPilotPtValues(a_p.m_scatIndex, ppVals)) return;

  CStr fname = a_list->GetSourceFile();
  util::StripExtensionFromFilename(fname, fname);
  fname += ".h5";
  PilotPoints pp(fname, a_p);
  int ncolOrig = m_ncol;
  if (m_unstructured && !m_stacked)
  {
    MfPackage* p = GetGlobal()->GetPackage(MfData::Packages::DISU);
    if (p)
    {
      const int *nodlay(0);
      p->GetField("NODLAY", &nodlay);
      if (nodlay)
      {
        int mxnodlay(0);
        std::vector<int> vNodes(GetGlobal()->NumLay(), 0);
        for (int i=0; i<GetGlobal()->NumLay(); ++i)
        {
          if (nodlay[i] > mxnodlay) mxnodlay = nodlay[i];
          vNodes[i] = nodlay[i];
        }
        pp.SetUnstructured(vNodes);
        m_ncol = mxnodlay;
      }
    }
  }

  CStr line, ppName, fname1 = GetNative()->FileName();
  util::StripExtensionFromFilename(fname1, fname1);

  MfPackage* pMlt = iGetPackage(GetGlobal(), MfData::Packages::MLT);
  std::vector<Real> mlt(m_nrow*m_ncol, 0);
  for (size_t i = 0; i<ppVals.size(); ++i)
  {
    pp.SetLayer(a_layer);
    pp.GetWeightsForPoint((int)i+1, mlt);
    ppName.Format("pp%d_%d", a_p.m_scatIndex, i+1);
    if (a_layer > 1)
    {
      ppName.Format("pp%dL%d_%d", a_p.m_scatIndex, a_layer, i+1);
    }
    pMlt->StringsToWrite().push_back(ppName);
    if (!GetNative()->GetArraysInternal())
    {
      CStr fileName = ArrayFileName(fname1);
      fname.Format("%s_array_MLT_%s.txt", fileName, ppName);
      ArrayDataToFile(fname, &mlt[0], NULL);
      util::StripPathFromFilename(fname, fname);

      line.Format("OPEN/CLOSE %s 1.0 (FREE) -1", fname);
      pMlt->StringsToWrite().push_back(line);
    }
    else
    {
      line.Format("INTERNAL %s (FREE) %s", StrMult(), StrIprn());
      pMlt->StringsToWrite().push_back(line);
      std::stringstream os;
      ArrayDataToStream(os, &mlt[0], NULL);
      line = os.str();
      RemoveLastReturn(line);
      pMlt->StringsToWrite().push_back(line);
    }
  }
  m_ncol = ncolOrig;
} // NativeExpArr2d::WritePilotPointMultArrays
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpArr2d::ParArrayName ()
{
  CStr ptype = GetNative()->ParamTypeFromArrayName(m_name);
  CStr strLay;
  strLay.Format("%d", *m_lay);
  if (StressPeriodPar(ptype)) strLay.Format("%d", m_curSp);
  CStr zoneName;
  zoneName.Format("%s_%s", ptype, strLay);
  return zoneName;
} // NativeExpArr2d::ParArrayName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr2d::AddToStoredLinesDesc (const char* a_line,
                                           const char* a_desc)
{
  NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
  if (m_name.Find(ARR_ZON) != -1)
  {
    MfPackage* p = iGetPackage(GetGlobal(), MfData::Packages::ZON);
    CStr name = m_name;
    name.Replace(ARR_ZON, "");
    name.Trim();
    if (m_firstTime)
    {
      p->StringsToWrite().push_back(name);
      m_firstTime = false;
    }
    p->StringsToWrite().push_back(a_line);
  }
  if (m_name.Find(ARR_MLT) != -1)
  {
    MfPackage* p = iGetPackage(GetGlobal(), MfData::Packages::MLT);
    CStr name = m_name;
    name.Replace(ARR_MLT, "");
    name.Trim();
    if (m_firstTime)
    {
      p->StringsToWrite().push_back(name);
      m_firstTime = false;
    }
    p->StringsToWrite().push_back(a_line);
  }
} // NativeExpArr2d::AddToStoredLinesDesc


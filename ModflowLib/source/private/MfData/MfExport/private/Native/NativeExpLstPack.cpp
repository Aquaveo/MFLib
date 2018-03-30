//------------------------------------------------------------------------------
// FILE      NativeExpLstPack.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpLstPack.h>

#include <sstream>

#include <private\MfData\MfExport\private\H5\H5BcList.h>
#include <private\MfData\MfExport\private\H5\H5Util.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativeExpMf6LstPack.h>
#include <private\MfData\MfExport\private\Sqlite\SqBcList.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfPackageUtil.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>
#include <private\util\EReadAsciiFile.h>

using namespace MfData::Export;
const char * const CLN_CREATED = "H5 CLN Created";
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLstPack::NativeExpLstPack ()
: m_usg(false)
, m_unstructured(false)
, m_mf6(false)
, m_nBcs(0)
, m_nAux(0)
, m_nDataFields(0)
, m_nFields(0)
, m_data(0)
, m_fieldStrings()
, m_returnFlow(0)
, m_NP(0)
, m_MXL(0)
, m_nI(0)
, m_nJ(0)
, m_nK(0)
, m_nodeOffset(-1)
, m_par(0)
, m_h5Bc(0)
, m_sqList(0)
{
  m_usg = MfData::MfGlobal::Get().ModelType() == MfData::USG;
  if (m_usg)
  {
    m_nK = MfData::MfGlobal::Get().NumLay();
    m_nI = MfData::MfGlobal::Get().NumRow();
    m_nJ = MfData::MfGlobal::Get().NumCol();
    m_unstructured = MfData::MfGlobal::Get().Unstructured() ? 1 : 0;
  }

} // MfNativeExpLstPack::MfNativeExpLstPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLstPack::~NativeExpLstPack ()
{
  if (m_h5Bc) delete(m_h5Bc);
  if (m_sqList) delete(m_sqList);
} // MfNativeExpLstPack::~MfNativeExpLstPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::OnSetData ()
{
  MfData::Packages::GetBcData(GetPackage(), GetPackage()->PackageName(),
                              &m_nBcs, &m_nFields,
                              &m_nAux, &m_data, &m_nDataFields,
                              m_fieldStrings);
  if (GetPackage()->PackageName() == Packages::DRT &&
      m_fieldStrings.size() - *m_nAux >= 9)
  {
    m_returnFlow = true;
  }

  Mf2kNative* n = GetNative();
  if (n && n->GetExportMf6())
  {
    m_mf6 = true;
  }
} // NativeExpLstPack::OnSetData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::LastChanceBeforeWriting ()
{
  if (m_sqList) m_sqList->EndWriteFile();
  FILE *fp = fopen(ParInfoFileName(), "r");
  if (!fp) return;
  fclose(fp);
  RewriteFileWithParameters();
} // NativeExpLstPack::LastChanceBeforeWriting
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpLstPack::Export ()
{
  if (m_mf6)
  {
    NativeExpMf6LstPack lst(this);
    lst.Export();
    return true;
  }

  if (!m_sqList && GetNative()->GetUseSQLite()) m_sqList = new SqBcList(this);

  if (1 == GetGlobal()->GetCurrentPeriod())
  {
    if (GetH5Flag()) AddToStoredLinesDesc("#GMS_HDF5_01", "");
    Line1();
    Line2();

    if (m_sqList) m_sqList->AddSqComment();
    WriteComments();
  }

  Line5();
  Line6();

  WriteStoredLines();

  if (GetGlobal()->GetCurrentPeriod() == GetGlobal()->NumPeriods())
    LastChanceBeforeWriting();
  return true;
} // MfNativeExpLstPack::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::Line1 ()
{
  if (GetH5Flag() || Packages::DRT == GetPackage()->PackageName()) return;

  CStr desc = " 1. [PARAMETER ";
  desc += "NP";
  desc += GetPackage()->PackageName();
  desc += " MXL]";

  CStr ln;

  const int* np(0);
  if (!GetH5Flag() &&
      (!GetPackage()->GetField(Packages::ListPack::NP, &np) || !np ||
       *np < 1))
    return;

  AddToStoredLinesDesc(ln, desc);
} // NativeExpLstPack::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::CbFieldName ()
{
  CStr nm = GetPackage()->PackageName();
  if (Packages::DRN == nm)      return "IDRNCB";
  else if (Packages::WEL == nm) return "IWELCB";
  else if (Packages::GHB == nm) return "IGHBCB";
  else if (Packages::RIV == nm) return "IRIVCB";
  else if (Packages::CHD == nm) return "";
  else if (Packages::DRT == nm) return "IDRTCB";
  return "";
} // NativeExpLstPack::CbFieldName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Desc2 ()
{
  CStr nm = GetPackage()->PackageName();
  CStr cbField = CbFieldName();
  CStr desc = " 2. ";
  if (Packages::DRN == nm)      desc += "MXACTD ";
  else if (Packages::WEL == nm) desc += "MXACTW ";
  else if (Packages::GHB == nm) desc += "MXACTB ";
  else if (Packages::RIV == nm) desc += "MXACTR ";
  else if (Packages::CHD == nm) desc += "MXACTC ";
  else if (Packages::DRT == nm) desc += "MXADRT ";

  desc += cbField;
  if (Packages::DRT == nm)
  {
    desc += " NPDRT MXL";
    desc.Replace("2", "1");
  }

  desc += " [Option]";
  return desc;
} // NativeExpLstPack::Desc2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::Line2 ()
{
  CStr cbField = CbFieldName();
  CStr desc = Desc2();

  const int *maxBc(0), *cb(0);
  if (!GetPackage()->GetField(Packages::ListPack::MAXBC, &maxBc) || !maxBc) return;

  if (!cbField.IsEmpty() &&
      (!GetPackage()->GetField(cbField, &cb) || !cb)) return;

  CStr ln;
  ln.Format("%5d ", *maxBc);
  if (!cbField.IsEmpty())
  {
    CStr cbFieldVal;
    cbFieldVal.Format("%5d ", *cb);
    ln += cbFieldVal;
    if (m_sqList) m_sqList->AddVariable(cbField.c_str(), cbFieldVal.c_str());
  }

  if (Packages::DRT == GetPackage()->PackageName())
  {
    const int *np(0), *mxl(0);
    if (GetPackage()->GetField(Packages::ListPack::NP, &np) && np &&
        GetPackage()->GetField("MXL", &mxl) && mxl)
    {
      int tmpNp(*np);
      if (GetH5Flag()) tmpNp = 0;
      int imxl = *mxl;
      if (0 == tmpNp) imxl = 0;
      CStr ln2;
      ln2.Format("%5d %5d ", tmpNp, imxl);
      ln += ln2;
      if (m_returnFlow) ln += "RETURNFLOW ";
    }
  }

  int start = (int)m_fieldStrings.size() - *m_nAux;
  for (int i=0; i<*m_nAux; ++i)
  {
    ln += "AUX ";
    ln += m_fieldStrings[start+i];
    ln += " ";
  }
  if (Packages::WEL == GetPackage()->PackageName() &&
      GetGlobal()->ModelType() == MfData::USG)
  {
    const int* iwelqv(0), *iafr(0);
    GetPackage()->GetField("IWELQV", &iwelqv);
    GetPackage()->GetField("IAFR", &iafr);
    if (iwelqv && *iwelqv == 1)
    {
      ln += "AUTOFLOWREDUCE ";
    }
    if (iafr && *iafr != 0)
    {
      CStr tmp;
      tmp.Format("IUNITAFR %d ", *iafr);
      ln += tmp;
    }
  }

  AddToStoredLinesDesc(ln, desc);

  if (Packages::WEL == GetPackage()->PackageName() &&
      GetGlobal()->ModelType() == MfData::MFNWT)
  {
    const Real* PHIRAMP;
    if (GetPackage()->GetField("PHIRAMP", &PHIRAMP) && PHIRAMP)
    {
      CStr tmpLine;
      tmpLine.Format("SPECIFY %s", STR(*PHIRAMP));
      AddToStoredLinesDesc(tmpLine, "2b. [SPECIFY PHIRAMP IUNITRAMP]");
    }
  }
} // NativeExpLstPack::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::Line5 ()
{
  CStr desc = " 5. ITMP NP";
  CStr stress;
  stress.Format("          Stress Period %3d", GetGlobal()->GetCurrentPeriod());
  const int* itmpcln(0);
  GetPackage()->GetField(Packages::ListPack::ITMPCLN , &itmpcln);
  if (itmpcln)
  {
    desc += " ITMPCLN";
    int h5ClnCreated(0);
    GetGlobal()->GetIntVar(CLN_CREATED, h5ClnCreated);
    if (GetH5Flag() && !h5ClnCreated)
    {
      H5Util_CreateWelClnGroup(GetNative()->GetExp()->GetBaseFileName(),
                               GetNative()->CompressH5());
      h5ClnCreated = 1;
      GetGlobal()->SetIntVar(CLN_CREATED, h5ClnCreated);
    }
  }
  desc += stress;

  const int *itmp(0), *np(0);
  if (!GetPackage()->GetField(Packages::ListPack::ITMP, &itmp) || !itmp ||
      !GetPackage()->GetField(Packages::ListPack::NP , &np) || !np)
    return;

  if (m_sqList) m_sqList->AddItmp(GetGlobal()->GetCurrentPeriod(), *itmp);

  int tmpItmp(*itmp), tmpNp(*np), tmpItmpCln(0);
  if (itmpcln) tmpItmpCln = *itmpcln;
  if (GetH5Flag())
  {
    if (!m_h5Bc) m_h5Bc = new H5BcList(this);
    m_h5BcStr = m_h5Bc->LstPack(tmpItmp);
    tmpNp = 0;
    if (itmpcln && 0 < *itmpcln)
    {
      // get output lines for CLN wells to later write with line 6c
      m_h5BcStrClnWell = m_h5Bc->ClnWel(tmpItmpCln);
    }
    else
    {
      m_h5BcStrClnWell = "";
    }
  }
  CStr ln;
  ln.Format("%5d %5d", tmpItmp, tmpNp);
  if (itmpcln)
  {
    CStr ln1; ln1.Format(" %5d", tmpItmpCln);
    ln += ln1;
  }
  AddToStoredLinesDesc(ln, desc);
} // NativeExpLstPack::Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Desc6 ()
{
  CStr desc = " 6. Layer Row Column ";
  if (m_usg) desc.Replace(" 6", "6a");
  if (m_unstructured) desc = "6b. Node ";
  if (Packages::DRT == GetPackage()->PackageName()) desc.Replace("6", "5");
  CStr nm = GetPackage()->PackageName();
  if (Packages::DRN == nm)      desc += "Elevation Cond ";
  else if (Packages::WEL == nm) desc += "Q ";
  else if (Packages::GHB == nm) desc += "Bhead Cond ";
  else if (Packages::RIV == nm) desc += "Stage Cond Rbot ";
  else if (Packages::CHD == nm) desc += "Shead Ehead ";
  else if (Packages::DRT == nm) desc += "Elevation Cond [LayR RowR ColR Rfprop] ";
  desc += "[xyz]";
  return desc;
} // NativeExpLstPack::Desc6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::Line6 ()
{
  CStr desc = Desc6();

  const int *itmp(0);
  if (!GetPackage()->GetField(Packages::ListPack::ITMP, &itmp) || !itmp) return;
  const int* itmpcln(0);
  GetPackage()->GetField(Packages::ListPack::ITMPCLN , &itmpcln);

  CStr ln;
  if (GetH5Flag())
  {
    ln = m_h5BcStr;
    if (!ln.empty()) AddToStoredLinesDesc(ln, desc);
    ln = m_h5BcStrClnWell;
    if (!ln.empty()) AddToStoredLinesDesc(ln, "");
    return;
  }

  std::vector<CStr> lns;
  if (ParametersUsed(*itmp, lns))
  {
    for (size_t i=0; i<lns.size(); ++i) AddToStoredLinesDesc(lns[i], desc);
    return;
  }

  for (int i=0; i<*itmp; ++i)
  {
    ln = IjkToStr(i);
    for (size_t j=3; j<m_fieldStrings.size(); ++j)
    {
      ln += DataToStr(i, (int)j);
    }
    AddToStoredLinesDesc(ln, desc);
  }
  if (m_sqList) m_sqList->AddStressPeriodData();

  const int *nnpwel(0), *nodes(0);
  GetPackage()->GetField(Packages::ListPack::NUMBC, &nnpwel);
  GetPackage()->GetField("NODES", &nodes);
  if (nodes && nnpwel && itmpcln && 0 < *itmpcln)
  {
    m_nodeOffset = *nodes;
    bool tmpUnstructured = m_unstructured;
    m_unstructured = true;
    for (int ii=0; ii<*itmpcln; ++ii)
    {
      int i = ii + *nnpwel;
      ln = IjkToStr(i);
      for (size_t j=3; j<m_fieldStrings.size(); ++j)
      {
        ln += DataToStr(i, (int)j);
      }
      AddToStoredLinesDesc(ln, "6c. ICLNNODE Q [xyz]");
    }

    m_unstructured = tmpUnstructured;
  }
} // NativeExpLstPack::Line6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::IjkToStr (int a_i)
{
  CStr ln;

  if (!m_usg)
  {
    int i = (int)m_data[a_i*(*m_nDataFields)+0];
    int j = (int)m_data[a_i*(*m_nDataFields)+1];
    int k = (int)m_data[a_i*(*m_nDataFields)+2];

    if (!m_mf6)
    {
      ln.Format("%5d %5d %5d ", i, j, k);
    }
    else
    {
      int id = j + ( (i-1) * m_nJ ) + ( (k-1) * m_nI * m_nJ );
      ln.Format("%5d ", id);
    }
  }
  else
  {
    if (!m_unstructured && !m_mf6)
    { // calculate i, j, k from cell id
      int id = (int)m_data[a_i*(*m_nDataFields)+0];
      int i  = ( (id-1)/m_nJ ) % m_nI + 1;
      int j  = (id-1) % m_nJ + 1;
      int k  = (id-1) / (m_nI*m_nJ) + 1;
      ln.Format("%5d %5d %5d ", k, i, j);
    }
    else
    {
      int nodeid = (int)m_data[a_i*(*m_nDataFields)+0];
      if (-1 != m_nodeOffset) nodeid -= m_nodeOffset;
      ln.Format("%5d ", nodeid);
    }
  }
  return ln;
} // NativeExpLstPack::IjkToStr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::DataToStr (int a_i, int a_j)
{
  int w = util::RealWidth();
  CStr ln1 = STR(m_data[a_i*(*m_nDataFields)+a_j], -1, w, STR_FULLWIDTH);
  if (m_fieldStrings[a_j].CompareNoCase("iface") == 0 ||
      m_fieldStrings[a_j].CompareNoCase("cellgrp") == 0 ||
      m_fieldStrings[a_j].CompareNoCase("layr") == 0 ||
      m_fieldStrings[a_j].CompareNoCase("rowr") == 0 ||
      m_fieldStrings[a_j].CompareNoCase("colr") == 0)
  {
    ln1.Format("%5d ", (int)m_data[a_i*(*m_nDataFields)+a_j]);
  }
  ln1 += " ";
  return ln1;
} // NativeExpLstPack::DataToStr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::GetParType ()
{
  CStr nm = GetPackage()->PackageName();
  if (Packages::DRN == nm)      return "DRN";
  else if (Packages::WEL == nm) return "Q";
  else if (Packages::GHB == nm) return "GHB";
  else if (Packages::RIV == nm) return "RIV";
  else if (Packages::CHD == nm) return "CHD";
  else if (Packages::DRT == nm) return "DRT";
  return "";

} // NativeExpLstPack::GetParType
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int NativeExpLstPack::GetParamFieldIndex ()
{
  CStr nm = GetPackage()->PackageName();
  if (Packages::DRN == nm)      return 4;
  else if (Packages::WEL == nm) return 3;
  else if (Packages::GHB == nm) return 4;
  else if (Packages::RIV == nm) return 4;
  else if (Packages::CHD == nm) return 3;
  else if (Packages::DRT == nm) return 4;
  return -1;
} // NativeExpLstPack::GetParamFieldIndex
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int NativeExpLstPack::GetParamFactorFieldIndex ()
{
  CStr nm = GetPackage()->PackageName();
  int idx = -1;
  // find feild name Condfact, Qfact, Ehdfact
  for (size_t i=0; idx == -1 && i<m_fieldStrings.size(); ++i)
  {
    if (m_fieldStrings[i].CompareNoCase("condfact") == 0 ||
        m_fieldStrings[i].CompareNoCase("qfact") == 0 ||
        m_fieldStrings[i].CompareNoCase("sheadfact") == 0)
      idx = (int)i;
  }
  return idx;
} // NativeExpLstPack::GetParamFactorFieldIndex
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::GetParameterFieldIndices (int& a_pFieldIdx,
                                                 int& a_pFieldIdx2,
                                                 int& a_pFactIdx,
                                                 int& a_pFactIdx2)
{
  a_pFieldIdx = GetParamFieldIndex();
  a_pFieldIdx2 = -1;
  a_pFactIdx = GetParamFactorFieldIndex();
  a_pFactIdx2 = -1;
  if ("CHD" == GetParType())
  {
    a_pFieldIdx2 = a_pFieldIdx + 1;
    a_pFactIdx2 = a_pFactIdx + 1;
  }
} // NativeExpLstPack::GetParameterFieldIndices
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::GetParNameForBc (int a_bcIdx,
                                        int a_pFieldIdx,
                                        int a_pFieldIdx2,
                                        std::map<int, CStr>& a_mapKeyName)
{
  CStr nm;
  Real val = m_data[a_bcIdx*(*m_nDataFields)+a_pFieldIdx];
  int ival = static_cast<int>(val);
  if (a_mapKeyName.find(ival) != a_mapKeyName.end()) nm = a_mapKeyName[ival];
  if (nm.IsEmpty() && a_pFieldIdx2 > -1)
  {
    val = m_data[a_bcIdx*(*m_nDataFields)+a_pFieldIdx2];
    ival = static_cast<int>(val);
    if (a_mapKeyName.find(ival) != a_mapKeyName.end()) nm = a_mapKeyName[ival];
  }
  return nm;
} // NativeExpLstPack::GetParNameForBc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::map<int, CStr> NativeExpLstPack::GetParMap ()
{
  std::vector<Param> params = MfExportUtil::GetParamsOfType(GetParType());

  // create a map of keys to param names
  std::map<int, CStr> mapKeyName;
  for (size_t i=0; i<params.size(); i++)
  {
    mapKeyName[(int)params[i].m_key] = params[i].m_name;
  }
  return mapKeyName;
} // NativeExpLstPack::GetParMap
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpLstPack::ParametersUsed (int itmp, std::vector<CStr>& a_lns)
{
  std::map<int, CStr> mapKeyName = GetParMap();
  if (mapKeyName.empty()) return false;

  // get indices to the field with the potential parameter key (cond) and the
  // field for the parameter factor (cond. fact)
  int idx, idx2, factorIdx, factorIdx2;
  GetParameterFieldIndices(idx, idx2, factorIdx, factorIdx2);

  // map to hold line for the parameters
  std::map<CStr, std::vector<CStr> > mapNameLines;

  // variables for the parameter name associated with the BC
  CStr pName, nameNoPar;
  nameNoPar.Format("%s_NOPARAM", GetParType());

  ParamList* pList(NULL);
  Parameters::GetParameterList(&pList);
  // loop through the BCs
  CStr ln, ln1;
  for (int i=0; i<itmp; ++i)
  {
    pName = GetParNameForBc(i, idx, idx2, mapKeyName);
    if (pName.IsEmpty()) pName = nameNoPar;

    ln = IjkToStr(i);
    for (int j=3; j<(int)m_fieldStrings.size(); ++j)
    {
      ln1 = DataToStr(i, j);
      if (pName != nameNoPar && (j == idx || j == idx2))
      {
        ln1 = DataToStr(i, factorIdx);
        if ((int)j == idx2) ln1 = DataToStr(i, factorIdx2);
        if ("CHD" == GetParType() && pList)
        { // we need to make sure the field has the key value
          // if it doesn't then we get the value in the field and divide it
          // by the parameter value
          Param chd;
          if (pList->FindByName(pName, &chd))
          {
            if (chd.m_key != m_data[i*(*m_nDataFields)+j])
            {
              Real val = m_data[i*(*m_nDataFields)+j] / (Real)chd.m_value;
              ln1 = STR(val,-1,util::RealWidth(),STR_FULLWIDTH);
              ln1 += " ";
            }
          }
        }
      }
      ln += ln1;
    }
    mapNameLines[pName].push_back(ln);
  }

  WriteParDataToTmp(itmp, mapNameLines);
  if (mapNameLines.find(nameNoPar) != mapNameLines.end())
  {
    a_lns = mapNameLines[nameNoPar];
  }

  return true;
} // NativeExpLstPack::ParametersUsed
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::WriteParDataToTmp (int itmp,
                             std::map<CStr, std::vector<CStr> >& a_mapNameLines)
{
  CStr fFlag = "w";
  int per = GetGlobal()->GetCurrentPeriod();
  if (per > 1) fFlag = "a";

  CStr fnamePar = ParInfoFileName();
  FILE *fpPar = fopen(fnamePar, fFlag);
  int flg = (int)a_mapNameLines.size();
  if (a_mapNameLines.empty() && itmp < 0) flg = -1;
  if (fpPar)
  {
    fprintf(fpPar, "SP %d %d\n", per, flg);
  }

  int len;
  std::map<CStr, std::vector<CStr> >::iterator it(a_mapNameLines.begin());
  for (; it!=a_mapNameLines.end(); ++it)
  {
    std::stringstream os;
    for (size_t i=0; i<it->second.size(); ++i)
    {
      os << it->second[i] << "\n";
      len = it->second[i].GetLength();
      if (len + 4 > CommentCharPos()) CommentCharPos(len + 4);
    }

    int nBc = (int)it->second.size();
    CStr myStr;
    if (GetGlobal()->GetStrVar(it->first, myStr))
    {
      if (myStr == os.str()) nBc *= -1;
    }
    GetGlobal()->SetStrVar(it->first, os.str());

    // Is the number of bcs in every sp the same? If not we have to create
    // multiple parameters.
    CStr pName = it->first;
    CheckNumBcsForPar(pName, abs(nBc));

    if (fpPar)
    {
      fprintf(fpPar, "\"%s\" %d\n", pName.c_str(), nBc);
    }

    if (nBc > 0)
    {
      CStr f = ParSpFileName(pName, per);
      FILE *fp = fopen(f, "w");
      if (fp)
      {
        fprintf(fp, "%s", os.str().c_str());
        fclose(fp);
      }
    }
  }

  if (fpPar) fclose(fpPar);
} // NativeExpLstPack::WriteParDataToTmp
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::CheckNumBcsForPar (CStr& a_pName, int a_nBc)
{
  if (a_pName.find("_NOPARAM") != -1) return;

  int idx = -1;
  std::vector<int> npar;
  CStr fStr, field;
  field.Format("NPAR_%s", a_pName);
  fStr.Format("%d ", a_nBc);
  if (GetGlobal()->GetStrVar(field, fStr))
  {
    std::stringstream ss;
    ss << fStr;
    while (ss.good())
    {
      int i;
      ss >> i;
      if (ss.good()) npar.push_back(i);
    }
    // get the index in the vector that matches a_nBc
    for (size_t i=0; i<npar.size(); ++i)
    {
      if (a_nBc == npar[i]) idx = (int)i;
    }
    if (idx < 0)
    {
      npar.push_back(a_nBc);
      idx = (int)npar.size() - 1;
    }
    std::stringstream ss1;
    for (size_t i=0; i<npar.size(); ++i)
    {
      ss1 << npar[i] << " ";
    }
    fStr = ss1.str();
  }
  GetGlobal()->SetStrVar(field, fStr);
  if (idx > 0)
  {
    ModifyParNameAndCreatePar(a_pName, idx);
  }
} // NativeExpLstPack::CheckNumBcsForPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::ModifyParNameAndCreatePar (CStr& a_pName, int a_idx)
{
  ParamList* pList(0);
  Parameters::GetParameterList(&pList);
  Param orig, newPar;
  pList->FindByName(a_pName, &orig);
  newPar = orig;
  // change param name
  CStr pName = a_pName;
  a_pName.Format("%s_%d", pName, a_idx);
  // create a new parameter if needed
  newPar.m_name = a_pName;
  newPar.m_key = pList->UnusedParamKey();
  if (!pList->FindByName(a_pName, &orig))
  {
    pList->PushBack(&newPar);
    // add to the PVAL file
    MfPackage* p = GetGlobal()->GetPackage(Packages::PVAL);
    if (p)
    {
      CStr desc = p->StringDescriptions().back();
      CStr line;
      pName = a_pName;
      MfExportUtil::InsertSingleQuotesInName(pName);
      while (pName.GetLength() < 12) pName += " ";
      line.Format("%s %s", pName, STR(newPar.m_b));
      p->StringsToWrite().push_back(line);
      p->StringDescriptions().push_back(desc);
    }
  }
} // NativeExpLstPack::AddNewParameter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::RewriteFileWithParameters ()
{
  // read in the info on all the stress periods
  ReadParSpInfo();
  // remove unused parameters of this type
  ParamList *pList(NULL);
  Parameters::GetParameterList(&pList);
  if (pList)
  {
    CStr parToSkip;
    GetGlobal()->GetStrVar("Pars2Skip", parToSkip);
    std::vector<Param> pars = MfExportUtil::GetParamsOfType(GetParType());
    for (size_t i=0; i<pars.size(); ++i)
    {
      if (m_mapParSp.find(pars[i].m_name) == m_mapParSp.end())
      {
        if (parToSkip.GetLength() != 0) parToSkip += " ";
        parToSkip += pars[i].m_name;
      }
    }
    GetGlobal()->SetStrVar("Pars2Skip", parToSkip);
  }
  if (m_mapParSp.size() == 1 &&
      m_mapParSp.begin()->first.Find("_NOPARAM") != -1)
  {
    // delete all the NOPARAM files
    CStr fname;
    for (int i=0; i<GetGlobal()->NumPeriods(); ++i)
    {
      fname = ParSpFileName(m_mapParSp.begin()->first, i+1);
      remove(fname);
    }
    return;
  }
  // write the parameter definitions
  WriteParDefToFile();
  // rewrite file
  WriteLstWithPar();
} // NativeExpLstPack::RewriteFileWithParameters
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::ReadParSpInfo ()
{
  EReadAsciiFile r(ParInfoFileName());
  try
  {
    if (!r.OpenFile()) return;

    int nStress = GetGlobal()->NumPeriods();
    CStr strSp;
    int sp, nParSp, nBc;
    std::map<CStr, std::vector<int> >::iterator it;
    for (int i=0; i<nStress; ++i)
    {
      r.GetLine();
      // read SP card
      r.ReadData(strSp);
      if (strSp != "SP") throw ioError("Error reading SP");
      // read SP number
      r.ReadData(sp);
      if (sp != i+1) throw ioError("Error read SP number");
      // read num par for this sp
      r.ReadData(nParSp);
      for (int j=0; j<nParSp; ++j)
      {
        r.GetLine();
        // read parameter name
        CStr pName;
        r.ReadData(pName);
        // parameter in map if not there
        it = m_mapParSp.find(pName);
        if (it == m_mapParSp.end())
        {
          std::vector<int> vecBcs(nStress, 0);
          m_mapParSp.insert(std::make_pair(pName, vecBcs));
          it = m_mapParSp.find(pName);
        }
        // read number of bcs with this parameter
        r.ReadData(nBc);
        it->second.at(i) = nBc;
      }
      if (nParSp == -1)
      {
        it = m_mapParSp.begin();
        for (; it != m_mapParSp.end(); ++it)
        {
          if (it->second.at(i-1) > 0) it->second.at(i) = -it->second.at(i-1);
          else it->second.at(i) = it->second.at(i-1);
        }
      }
    }
  }
  catch (ioexception&)
  {
  }
  catch (std::out_of_range&)
  {
  }
  r.CloseFile();
  remove(ParInfoFileName().c_str());
} // NativeExpLstPack::ReadParSpInfo
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::ParInfoFileName ()
{
  CStr base = GetNative()->FileName();
  util::StripExtensionFromFilename(base, base);
  CStr fname;
  fname.Format("%s.%s.par.txt", base, GetPackage()->PackageName());
  return fname;
} // NativeExpLstPack::ParInfoFileName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::ParSpFileName (const CStr& a_pName, int a_sp)
{
  CStr base = GetNative()->FileName();
  util::StripExtensionFromFilename(base, base);
  CStr f;
  f.Format("%s.par.%s.SP%d.txt", base, a_pName, a_sp);
  return f;
} // NativeExpLstPack::ParSpFileName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::WriteParDefToFile ()
{
  CStr fname = ParInfoFileName();
  FILE* fpPar = fopen(fname, "w");
  if (!fpPar) return;

  // we will overwrite the ParInfo file with the parameter definitions
  CStr noPar, tmpLn;
  noPar.Format("%s_NOPARAM", GetParType());
  std::vector<CStr> lines, desc;

  std::map<CStr, std::vector<int> >::iterator it(m_mapParSp.begin());
  for (; it != m_mapParSp.end(); ++it)
  {
    if (it->first == noPar) continue;

    if (m_parSp.empty()) m_parSp.assign(it->second.size(), std::vector<CStr>());

    m_NP++;
    // see if we need instances for this parameter
    // if we only have 1 positive value in our vector then we don't need instances
    int nInst(0), nLst(0);
    std::vector<int>& sp(it->second);
    for (size_t i=0; i<sp.size(); ++i)
    {
      if (sp[i] > nLst) nLst = sp[i];
      if (sp[i] > 0) nInst++;
    }
    lines.push_back(Line3(it->first, nLst, nInst));
    desc.push_back(Desc3());
    m_MXL += (nLst * nInst);

    for (size_t i=0; i<sp.size(); ++i)
    {
      AddToParSpVec(it->first, (int)i+1, nInst, sp);

      if (sp[i] < 1) continue;

      if (nInst > 1)
      {
        lines.push_back(Line4a((int)i+1));
        desc.push_back(Desc4a());
      }
      // grab the data from the tmp file and then delete it
      CStr ln, fPar = ParSpFileName(it->first, (int)i+1);
      EReadAsciiFile fp(fPar);
      fp.OpenFile();
      while (fp.GetLine(&ln))
      {
        lines.push_back(ln);
        desc.push_back(Desc4b());
      }
      fp.CloseFile();
      remove(fPar.c_str());

      for (size_t j=0; j<lines.size(); ++j)
      {
        BufferTheLineForComments(lines[j]);
        fprintf(fpPar, "%s# %s\n", lines[j].c_str(), desc[j].c_str());
      }

      // write data to tmp file
      //GetNative()->GetExp()->WriteLinesAndDescriptionsToFile("TMP", lines, desc);
      lines.resize(0);
      desc.resize(0);
    }
  }
  fclose(fpPar);
} // NativeExpLstPack::WriteParDefToFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::AddToParSpVec (CStr a_pName,
                                      int a_sp,
                                      int a_nInst,
                                      std::vector<int>& a_vecSp)
{
  if (a_vecSp[a_sp-1] == 0) return;

  CStr pName = a_pName;
  MfExportUtil::InsertSingleQuotesInName(pName);
  while (pName.GetLength() < 12) pName += " ";
  m_parSp[a_sp-1].push_back(pName);
  if (a_nInst > 1)
  {
    int sp = a_sp;
    if (a_vecSp[a_sp-1] < 0)
    {
      // figure out which stress period we are reusing
      int cnt = a_sp - 2;
      while (cnt > -1)
      {
        if (a_vecSp[cnt] > 0)
        {
          sp = cnt + 1;
          cnt = -1;
        }
        cnt--;
      }
    }
    CStr tmp;
    tmp.Format(" SP_%d", sp);
    m_parSp[a_sp-1].back() += tmp;
  }
} // NativeExpLstPack::AddToParSpVec
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::WriteLstWithPar ()
{
  CStr fname(GetNative()->GetExp()->GetBaseFileName());
  fname += ".";
  fname += GetNative()->GetExp()->GetExtension(GetPackage()->PackageName());
  std::vector<CStr> lines;
  GetCommentsAndLine2(fname, lines);

  FILE *fp = fopen(fname.c_str(), "w");
  if (!fp) return;
  // write the comments
  for (size_t i=0; i<lines.size()-1; i++) fprintf(fp, "%s\n", lines[i].c_str());

  // Get the number of characters to the comments from the tmp par file
  // Write lines 1 and 2
  fprintf(fp, "%s\n", Line1to2WithDesc(lines.back()).c_str());
  // Write parameters
  WriteParToPackage(fp);
  // Write stress periods
  WriteSpWithPar(fp);

  fclose(fp);
} // NativeExpLstPack::WriteLstWithPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Line1to2WithDesc (CStr a_l2)
{
  CStr l1;
  CStr l2Left;
  l2Left = a_l2.Left(a_l2.Find("#"));
  l2Left.TrimRight(" ");
  if (l2Left.GetLength() + 4 > CommentCharPos())
    CommentCharPos(l2Left.GetLength() + 4);
  if (Packages::DRT != GetPackage()->PackageName())
  {
    l1 = Line1a();
    BufferTheLineForComments(l1);
    l1 += "# ";
    l1 += Desc1();
    BufferTheLineForComments(l2Left);
    l2Left += "# ";
    l2Left += Desc2();
    l1 += "\n";
    l1 += l2Left;
  }
  else
  {
    l2Left = a_l2.Left(a_l2.Find("#"));
    a_l2.Replace(l2Left, "");
    l2Left.Trim();
    int i[4];
    std::stringstream ss;
    ss << l2Left;
    ss >> i[0] >> i[1] >> i[2] >> i[3];
    std::getline(ss, l2Left);
    l1.Format("%5d %5d %5d %5d", i[0], i[1], m_NP, m_MXL);
    l1 += l2Left;
    BufferTheLineForComments(l1);
    l1 += a_l2;
  }
  return l1;
} // NativeExpLstPack::Line1to2WithDesc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Line1a ()
{
  CStr rval;
  rval.Format("PARAMETER %5d %5d", m_NP, m_MXL);
  return rval;
} // NativeExpLstPack::Line1a
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Desc1 ()
{
  return " 1. [PARAMETER NPDRN MXL]";
} // NativeExpLstPack::Desc1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Line3 (const CStr& a_pName,
                              int a_nBc,
                              int a_nInst)
{
  CStr tmpLn;
  ParamList* pList;
  Parameters::GetParameterList(&pList);
  if (!pList) return tmpLn;
  Param par;
  if (!pList->FindByName(a_pName, &par)) return tmpLn;
  double pVal = par.m_parVal;
  // write Line 3
  int w = util::RealWidth();
  int flg = STR_FULLWIDTH;
  CStr pName = a_pName;
  CStr type = GetParType();
  while (type.GetLength() < 3) type += " ";
  MfExportUtil::InsertSingleQuotesInName(pName);
  while (pName.GetLength() < 12) pName += " ";
  tmpLn.Format("%s %s %s %5d", pName, type, STR(pVal,-1,w,flg), a_nBc);
  if (a_nInst > 1)
  {
    CStr tmp;
    tmp.Format(" INSTANCES %5d", a_nInst);
    tmpLn += tmp;
  }
  return tmpLn;
} // NativeExpLstPack::WriteParDefToFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Desc3 ()
{
  CStr desc;
  if (Packages::DRT == GetPackage()->PackageName()) desc = " 2. ";
  else desc = " 3. ";
  desc += "[PARNAM PARTYP Parval NLST [INSTANCES NUMINST]]";
  return desc;
} // NativeExpLstPack::Desc3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Line4a (int a_sp)
{
  CStr nm;
  nm.Format("SP_%d", a_sp);
  return nm;
} // NativeExpLstPack::Line4a
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Desc4a ()
{
  CStr desc;
  if (Packages::DRT != GetPackage()->PackageName()) desc = "4a. ";
  else desc = "3a. ";
  desc += "INSTNAM";
  return desc;
} // NativeExpLstPack::Desc4a
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Desc4b ()
{
  CStr s = Desc6();
  s.Replace(" 5", "3b");
  s.Replace(" 6", "4b");
  s.Replace("6a", "4b");
  s.Replace("6b", "4c");
  s.Replace("Cond", "Condfact");
  s.Replace("Q", "Qfact");
  s.Replace("Shead", "Shdfact");
  s.Replace("Ehead", "Ehdfact");
  return s;
} // NativeExpLstPack::Desc4b
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::GetCommentsAndLine2 (const CStr& a_fname,
  std::vector<CStr>& a_lines)
{
  CStr ln;
  EReadAsciiFile af(a_fname);
  af.OpenFile();
  bool done(false);
  while (!done && af.GetLine(&ln))
  {
    if (ln.Find("#  2. ") != -1 ||
        ln.Find("#  1. MXADRT") != -1) done = true;
    a_lines.push_back(ln);
  }
  af.CloseFile();
} // NativeExpLstPack::GetCommentsAndLine2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int NativeExpLstPack::CommentCharPos ()
{
  CStr field;
  field.Format("%s_CommentPos", GetPackage()->PackageName());
  int iVal(0);
  GetGlobal()->GetIntVar(field, iVal);
  return iVal;
} // NativeExpLstPack::CommentCharPos
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::CommentCharPos (int a_)
{
  CStr field;
  field.Format("%s_CommentPos", GetPackage()->PackageName());
  GetGlobal()->SetIntVar(field, a_);
} // NativeExpLstPack::CommentCharPos
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::WriteParToPackage (FILE *a_fp)
{
  if (!a_fp) return;
  CStr fname = ParInfoFileName();
  EReadAsciiFile af(fname);
  af.OpenFile();
  CStr ln;
  while (af.GetLine(&ln))
  {
    fprintf(a_fp, "%s\n", ln.c_str());
  }
  af.CloseFile();
  remove(fname);
} // NativeExpLstPack::WriteParToPackage
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::WriteSpWithPar (FILE *a_fp)
{
  if (!a_fp) return;

  std::vector<int> vecNoPar;
  std::map<CStr, std::vector<int> >::iterator it = m_mapParSp.begin();
  for (; it != m_mapParSp.end(); ++it)
  {
    if (it->first.find("NOPARAM") != -1) vecNoPar = it->second;
  }

  int ITMP(0);
  CStr ln;
  for (size_t i=0; i<m_parSp.size(); ++i)
  {
    ln = Line5WithPar(vecNoPar, (int)i+1, ITMP, (int)m_parSp[i].size());
    fprintf(a_fp, "%s\n", ln.c_str());
    Line6WithPar(a_fp, (int)i+1, ITMP);
    Line7(a_fp, (int)i+1);
  }
} // NativeExpLstPack::WriteSpWithPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstPack::Line5WithPar (std::vector<int>& a_vecNoPar,
                                     int a_sp,
                                     int& a_ITMP,
                                     int a_nPar)
{
  CStr ln, stress;
  stress.Format("          Stress Period %3d", a_sp);
  a_ITMP = 0;
  if (!a_vecNoPar.empty()) a_ITMP = a_vecNoPar[a_sp-1];
  if (a_ITMP < 0) a_ITMP = -1;
  ln.Format("%5d %5d", a_ITMP, a_nPar);
  BufferTheLineForComments(ln);
  if (Packages::DRT == GetPackage()->PackageName()) ln += "#  4. ";
  else ln += "#  5. ";
  ln += "ITMP NP";
  ln += stress;
  return ln;
} // NativeExpLstPack::Line5WithPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::Line6WithPar (FILE *a_fp, int a_sp, int a_ITMP)
{
  if (!a_fp || a_ITMP < 1) return;

  CStr ln, desc, pName; pName.Format("%s_NOPARAM", GetParType());
  CStr fname = ParSpFileName(pName, a_sp);
  EReadAsciiFile af(fname);
  af.OpenFile();
  while (af.GetLine(&ln))
  {
    BufferTheLineForComments(ln);
    ln += "# ";
    ln += Desc6();
    fprintf(a_fp, "%s\n", ln.c_str());
  }
  af.CloseFile();
  remove(fname);
} // NativeExpLstPack::Line6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::Line7 (FILE *a_fp, int a_sp)
{
  for (size_t i=0; i<m_parSp[a_sp-1].size(); ++i)
  {
    CStr ln = m_parSp[a_sp-1][i];
    BufferTheLineForComments(ln);
    if (Packages::DRT == GetPackage()->PackageName()) ln += "#  6. ";
    else ln += "#  7. ";
    ln += "[Pname [Iname]]";
    fprintf(a_fp, "%s\n", ln.c_str());
  }
} // NativeExpLstPack::Line7
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstPack::BufferTheLineForComments (CStr& a_line)
{
  int diff = CommentCharPos() - a_line.GetLength();
  if (diff > 0)
  {
    // buffer the text out to the max
    CStr buff(diff, ' ');
    a_line += buff;
  }
  else if (a_line.GetAt((int)a_line.size()-1) != ' ')
  {
    a_line += " ";
  }
} // NativeExpLstPack::BufferTheLineForComments

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpLstPack.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpLstPackT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::DRN);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpLstPack*>(p);
} // NativeExpLstPackT::setUp
//------------------------------------------------------------------------------
void NativeExpLstPackT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpLstPackT::tearDown
//------------------------------------------------------------------------------
void NativeExpLstPackT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpLstPackT::testCreateClass

#endif
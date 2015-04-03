//------------------------------------------------------------------------------
// FILE      NativeExpStr.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpStr.h>

#include <sstream>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativeExpNam.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>
#include <private\util\EReadAsciiFile.h>

using namespace MfData::Export;
using util::ForElement;

namespace
{

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<Real>& StrCondFact (int a_idx)
{
  static std::map< int, std::vector<Real> > m_map;
  return m_map[a_idx];
} // StrCondFact
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr Desc2 ()
{
  return " 2. MXACTS NSS NTRIB NDIV  ICALC CONST ISTCB1 ISTCB2 Option";
} // Desc2
} // unnamed namespace
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpStr::NativeExpStr () :
  m_mapParKeyVal()
, m_mapParSegKey()
, m_mapParSegInstances()
, m_usg(false)
, m_unstructured(false)
, m_nI(0)
, m_nJ(0)
{
  m_usg = MfData::MfGlobal::Get().ModelType() == MfData::USG;
  if (m_usg) m_unstructured = MfData::MfGlobal::Get().Unstructured() ? 1 : 0;
  m_nI = MfData::MfGlobal::Get().NumRow();
  m_nJ = MfData::MfGlobal::Get().NumCol();
} // MfNativeExpStr::MfNativeExpStr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpStr::~NativeExpStr ()
{
} // MfNativeExpStr::~MfNativeExpStr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpStr::Export ()
{
  if ("STR_CONDFACT" == GetPackage()->PackageName())
  {
    SaveCondFact();
    return true;
  }

  if (1 == GetGlobal()->GetCurrentPeriod())
  {
    Line2();
  }
  Lines5to6();
  Lines8to10();
  CheckParameters();

  TmpPackageNameChanger tmp(GetPackage(), "STR");
  if (1 == GetGlobal()->GetCurrentPeriod()) WriteComments();
  WriteStoredLines();

  if (GetGlobal()->GetCurrentPeriod() == GetGlobal()->NumPeriods())
    LastChanceBeforeWriting();

  return true;
} // MfNativeExpStr::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Line2 ()
{
  const int *istrpb(0), *nss(0), *ntrib(0), *ndiv(0), *icalc(0), *istcb1(0),
            *istcb2(0);
  const Real *constv(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(Packages::STRpack::MXACTS, &istrpb) && istrpb &&
      a_p->GetField(Packages::STRpack::NSS, &nss) && nss &&
      a_p->GetField(Packages::STRpack::NTRIB, &ntrib) && ntrib &&
      a_p->GetField(Packages::STRpack::NDIV, &ndiv) && ndiv &&
      a_p->GetField(Packages::STRpack::ICALC, &icalc) && icalc &&
      a_p->GetField(Packages::STRpack::CONSTV, &constv) && constv &&
      a_p->GetField(Packages::STRpack::ISTCB1, &istcb1) && istcb1 &&
      a_p->GetField(Packages::STRpack::ISTCB2, &istcb2) && istcb2)
  {
    CStr line;
    line.Format("%9d %9d %9d %9d %9d %s %9d %9d", *istrpb-1, *nss, *ntrib,
                *ndiv, *icalc, STR(*constv,-1,9,STR_FULLWIDTH),
                *istcb1, *istcb2);
    AddToStoredLinesDesc(line, Desc2());
    GetGlobal()->SetStrVar("STR_Line2", line);
  }
} // NativeExpStr::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Lines5to6 ()
{
  const int *istrpb(0), *nss(0), *ntrib(0), *ndiv(0), *icalc(0), *istcb1(0),
            *istcb2(0), *itmp(0), *irdflg(0), *iptflg(0), *istrm(0),
            *nstrem(0), *mxstrm(0), *itrbar(0), *idivar(0);
  const Real *constv, *strm(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(Packages::STRpack::MXACTS, &istrpb) && istrpb &&
      a_p->GetField(Packages::STRpack::NSS, &nss) && nss &&
      a_p->GetField(Packages::STRpack::NTRIB, &ntrib) && ntrib &&
      a_p->GetField(Packages::STRpack::NDIV, &ndiv) && ndiv &&
      a_p->GetField(Packages::STRpack::ICALC, &icalc) && icalc &&
      a_p->GetField(Packages::STRpack::CONSTV, &constv) && constv &&
      a_p->GetField(Packages::STRpack::ISTCB1, &istcb1) && istcb1 &&
      a_p->GetField(Packages::STRpack::ISTCB2, &istcb2) && istcb2 &&
      a_p->GetField(Packages::STRpack::ITMP, &itmp) && itmp &&
      a_p->GetField(Packages::STRpack::IRDFLG, &irdflg) && irdflg &&
      a_p->GetField(Packages::STRpack::IPTFLG, &iptflg) && iptflg &&
      a_p->GetField(Packages::STRpack::STRM, &strm) && strm &&
      a_p->GetField(Packages::STRpack::ISTRM, &istrm) && istrm &&
      a_p->GetField(Packages::STRpack::NSTREM, &nstrem) && nstrem &&
      a_p->GetField(Packages::STRpack::MXSTRM, &mxstrm) && mxstrm &&
      a_p->GetField(Packages::STRpack::ITRBAR, &itrbar) && itrbar &&
      a_p->GetField(Packages::STRpack::IDIVAR, &idivar) && idivar)
  {
    CStr line;
    CStr desc = " 5. ITMP IRDFLG IPTFLG";
    line.Format("%9d %9d %9d", *itmp, *irdflg, *iptflg);
    AddToStoredLinesDesc(line, desc);
    // store variables
    std::stringstream ss;
    ss << *irdflg << " " << *iptflg << " ";
    CStr myStr;
    GetGlobal()->GetStrVar("STR_LN_5", myStr);
    myStr += ss.str();
    GetGlobal()->SetStrVar("STR_LN_5", myStr);

    desc = " 6. Layer Row Col Seg Reach Flow Stage Cond Sbot Stop [xyz]";
    if (m_usg)
    {
      desc.Replace(" 6", "6a");
      if (m_unstructured) desc.Replace("6a. Layer Row Col", "6b. Node");
    }
    for (int i=1; i<=*itmp; ++i)
    {
      line = Line6FromData(i, istrm, strm);
      AddToStoredLinesDesc(line, desc);
    }
  }
} // NativeExpStr::Lines5to6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpStr::KijStr (int ck, int ci, int cj)
{
  CStr s;
  if (!m_unstructured) s.Format("%4d %4d %4d", ck, ci, cj);
  else                 s.Format("%4d", ck);
  return s;
} // NativeExpStr::KijStr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpStr::Line6FromData (int i,
                                  const int* istrm,
                                  const Real* strm)
{
  bool isMf2k = GetGlobal()->ModelType() == MfData::MF2K;
  int ck, ci, cj, seg, reach, condInt, flg = STR_FULLWIDTH;
  Real flow, stage, cond, sbot, stop;
  CStr flowStr, line;
  std::map<int, double>& mapPar = GetParamMap();

  ck    = ForElement(istrm, 1, i, 5);
  if (m_usg && !m_unstructured)
  {
    ck = ck / (m_nI*m_nJ) + 1;
  }
  ci    = ForElement(istrm, 2, i, 5);
  cj    = ForElement(istrm, 3, i, 5);
  seg   = ForElement(istrm, 4, i, 5);
  reach = ForElement(istrm, 5, i, 5);
  flow  = ForElement(strm, 1, i, 11);
  stage = ForElement(strm, 2, i, 11);
  cond  = ForElement(strm, 3, i, 11);
  sbot  = ForElement(strm, 4, i, 11);
  stop  = ForElement(strm, 5, i, 11);
  flowStr = "              ";
  if (1 == reach || !isMf2k) flowStr.Format("%s", STR(flow,-1,14,flg));
  line.Format("%s %4d %4d %s %s %s %s %s", KijStr(ck, ci, cj), seg, reach,
              flowStr, STR(stage,-1,9,flg), STR(cond,-1,9,flg),
              STR(sbot,-1,9,flg), STR(stop,-1,9,flg));
  condInt = static_cast<int>(cond);
  if (mapPar.find(condInt) != mapPar.end())
  {
    if (m_mapParSegKey.find(seg) == m_mapParSegKey.end())
    {
      m_mapParSegKey[seg] = condInt;
    }
  }
  return line;
} // NativeExpStr::Line6FromData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Lines8to10 ()
{
  const int *istrpb(0), *nss(0), *ntrib(0), *ndiv(0), *icalc(0), *istcb1(0),
            *istcb2(0), *itmp(0), *irdflg(0), *iptflg(0), *istrm(0),
            *nstrem(0), *mxstrm(0), *itrbar(0), *idivar(0);
  const Real *constv, *strm(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(Packages::STRpack::MXACTS, &istrpb) && istrpb &&
      a_p->GetField(Packages::STRpack::NSS, &nss) && nss &&
      a_p->GetField(Packages::STRpack::NTRIB, &ntrib) && ntrib &&
      a_p->GetField(Packages::STRpack::NDIV, &ndiv) && ndiv &&
      a_p->GetField(Packages::STRpack::ICALC, &icalc) && icalc &&
      a_p->GetField(Packages::STRpack::CONSTV, &constv) && constv &&
      a_p->GetField(Packages::STRpack::ISTCB1, &istcb1) && istcb1 &&
      a_p->GetField(Packages::STRpack::ISTCB2, &istcb2) && istcb2 &&
      a_p->GetField(Packages::STRpack::ITMP, &itmp) && itmp &&
      a_p->GetField(Packages::STRpack::IRDFLG, &irdflg) && irdflg &&
      a_p->GetField(Packages::STRpack::IPTFLG, &iptflg) && iptflg &&
      a_p->GetField(Packages::STRpack::STRM, &strm) && strm &&
      a_p->GetField(Packages::STRpack::ISTRM, &istrm) && istrm &&
      a_p->GetField(Packages::STRpack::NSTREM, &nstrem) && nstrem &&
      a_p->GetField(Packages::STRpack::MXSTRM, &mxstrm) && mxstrm &&
      a_p->GetField(Packages::STRpack::ITRBAR, &itrbar) && itrbar &&
      a_p->GetField(Packages::STRpack::IDIVAR, &idivar) && idivar)
  {
    Lines8to10FromData(itmp, icalc, nss, ntrib, ndiv, strm, itrbar, idivar);
  }

} // NativeExpStr::Lines8to10
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Lines8to10FromData (const int* itmp, const int* icalc,
                                       const int* nss, const int* ntrib,
                                       const int* ndiv, const Real* strm,
                                       const int *itrbar, const int* idivar)
{
  if (*itmp < 1) return;
  CStr line;
  CStr desc = " 8. Width Slope Rough";
  if (*icalc > 0)
  {
    int flg = STR_FULLWIDTH;
    Real width, slope, rough;
    for (int i=1; i<=*itmp; ++i)
    {
      width  = ForElement(strm, 6, i, 11);
      slope  = ForElement(strm, 7, i, 11);
      rough  = ForElement(strm, 8, i, 11);
      line.Format("%s %s %s", STR(width,-1,9,flg), STR(slope,-1,9,flg),
        STR(rough,-1,9,flg));
      AddToStoredLinesDesc(line,desc);
    }
  }
  if (*ntrib > 0)
  {
    desc = " 9. Itrib(NTRIB)";
    for (int i=1; i<=*nss; ++i)
    {
      line = "";
      for (int j=1; j<=*ntrib; ++j)
      {
        CStr tmp;
        tmp.Format("%4d ", ForElement(itrbar, i, j, *nss));
        line += tmp;
      }
      AddToStoredLinesDesc(line, desc);
    }
  }
  if (*ndiv > 0)
  {
    desc = "10. Iupseg";
    for (int i=0; i<*nss; ++i)
    {
      line.Format("%9d", idivar[i]);
      AddToStoredLinesDesc(line, desc);
    }
  }
} // NativeExpStr::Lines8to10FromData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::SaveCondFact ()
{
  const Real* condfact(0);
  const int*  nval(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField("CONDFACT", &condfact) && condfact &&
      a_p->GetField("NVAL", &nval) && nval)
  {
    int idx = (int)GetGlobal()->CurModIdx();
    std::vector<Real>& vec = StrCondFact(idx);
    vec.resize(*nval);
    for (size_t i=0; i<vec.size(); ++i) vec[i] = condfact[i];
  }
} // NativeExpStr::SaveCondFact
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::LastChanceBeforeWriting ()
{
  CStr myStr;
  GetGlobal()->GetStrVar("STR_PAR_SEG", myStr);
  if (myStr.IsEmpty())
  {
    remove(StrTmpFileName());
    Par_SetToSkipExistingStrParams();
    return;
  }

  GetParSegForAllSp();
  // this gives us the number of parameters and how the segments can be
  // combined with the parameters
  std::vector<int> pSeg;
  CreateParSegVector(pSeg);
  WriteParDefAndStressToTmp(pSeg);
  WriteStrFile();
} // NativeExpStr::LastChanceBeforeWriting
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::GetParSegForAllSp ()
{
  CStr myStr;
  GetGlobal()->GetStrVar("STR_PAR_SEG", myStr);
  std::stringstream ss;
  ss << myStr;
  int seg, key;
  while (ss.good())
  {
    ss >> seg >> key;
    if (m_mapParSegKey.find(seg) == m_mapParSegKey.end())
    {
      m_mapParSegKey[seg] = key;
    }
  }
} // NativeExpStr::GetParSegForAllSp
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::CreateParSegVector (std::vector<int>& a_)
{
  a_.clear();
  std::map<int, int>::iterator it = m_mapParSegKey.begin();
  for (; it != m_mapParSegKey.end(); ++it)
  {
    a_.push_back(it->first);
  }
} // NativeExpStr::CreateParSegVector
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::CheckParameters ()
{
  std::vector<Param> params = MfExportUtil::GetParamsOfType("STR");
  if (params.empty()) return;

  MfPackage* a_p = GetPackage();
  const int *itmp, *icalc, *nss, *ntrib, *ndiv, *istrm, *itrbar, *idivar;
  const Real *strm;
  if (a_p->GetField(Packages::STRpack::ITMP, &itmp) && itmp &&
      a_p->GetField(Packages::STRpack::ICALC, &icalc) && icalc &&
      a_p->GetField(Packages::STRpack::NSS, &nss) && nss &&
      a_p->GetField(Packages::STRpack::NTRIB, &ntrib) && ntrib &&
      a_p->GetField(Packages::STRpack::NDIV, &ndiv) && ndiv &&
      a_p->GetField(Packages::STRpack::STRM, &strm) && strm &&
      a_p->GetField(Packages::STRpack::ISTRM, &istrm) && istrm &&
      a_p->GetField(Packages::STRpack::ITRBAR, &itrbar) && itrbar &&
      a_p->GetField(Packages::STRpack::IDIVAR, &idivar) && idivar)
  {
    CStr wFlg("ab");
    if (GetGlobal()->GetCurrentPeriod() == 1) wFlg = "wb";
    FILE *fp = fopen(StrTmpFileName().c_str(), wFlg.c_str());
    int cnt = *itmp;
    fwrite(itmp, sizeof(int), 1, fp);
    fwrite(icalc, sizeof(int), 1, fp);
    fwrite(nss, sizeof(int), 1, fp);
    fwrite(ntrib, sizeof(int), 1, fp);
    fwrite(ndiv, sizeof(int), 1, fp);
    if (cnt > 0)
    {
      fwrite(istrm, sizeof(int), cnt*5, fp);
      fwrite(strm, sizeof(Real), cnt*11, fp);
      std::vector<Real>& vec = StrCondFact((int)GetGlobal()->CurModIdx());
      if ((int)vec.size() != cnt)
      {
        //ASSERT(0);
        vec.resize(cnt);
      }
      fwrite(&vec[0], sizeof(Real), cnt, fp);
    }
    if (*nss * *ntrib > 0) fwrite(itrbar, sizeof(int), *nss * *ntrib, fp);
    if (*ndiv > 0 && *nss > 0) fwrite(idivar, sizeof(int), *nss, fp);
    fclose(fp);
  }
  std::stringstream ss;
  std::map<int, int>::iterator it = m_mapParSegKey.begin();
  for (; it != m_mapParSegKey.end(); ++it)
  {
    ss << it->first << " " << it->second << " ";
  }
  CStr myStr;
  GetGlobal()->GetStrVar("STR_PAR_SEG", myStr);
  myStr += ss.str();
  GetGlobal()->SetStrVar("STR_PAR_SEG", myStr);
} // NativeExpStr::CheckParameters
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpStr::StrTmpFileName ()
{
  CStr base = GetNative()->FileName();
  util::StripExtensionFromFilename(base, base);
  CStr fname;
  fname.Format("%s.%s.tmp", base, GetPackage()->PackageName());
  return fname;
} // NativeExpStr::StrTmpFileName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::map<int, double>& NativeExpStr::GetParamMap ()
{
  if (m_mapParKeyVal.empty())
  {
    std::vector<Param> params = MfExportUtil::GetParamsOfType("STR");
    for (size_t i=0; i<params.size(); ++i)
    {
      m_mapParKeyVal[static_cast<int>(params[i].m_key)] = params[i].m_value;
    }
  }
  return m_mapParKeyVal;
} // NativeExpStr::GetParamMap
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::WriteParDefAndStressToTmp (std::vector<int>& a_parSeg)
{
  // open the tmp file with the stream data
  FILE *fp = fopen(StrTmpFileName().c_str(), "rb");
  if (!fp) return;

  // set up the num instances for each segment parameter
  for (size_t j=0; j<a_parSeg.size(); ++j)
  {
    m_mapParSegInstances[a_parSeg[j]] = 0;
  }

  // loop through num stress
  int nstress = GetGlobal()->NumPeriods();
  int itmp, icalc, nss, ntrib, ndiv;
  std::vector<int> istrm, itrbar, idivar;
  std::vector<Real> strm;
  std::vector<CStr> vecStr(a_parSeg.size(), "");
  for (int i=0; i<nstress; ++i)
  {
    // read stream data
    ReadStrDataFromTmp(fp, itmp, icalc, nss, ntrib, ndiv, istrm, strm,
                       itrbar, idivar);
    AdjustStrDataUsingParams(itmp, istrm, strm);
    // create parameters
    int cnt(0), nextSeg;
    CStr line, pName = "S_1";
    for (size_t j=0; j<a_parSeg.size(); ++j)
    {
      line = "";
      nextSeg = -1;
      if (j+1<a_parSeg.size()) nextSeg = a_parSeg[j+1];

      line = Lines6ForPar(cnt, nextSeg, itmp, istrm, strm);

      if (line == vecStr[j] || itmp < 0)
      { // parameter definition is the same as last stress period
        line = "";
      }
      else
      {
        vecStr[j] = line;
        m_mapParSegInstances[a_parSeg[j]] += 1;
      }

      StoreParDef(line, pName, i+1, a_parSeg[j]);

      pName.Format("S_%d", nextSeg);
    }
    // store lines 8to10 for this stress period
    Store8to10ForPar(i+1, itmp, icalc, nss, ntrib, ndiv,
                     istrm, strm, itrbar, idivar);
  }
  fclose(fp);
  remove(StrTmpFileName().c_str());
} // NativeExpStr::WriteParDefAndStressToTmp
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpStr::Lines6ForPar (int& cnt, int nextSeg, int itmp,
                                 std::vector<int>& istrm,
                                 std::vector<Real>& strm)
{
  CStr line, line1;

  int seg = ForElement(&istrm[0], 4, cnt+1, 5);
  while (cnt < itmp && seg != nextSeg)
  {
    line1 = Line6FromData(cnt+1, &istrm[0], &strm[0]);
    line1 += "\n";
    line += line1;
    cnt++;
    seg = ForElement(&istrm[0], 4, cnt+1, 5);
  }
  return line;
} // NativeExpStr::Line6ForPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::StoreParDef (CStr& line, CStr& pName, int a_sp, int a_seg)
{
  FILE *fp = ParTmpFilePtr(pName, a_sp, false);
  if (!fp) return;
  size_t cnt = std::count(line.begin(), line.end(), '\n');
  if (m_mapParSegNlst.find(a_seg) == m_mapParSegNlst.end() ||
      m_mapParSegNlst[a_seg] < (int)cnt)
  {
    m_mapParSegNlst[a_seg] = (int)cnt;
  }
  fprintf(fp, "%d\n", cnt);
  fprintf(fp, "%s", line.c_str());
  fclose(fp);
} // NativeExpStr::StoreParDef
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::ReadStrDataFromTmp (FILE *fp,
                                       int& itmp,
                                       int& icalc,
                                       int& nss,
                                       int& ntrib,
                                       int& ndiv,
                                       std::vector<int>& istrm,
                                       std::vector<Real>& strm,
                                       std::vector<int>& itrbar,
                                       std::vector<int>& idivar)
{
  // read stream data
  fread(&itmp, sizeof(int), 1, fp);
  fread(&icalc, sizeof(int), 1, fp);
  fread(&nss, sizeof(int), 1, fp);
  fread(&ntrib, sizeof(int), 1, fp);
  fread(&ndiv, sizeof(int), 1, fp);
  if (itmp > (int)istrm.size()) istrm.resize(itmp*5);
  if (itmp > (int)strm.size()) strm.resize(itmp*11);
  if (itmp > 0)
  {
    fread(&istrm[0], sizeof(int), istrm.size(), fp);
    fread(&strm[0], sizeof(Real), strm.size(), fp);
    std::vector<Real>& vec = StrCondFact((int)GetGlobal()->CurModIdx());
    if ((int)vec.size() != itmp)
    {
      ASSERT(0);
      vec.resize(itmp);
    }
    fread(&vec[0], sizeof(Real), vec.size(), fp);
  }
  if (nss*ntrib > (int)itrbar.size()) itrbar.resize(nss*ntrib);
  if (nss*ntrib > 0) fread(&itrbar[0], sizeof(int), itrbar.size(), fp);
  if (nss > (int)idivar.size()) idivar.resize(nss);
  if (ndiv > 0 && nss > 0) fread(&idivar[0], sizeof(int), idivar.size(), fp);
} // NativeExpStr::ReadStrDataFromTmp
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpStr::ParTmpFileName (CStr a_pName)
{
  CStr base = GetNative()->FileName();
  util::StripExtensionFromFilename(base, base);
  CStr fname;
  fname.Format("%s.%s.%s.txt", base, GetPackage()->PackageName(), a_pName);
  return fname;
} // NativeExpStr::ParTmpFileName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
FILE* NativeExpStr::ParTmpFilePtr (CStr& a_pName, int a_sp, bool a_readonly)
{
  CStr fname = ParTmpFileName(a_pName);
  CStr wFlg("a");
  if (a_sp == 1) wFlg = "w";
  if (a_readonly) wFlg = "r";
  FILE *fp = fopen(fname.c_str(), wFlg.c_str());
  return fp;
} // NativeExpStr::ParTmpFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Store8to10ForPar (int sp, int itmp, int icalc, int nss,
                                     int ntrib, int ndiv,
                                     std::vector<int>& istrm,
                                     std::vector<Real>& strm,
                                     std::vector<int>& itrbar,
                                     std::vector<int>& idivar)
{
  Lines8to10FromData(&itmp, &icalc, &nss, &ntrib, &ndiv,
                     &strm[0], &itrbar[0], &idivar[0]);
  std::vector<CStr>& lines(GetPackage()->StringsToWrite());
  std::vector<CStr>& descs(GetPackage()->StringDescriptions());
  CStr fname = ParTmpFileName("8to10");
  CStr wFlg("a");
  if (sp == 1) wFlg = "w";
  FILE *fp = fopen(fname.c_str(), wFlg.c_str());

  CStr line;
  int len = GetNative()->GetExp()->GetMaxLineLengthFromType(Packages::STRSP);
  fprintf(fp, "%d\n", lines.size());
  for (size_t i=0; i<lines.size(); ++i)
  {
    line = lines[i];
    int size = len-line.GetLength();
    if (size < 0) size = 0;
    CStr buff(size, ' ');
    line += buff;
    line += " # ";
    line += descs[i];
    fprintf(fp, "%s\n", line.c_str());
  }
  fclose(fp);
  lines.clear();
  descs.clear();
} // NativeExpStr::Store8to10ForPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::AdjustStrDataUsingParams (int itmp,
                                             std::vector<int>& istrm,
                                             std::vector<Real>& strm)
{
  if (itmp < 0) return;

  std::vector<Real>& condfact = StrCondFact((int)GetGlobal()->CurModIdx());
  if ((int)condfact.size() > itmp)
  {
    ASSERT(0);
    return;
  }
  std::map<int, double>& pMap = GetParamMap();
  for (int i=0; i<itmp; ++i)
  {
    int seg = ForElement(&istrm[0], 4, i+1, 5);
    Real cond = ForElement(&strm[0], 3, i+1, 11);
    int icond = static_cast<int>(cond);
    Real condF = condfact[i];

    // get parameter associated with this segment
    std::map<int, int>::iterator it = m_mapParSegKey.lower_bound(seg);
    if (it->first != seg) --it;
    // parameter value associated with this segment
    double pval = pMap[it->second];

    // the conductance is from a parameter
    if (pMap.find(icond) != pMap.end())
    {
      if (it->second != seg)
      { // The conductance comes from a parameter that is not associated with
        // this segment. You can only have 1 parameter per segment.
        condF = (Real)((pMap[icond] * condfact[i]) / pval);
      }
    }
    else
    { // the conductance is specified and we need to divide by the parameter
      // value so that we will have the same final conductance when this item
      // is associated with a parameter
      condF = cond / (Real)pval;
    }
    // assign the conductance factor to conductance
    ForElement(&strm[0], 3, i+1, 11) = condF;
  }
} // NativeExpStr::AdjustStrDataUsingParams
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::WriteStrFile ()
{
  std::vector<CStr>& lines = GetPackage()->StringsToWrite();
  std::vector<CStr>& desc =  GetPackage()->StringDescriptions();
  lines.clear();
  desc.clear();

  ClearFile();
  WriteComments();
  Par_Lines1to2();
  std::vector< std::vector<CStr> > spLines, prev;
  Par_Lines3to4(spLines);
  std::vector< std::vector<int> > iData;
  Par_Line5Data(iData);
  EReadAsciiFile aFile(ParTmpFileName("8to10"));
  aFile.OpenFile();
  int nStress = GetGlobal()->NumPeriods();
  for (int i=0; i<nStress; ++i)
  {
    Par_Line5(i, spLines, iData);
    Par_Line7(i, spLines);
    Par_Lines8to10(aFile, prev);
    WriteStoredLines();
  }
  aFile.CloseFile();
  remove(ParTmpFileName("8to10"));
} // NativeExpStr::WriteStrFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Par_Lines1to2 ()
{
  CStr line;
  int npar = (int)m_mapParSegKey.size();
  int mxl(0);
  std::map<int, int>::iterator it = m_mapParSegInstances.begin();
  for (; it != m_mapParSegInstances.end(); ++it)
  {
    mxl += it->second * m_mapParSegNlst[it->first];
  }
  line.Format("PARAMETER %5d %5d", npar, mxl);
  AddToStoredLinesDesc(line.c_str(), " 1. [PARAMETER NPSTR MXL]");
  // line 2
  GetGlobal()->GetStrVar("STR_Line2", line);
  AddToStoredLinesDesc(line.c_str(), Desc2());
} // NativeExpStr::Par_Lines1to2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Par_Lines3to4 (std::vector< std::vector<CStr> >& a_spLines)
{
  // delete all stream parameters from the parameter list
  Par_SetToSkipExistingStrParams();
  ParamList* pList(0);
  Parameters::GetParameterList(&pList);

  int nStress = GetGlobal()->NumPeriods();
  a_spLines.assign(nStress, std::vector<CStr>());
  int w = 14;
  int flg = STR_FULLWIDTH;
  CStr line, desc;
  // loop through the parameters
  std::map<int, int>::iterator it = m_mapParSegInstances.begin();
  for (; it != m_mapParSegInstances.end(); ++it)
  {
    int ninst = it->second;
    int nlst = m_mapParSegNlst[it->first];
    double pval = m_mapParKeyVal[m_mapParSegKey[it->first]];
    CStr pName; pName.Format("S_%d", it->first);
    Param p(pName, pList->UnusedParamKey(), "STR", pval);
    p.m_b = pval;
    pList->PushBack(&p);
    // add to the PVAL file
    MfPackage* pack = GetGlobal()->GetPackage(Packages::PVAL);
    if (pack)
    {
      CStr desc = pack->StringDescriptions().back();
      CStr line;
      CStr parName = pName;
      MfExportUtil::InsertSingleQuotesInName(parName);
      while (parName.GetLength() < 12) parName += " ";
      line.Format("%s %s", parName, STR(p.m_b));
      pack->StringsToWrite().push_back(line);
      pack->StringDescriptions().push_back(desc);
    }


    line.Format("%s STR %s %5d ", pName, STR(pval,-1,w,flg), nlst);
    if (ninst > 1)
    {
      CStr l;
      l.Format("INSTANCES %5d", ninst);
      line += l;
    }
    desc = " 3. [PARNAM PARTYP Parval NLST [INSTANCES NUMINST]]";
    AddToStoredLinesDesc(line, desc);

    CStr tmpFname = ParTmpFileName(pName);
    EReadAsciiFile r(tmpFname);
    r.OpenFile();
    for (int i=0; i < nStress; ++i)
    {
      a_spLines[i].push_back(pName);
      r.GetLine();
      int nline;
      r.ReadData(nline);
      if (nline > 0)
      {
        CStr instName;
        instName.Format("SP_%d", i+1);
        if (ninst > 1)
        {
          AddToStoredLinesDesc(instName, "4a. [INSTNAME]");
          a_spLines[i].back() += " ";
          a_spLines[i].back() += instName;
        }
        desc = "4b. Layer Row Col Seg Reach Flow Stage Condfact Sbot Stop [xyz]";
        if (m_unstructured) desc.Replace("4b. Layer Row Col", "4c. Node");
        for (int j=0; j<nline; ++j)
        {
          r.GetLine(&line);
          AddToStoredLinesDesc(line, desc);
        }
        WriteStoredLines();
      }
      else
      {
        a_spLines[i].back() = a_spLines[i-1].back();
      }
    }
    r.CloseFile();
    remove(tmpFname);
  }
} // NativeExpStr::Par_Lines3to4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Par_Line5Data (std::vector< std::vector<int> >& a_iData)
{
  int nStress = GetGlobal()->NumPeriods();
  a_iData.assign(nStress, std::vector<int>());
  CStr myStr;
  GetGlobal()->GetStrVar("STR_LN_5", myStr);
  std::stringstream ss;
  ss << myStr;
  int irdflg, iptflg;
  for (int i=0; i<nStress; ++i)
  {
    ss >> irdflg >> iptflg;
    a_iData[i].push_back(irdflg);
    a_iData[i].push_back(iptflg);
  }
} // NativeExpStr::Par_Line5Data
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Par_Line5 (int a_spIdx,
                               std::vector< std::vector<CStr> >& a_spLines,
                               std::vector< std::vector<int> >& a_iData)
{
  int itmp = (int)a_spLines[a_spIdx].size();
  int irdflg = a_iData[a_spIdx][0];
  int iptflg = a_iData[a_spIdx][0];
  CStr str;
  str.Format("%5d %5d %5d", itmp, irdflg, iptflg);
  AddToStoredLinesDesc(str, " 5. ITMP IRDFLG IPTFLG");
} // NativeExpStr::Par_Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Par_Line7 (int a_spIdx,
                              std::vector< std::vector<CStr> >& a_spLines)
{
  for (size_t i=0; i<a_spLines[a_spIdx].size(); ++i)
  {
    AddToStoredLinesDesc(a_spLines[a_spIdx][i], " 7. [Pname [Iname]]");
  }
} // NativeExpStr::Par_Line7
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Par_Lines8to10 (EReadAsciiFile& a_File,
                                   std::vector< std::vector<CStr> >& a_prev)
{
  CStr tmp, line;
  int nLines;
  a_File.GetLine();
  a_File.ReadData(nLines);
  if (nLines < 1)
  {
    for (size_t i=0; i<a_prev[0].size(); ++i)
      AddToStoredLinesDesc(a_prev[0][i], a_prev[1][i]);
  }
  else
  {
    a_prev.assign(2, std::vector<CStr>());
    for (int i=0; i<nLines; ++i)
    {
      a_File.GetLine(&tmp);
      line = tmp.Left(tmp.Find("#"));
      tmp.Replace(line, "");
      tmp.Replace("# ", "");
      line.TrimRight();
      AddToStoredLinesDesc(line, tmp);
      a_prev[0].push_back(line);
      a_prev[1].push_back(tmp);
    }
  }
} // NativeExpStr::Par_Lines8to10
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpStr::Par_SetToSkipExistingStrParams ()
{
  ParamList* pList(0);
  Parameters::GetParameterList(&pList);
  CStr parToSkip;
  GetGlobal()->GetStrVar("Pars2Skip", parToSkip);
  std::vector<Param> pars = MfExportUtil::GetParamsOfType("STR");
  for (size_t i=0; i<pars.size(); ++i)
  {
    if (parToSkip.GetLength() != 0) parToSkip += " ";
    parToSkip += pars[i].m_name;
  }
  GetGlobal()->SetStrVar("Pars2Skip", parToSkip);
} // NativeExpStr::Par_SetToSkipExistingStrParams

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpStr.t.h>


using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpStrT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::STRSP);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpStr*>(p);
} // NativeExpStrT::setUp
//------------------------------------------------------------------------------
void NativeExpStrT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpStrT::tearDown
//------------------------------------------------------------------------------
void NativeExpStrT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpStrT::testCreateClass

#endif
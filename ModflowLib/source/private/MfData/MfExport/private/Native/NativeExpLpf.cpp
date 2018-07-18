//------------------------------------------------------------------------------
// FILE      NativeExpLpf.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpLpf.h>

#include <sstream>

#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Ic.h>
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Sto.h>
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Npf.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
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
static int iCountLpfParams (ParamList* a_list, bool a_checkClusters)
{
  Param          p;
  CStr           pType;
  std::set<CStr> lpfParTypes = MfExportUtil::LpfParamTypes();

  int npar = 0;
  for (size_t i=0; i<a_list->Size(); i++)
  {
    a_list->At(i, &p);
    pType = p.m_type;
    pType.ToLower();
    if (lpfParTypes.find(pType) != lpfParTypes.end())
    {
      if (!a_checkClusters && p.m_pilotPoints)
      {
        std::vector<double> vals;
        a_list->GetPilotPtValues(p.m_scatIndex, vals);
        npar += (int)vals.size();
      }
      else if (!p.m_clust.empty()) npar++;
    }
  }
  return npar;
} // iCountLpfParams
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLpf::NativeExpLpf () :
  m_nLay(0)
, m_nPar(0)
, m_ikcflag(-999)
, m_usg(0)
, m_unstructured(0)
, m_stacked(0)
, m_anyChaniNotOne(0)
, m_useStorageCoefficent(false)
, m_mf6(false)
{
  m_usg = MfExportUtil::MfIsUsgModelType();
  if (m_usg)
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
} // MfNativeExpLpf::MfNativeExpLpf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLpf::~NativeExpLpf ()
{
} // MfNativeExpLpf::~MfNativeExpLpf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpLpf::Export ()
{

  if ("L98" == GetPackage()->PackageName())
  {
    SetData(GetNative(), GetGlobal(), GetGlobal()->GetPackage(Packages::LPF));
  }

  if (m_mf6)
  {
    NativeExpMf6Npf npf(this);
    npf.Export();
    NativeExpMf6Sto sto(this);
    sto.Export();   
    return true;
  }

  AddToStoredLinesDesc(Line1(), Desc(1));
  AddToStoredLinesDesc(Line2to6(2), Desc(2));
  AddToStoredLinesDesc(Line2to6(3), Desc(3));
  AddToStoredLinesDesc(Line2to6(4), Desc(4));
  AddToStoredLinesDesc(Line2to6(5), Desc(5));
  AddToStoredLinesDesc(Line2to6(6), Desc(6));
  if (CanWriteLine(7)) AddToStoredLinesDesc(Line7(), Desc(7));
  if (CanWriteLine(8)) WriteLpfParams();
  if (m_unstructured && m_ikcflag == 0 && m_anyChaniNotOne) Line10Usg();
  for (int k=1; k<=m_nLay; ++k)
  {
    Line10to16(10, k);
    if (CanWriteLine(11,k)) Line10to16(11,k);
    Line12(k);
    if (CanWriteLine(13,k)) Line10to16(13,k);
    if (CanWriteLine(14,k)) Line10to16(14,k);
    if (CanWriteLine(15,k)) Line10to16(15,k);
    if (CanWriteLine(16,k)) Line10to16(16,k);
  }

  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpLpf::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLpf::OnSetData ()
{
  m_nLay = GetGlobal()->NumLay();
  ParamList *list;
  Parameters::GetParameterList(&list);
  bool flag(false);
  if (GetH5Flag()) flag = true;
  m_nPar = iCountLpfParams(list, flag);
  Mf2kNative* n = GetNative();
  if (n && n->GetExportMf6())
  {
    m_mf6 = true;
  }
} // MfNativeExpLpf::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLpf::Desc (int a_line)
{
  CStr desc[16] = {" 1. ILPFCB HDRY NPLPF [Options]",
                   " 2. Ltype(NLAY)",
                   " 3. LAYAVG(NLAY)",
                   " 4. CHANI(NLAY)",
                   " 5. LAYVKA(NLAY)",
                   " 6. LAYWET(NLAY)",
                   " 7. [WETFCT IWETIT IHDWET]",
                   " 8. [PARNAM PARTYP Parval NCLU]",
                   " 9. [Layer Mltarr Zonarr IZ]",
                   "10. HK(NCOL,NROW)        LAY ",
                   "11. [HANI(NCOL,NROW)]    LAY ",
                   "12. VKA(NCOL,NROW)       LAY ",
                   "13. [Ss(NCOL,NROW)]      LAY ",
                   "14. [Sy(NCOL,NROW)]      LAY ",
                   "15. [VKCB(NCOL,NROW)]    LAY ",
                   "16. [WETDRY(NCOL,NROW)]  LAY "
                   };
  if (Packages::UPW == GetPackage()->PackageName())
  {
    desc[0] = " 1. IUPWCB HDRY NPUPW IPHDRY [Options]";
  }
  if (m_usg) desc[0].Replace(" 1", "1a");

  if (!m_unstructured || (a_line > 1 && a_line < 10) ) return desc[a_line-1];

  CStr descUsg[17] = {"1b. ILPFCB HDRY NPLPF IKCFLAG [Options]",
                      "","","","","","","","",
                      "11. HK(NDSLAY)        LAY ",
                      "12. [HANI(NDSLAY)]    LAY ",
                      "13. VKA(NDSLAY)       LAY ",
                      "14. [Ss(NDSLAY)]      LAY ",
                      "15. [Sy(NDSLAY)]      LAY ",
                      "16. [VKCB(NDSLAY)]    LAY ",
                      "17. [WETDRY(NDSLAY)]  LAY ",
                      "10. [ANGLEX(NJAG)]"
                      };
  return descUsg[a_line-1];
} // NativeExpLpf::Desc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLpf::Desc (int a_line, int a_lay)
{
  std::stringstream ss;
  ss << Desc(a_line);
  if (-1 != a_lay)
  {
    ss << a_lay;
  }
  return ss.str();
} // NativeExpLpf::Desc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLpf::Line1 ()
{
  CStr rval;
  const Real *hdry(0);
  const int  *vleak(0), *mf2k5(0);
  const int  *ILPFCB, *ISFAC(0), *ICONCV(0), *ITHFLG(0), *NOCVCO(0), *NOVFC(0),
             *IPHDRY(0);

  MfPackage* p = GetPackage();
  if (Packages::UPW == p->PackageName())
  {
    if (!p->GetField(Packages::UPWpack::IUPWCB, &ILPFCB) || !ILPFCB ||
        !p->GetField(Packages::LPFpack::HDRY, &hdry) || !hdry ||
        !p->GetField(Packages::UPWpack::IPHDRY, &IPHDRY) || !IPHDRY)
      return rval;
  }
  else
  {
    if (!p->GetField(Packages::LPFpack::ILPFCB, &ILPFCB) || !ILPFCB ||
        !p->GetField(Packages::LPFpack::HDRY, &hdry) || !hdry ||
        !p->GetField(Packages::LPFpack::VERTLEAKFLAG, &vleak) || !vleak ||
        !p->GetField(Packages::LPFpack::MF2K5, &mf2k5) || !mf2k5)
      return rval;
  }
  p->GetField(Packages::LPFpack::ISFAC, &ISFAC);
  p->GetField(Packages::LPFpack::ICONCV, &ICONCV);
  p->GetField(Packages::LPFpack::ITHFLG, &ITHFLG);
  p->GetField(Packages::LPFpack::NOCVCO, &NOCVCO);
  p->GetField(Packages::LPFpack::NOVFC, &NOVFC);

  if (ISFAC && *ISFAC != 0)
    m_useStorageCoefficent = true;

  // ILPFCB HDRY NPLPF VERTLEAKFLAG
  rval.Format("%d %s %d", *ILPFCB, STR(*hdry), m_nPar);
  if (m_unstructured)
  { // IKCFLAG - we only support option 0
    const int* ikcflag(0);
    p->GetField(Packages::LPFpack::IKCFLAG, &ikcflag);
    CStr s = " 0";
    if (ikcflag)
    {
      s.Format(" %d", *ikcflag);
      m_ikcflag = *ikcflag;
    }
    rval += s;
  }
  if (IPHDRY)
  {
    CStr iStr;
    iStr.Format(" %d", *IPHDRY);
    rval += iStr;
  }
  if (mf2k5 && !(*mf2k5))
  {
    CStr mystr;
    mystr.Format(" %d", *vleak);
    rval += mystr;
  }
  else if (vleak && *vleak)
  {
    rval += " NOVFC";
  }

  if (ISFAC && *ISFAC) { rval += " STORAGECOEFFICIENT"; }
  if (ICONCV && *ICONCV) { rval += " CONSTANTCV"; }
  if (ITHFLG && *ITHFLG) { rval += " THICKSTRT"; }
  if (NOCVCO && *NOCVCO) { rval += " NOCVCORRECTION"; }
  if (NOVFC && *NOVFC) { rval += " NOVFC"; }

  return rval;
} // NativeExpLpf::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLpf::Line2to6 (int a_line)
{
  if (a_line < 2 || a_line > 6) return CStr();

  CStr fields[7] = {"", "",
                    Packages::LPFpack::LAYTYP,
                    Packages::LPFpack::LAYAVG,
                    Packages::LPFpack::CHANI,
                    Packages::LPFpack::LAYVKA,
                    Packages::LPFpack::LAYWET
                   };
  if (Packages::UPW == GetPackage()->PackageName())
  {
    fields[2] = Packages::UPWpack::LAYTYPUPW;
    fields[5] = Packages::UPWpack::LAYVKAUPW;
  }
  const int*  vals(0);
  const Real* rVals(0);
  GetPackage()->GetField(fields[a_line], &vals);
  GetPackage()->GetField(fields[a_line], &rVals);
  if (!vals && !rVals) return CStr();

  std::stringstream ss;
  for (int i=0; i<m_nLay; ++i)
  {
    if (vals) ss << vals[i];
    else ss << STR(rVals[i]);
    ss << " ";
    if (a_line == 4 && rVals[i] != 1) m_anyChaniNotOne = true;
  }
  return ss.str();
} // NativeExpLpf::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpLpf::CanWriteLine (int a_line, int a_lay/*-1*/)
{
  const int*  vals(0);
  if (7 == a_line)
  {
    if (GetPackage()->GetField(Packages::LPFpack::LAYWET, &vals) && vals)
    {
      for (int i=0; i<m_nLay; ++i)
      {
        if (0 != vals[i]) return true;
      }
    }
  }
  else if (8 == a_line || 9 == a_line)
  {
    if (0 != m_nPar) return true;
  }
  else if (11 == a_line)
  {
    const Real* chani;
    GetPackage()->GetField(Packages::LPFpack::CHANI, &chani);
    if (chani[a_lay-1] <= 0) return true;
  }
  else if (13 == a_line)
  {
    if (GetNative()->GetExp()->AtLeastOneTransientSPExists()) return true;
  }
  else if (14 == a_line)
  {
    if (GetNative()->GetExp()->AtLeastOneTransientSPExists())
    {
      const int* laytyp;
      if (!GetPackage()->GetField(Packages::LPFpack::LAYTYP, &laytyp) ||
          !laytyp)
      {
        if (!GetPackage()->GetField(Packages::UPWpack::LAYTYPUPW, &laytyp) ||
            !laytyp)
          return false;
      }
      if (0 != laytyp[a_lay-1]) return true;
    }
  }
  else if (15 == a_line)
  {
    MfPackage* p = GetGlobal()->GetPackage(Packages::DIS);
    if (m_unstructured) p = GetGlobal()->GetPackage(Packages::DISU);
    if (p)
    {
      const int* laycbd;
      p->GetField(Packages::DisPack::LAYCBD, &laycbd);
      if (0 != laycbd[a_lay-1]) return true;
    }
  }
  else if (16 == a_line)
  {
    const int* laytyp, *laywet;
    if (!GetPackage()->GetField(Packages::LPFpack::LAYTYP, &laytyp) ||
        !laytyp)
    {
      if (!GetPackage()->GetField(Packages::UPWpack::LAYTYPUPW, &laytyp) ||
          !laytyp)
        return false;
    }
    GetPackage()->GetField(Packages::LPFpack::LAYWET, &laywet);
    if (0 != laytyp[a_lay-1] && 0 != laywet[a_lay-1]) return true;
  }
  return false;
} // NativeExpLpf::CanWriteLine
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLpf::Line7 ()
{
  CStr rval;
  const int *iwetit(0), *ihdwet(0);
  const Real *wetfct(0);
  MfPackage* p = GetGlobal()->GetPackage("L99");
  if (p->GetField("WETFCT", &wetfct) && wetfct &&
      p->GetField("IWETIT", &iwetit) && iwetit &&
      p->GetField("IHDWET", &ihdwet) && ihdwet)
  {
    rval.Format("%s %d %d", STR(*wetfct), *iwetit, *ihdwet);
  }
  return rval;
} // NativeExpLpf::CanWriteLine
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLpf::WriteLpfParams ()
{
  std::set<CStr> lpfParTypes = MfExportUtil::LpfParamTypes();
  ParamList *list;
  Parameters::GetParameterList(&list);
  Param p;
  CStr pType, line, ln;
  for (size_t i=0; i<list->Size(); i++)
  {
    list->At(i, &p);
    pType = p.m_type;
    pType.ToLower();
    if (lpfParTypes.find(pType) != lpfParTypes.end())
    {
      if (p.m_pilotPoints && !GetH5Flag())
      {
        WriteLpfPilotPar(list, &p);
      }
      else if (!p.m_clust.empty())
      {
        AddToStoredLinesDesc(Line8(&p), Desc(8));
        for (size_t j=0; j < p.m_clust.size(); j++)
        {
          AddToStoredLinesDesc(Line9(&p.m_clust[j]), Desc(9));
        }
      }
    }
  }
} // NativeExpLpf::WriteLpfParams
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLpf::WriteLpfPilotPar (ParamList* list, Param* p)
{
  if (!list || !p) return;
  Param p1(*p);
  std::vector<double> ppVals;
  if (!list->GetPilotPtValues(p->m_scatIndex, ppVals)) return;

  for (size_t i=0; i<ppVals.size(); ++i)
  {
    p1.m_name.Format("pp%d_%d", p->m_scatIndex, i+1);
    // add this name to the multiplier array file
    p1.m_value = ppVals[i];
    AddToStoredLinesDesc(Line8(&p1), Desc(8));
    for (size_t j=0; j<p1.m_clust.size(); ++j)
    {
      p1.m_clust[j].m_mlt = p1.m_name;
      if (m_unstructured && !m_stacked && p1.m_clust[j].m_lay > 1)
      {
        CStr nm;
        nm.Format("pp%dL%d_%d", p->m_scatIndex, p1.m_clust[j].m_lay, i+1);
        p1.m_clust[j].m_mlt = nm;
      }
      AddToStoredLinesDesc(Line9(&p1.m_clust[j]), Desc(9));
    }
  }
} // NativeExpLpf::WriteLpfPilotPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLpf::Line8 (Param* a_p)
{
  CStr rval, name = a_p->m_name;
  MfExportUtil::InsertSingleQuotesInName(name);
  // use m_value in case there was a SEN file with a different param
  // value than the LPF
  rval.Format("%s %s %s %d", name, a_p->m_type, STR((Real)a_p->m_value), 
              a_p->m_clust.size());
  return rval;
} // NativeExpLpf::Line8
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLpf::Line9 (PClust* a_clust)
{
  CStr rval, ln;
  rval.Format("%d %s %s ", a_clust->m_lay, a_clust->m_mlt, a_clust->m_zon);
  for (size_t k=0; k < a_clust->m_iz.size(); k++)
  {
    ln.Format("%d ", a_clust->m_iz[k]);
    rval += ln;
  }
  return rval;
} // NativeExpLpf::Line9
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLpf::Line10to16 (int a_line, int a_lay)
{
  CStr ptype[7] = {"HK", "HANI", "VK", "SS", "SY", "VKCB", ""};
  CStr arrName[7] = {ARR_LPF_HK, ARR_LPF_HANI, ARR_LPF_VK, ARR_LPF_SS,
                     ARR_LPF_SY, ARR_LPF_VKCBD, ARR_LPF_WET};

  ParamList* pList;
  Parameters::GetParameterList(&pList);
  if (pList->ParamOfTypeExists(ptype[a_line-10]))
  {
    AddToStoredLinesDesc("0", Desc(a_line, a_lay));
    return;
  }

  CStr aName = arrName[a_line-10];
  if (m_useStorageCoefficent && aName == ARR_LPF_SS)
    aName = ARR_LPF_SC;
  MfPackage* p = GetGlobal()->GetPackage(aName);
  CStr rval = p->StringsToWrite().front();
  p->StringsToWrite().erase(p->StringsToWrite().begin());
  AddToStoredLinesDesc(rval, Desc(a_line, a_lay));
  if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), rval))
  {
    AddToStoredLinesDesc(p->StringsToWrite()[0], "");
    p->StringsToWrite().erase(p->StringsToWrite().begin());
  }
} // NativeExpLpf::Line10to16
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLpf::Line12 (int a_lay)
{
  ParamList* pList;
  Parameters::GetParameterList(&pList);
  if (pList->ParamOfTypeExists("VK") ||
      pList->ParamOfTypeExists("VANI"))
  {
    AddToStoredLinesDesc("0", Desc(12, a_lay));
    return;
  }

  CStr rval;
  const int* layvka;
  if (Packages::UPW == GetPackage()->PackageName())
    GetPackage()->GetField(Packages::UPWpack::LAYVKAUPW, &layvka);
  else
    GetPackage()->GetField(Packages::LPFpack::LAYVKA, &layvka);
  MfPackage* p(0);
  if (0 == layvka[a_lay-1]) // VK
  {
    p = GetGlobal()->GetPackage(ARR_LPF_VK);
  }
  else // VANI
  {
    p = GetGlobal()->GetPackage(ARR_LPF_VANI);
  }
  rval = p->StringsToWrite().front();
  p->StringsToWrite().erase(p->StringsToWrite().begin());
  AddToStoredLinesDesc(rval, Desc(12, a_lay));
  if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), rval))
  {
    AddToStoredLinesDesc(p->StringsToWrite()[0], "");
    p->StringsToWrite().erase(p->StringsToWrite().begin());
  }
} // NativeExpLpf::Line12
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLpf::Line10Usg ()
{
  MfPackage* p = GetGlobal()->GetPackage("FACE ANGLE");
  CStr rval = p->StringsToWrite().front();
  p->StringsToWrite().erase(p->StringsToWrite().begin());
  AddToStoredLinesDesc(rval, Desc(17));
  if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(),rval))
  {
    AddToStoredLinesDesc(p->StringsToWrite()[0], "");
    p->StringsToWrite().erase(p->StringsToWrite().begin());
  }
} // NativeExpLpf::Line10Usg

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpLpf.t.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpLpfT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage("L98");
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpLpf*>(p);
} // NativeExpLpfT::setUp
//------------------------------------------------------------------------------
void NativeExpLpfT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpLpfT::tearDown
//------------------------------------------------------------------------------
void NativeExpLpfT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpLpfT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpLpfT::testDesc ()
{
  CStr base = " 1. ILPFCB HDRY NPLPF [Options]";
  CStr str = m_p->Desc(1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. Ltype(NLAY)";
  str = m_p->Desc(2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 3. LAYAVG(NLAY)";
  str = m_p->Desc(3);
  TS_ASSERT_EQUALS2(base, str);
  base = " 4. CHANI(NLAY)";
  str = m_p->Desc(4);
  TS_ASSERT_EQUALS2(base, str);
  base = " 5. LAYVKA(NLAY)";
  str = m_p->Desc(5);
  TS_ASSERT_EQUALS2(base, str);
  base = " 6. LAYWET(NLAY)";
  str = m_p->Desc(6);
  TS_ASSERT_EQUALS2(base, str);
  base = " 7. [WETFCT IWETIT IHDWET]";
  str = m_p->Desc(7);
  TS_ASSERT_EQUALS2(base, str);
  base = " 8. [PARNAM PARTYP Parval NCLU]";
  str = m_p->Desc(8);
  TS_ASSERT_EQUALS2(base, str);
  base = " 9. [Layer Mltarr Zonarr IZ]";
  str = m_p->Desc(9);
  TS_ASSERT_EQUALS2(base, str);
  base = "10. HK(NCOL,NROW)        LAY 2";
  str = m_p->Desc(10, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = "11. [HANI(NCOL,NROW)]    LAY 2";
  str = m_p->Desc(11, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = "12. VKA(NCOL,NROW)       LAY 2";
  str = m_p->Desc(12, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = "13. [Ss(NCOL,NROW)]      LAY 2";
  str = m_p->Desc(13, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = "14. [Sy(NCOL,NROW)]      LAY 2";
  str = m_p->Desc(14, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = "15. [VKCB(NCOL,NROW)]    LAY 2";
  str = m_p->Desc(15, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = "16. [WETDRY(NCOL,NROW)]  LAY 2";
  str = m_p->Desc(16, 2);
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpLpfT::testDesc
//------------------------------------------------------------------------------
void NativeExpLpfT::testLine1 ()
{
  Real hdry(88888);
  int  vleak(0), mf2k5(0);
  int  ILPFCB(22), ISFAC(1), ICONCV(1), ITHFLG(1), NOCVCO(1), NOVFC(1);

  MfPackage* p = m_p->GetPackage();
  p->SetField(Packages::LPFpack::ILPFCB, &ILPFCB);
  p->SetField(Packages::LPFpack::HDRY, &hdry);
  p->SetField(Packages::LPFpack::VERTLEAKFLAG, &vleak);
  p->SetField(Packages::LPFpack::MF2K5, &mf2k5);

  CStr base = "22 88888.0 0 0";
  CStr str = m_p->Line1();
  TS_ASSERT_EQUALS2(base, str);
  mf2k5 = 1;
  base = "22 88888.0 0";
  str = m_p->Line1();
  TS_ASSERT_EQUALS2(base, str);
  vleak = 1;
  base = "22 88888.0 0 NOVFC";
  str = m_p->Line1();
  TS_ASSERT_EQUALS2(base, str);

  p->SetField(Packages::LPFpack::ISFAC, &ISFAC);
  p->SetField(Packages::LPFpack::ICONCV, &ICONCV);
  p->SetField(Packages::LPFpack::ITHFLG, &ITHFLG);
  p->SetField(Packages::LPFpack::NOCVCO, &NOCVCO);
  p->SetField(Packages::LPFpack::NOVFC, &NOVFC);
  vleak = 0;
  base = "22 88888.0 0 STORAGECOEFFICIENT CONSTANTCV THICKSTRT NOCVCORRECTION NOVFC";
  str = m_p->Line1();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpLpfT::testLine1
//------------------------------------------------------------------------------
void NativeExpLpfT::testLine2to6 ()
{
  Real chani[3]={0,(Real)1.5, 0};
  int  laytyp[3]={0,1,-1}, layavg[3]={0,1,2}, layvka[3]={1,0,-1},
       laywet[3]={0,1,0};
  MfPackage* p = m_p->GetPackage();
  p->SetField(Packages::LPFpack::LAYTYP, laytyp);
  p->SetField(Packages::LPFpack::LAYAVG, layavg);
  p->SetField(Packages::LPFpack::CHANI,  chani);
  p->SetField(Packages::LPFpack::LAYVKA, layvka);
  p->SetField(Packages::LPFpack::LAYWET, laywet);

  CStr base = "0 1 -1 ";
  CStr str = m_p->Line2to6(2);
  TS_ASSERT_EQUALS2(base, str);
  base = "0 1 2 ";
  str = m_p->Line2to6(3);
  TS_ASSERT_EQUALS2(base, str);
  base = "0.0 1.5 0.0 ";
  str = m_p->Line2to6(4);
  TS_ASSERT_EQUALS2(base, str);
  base = "1 0 -1 ";
  str = m_p->Line2to6(5);
  TS_ASSERT_EQUALS2(base, str);
  base = "0 1 0 ";
  str = m_p->Line2to6(6);
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpLpfT::testLine2
//------------------------------------------------------------------------------
void NativeExpLpfT::testCanWriteLine ()
{
  int  laywet[3]={0,0,0};
  MfPackage* p = m_p->GetPackage();
  p->SetField(Packages::LPFpack::LAYWET, laywet);
  TS_ASSERT(!m_p->CanWriteLine(7));
  laywet[0] = 1;
  TS_ASSERT(m_p->CanWriteLine(7));
} // NativeExpLpfT::testCanWriteLine
//------------------------------------------------------------------------------
void NativeExpLpfT::testLine7 ()
{
  int iwetit(3), ihdwet(4);
  Real wetfct((Real).5);
  MfPackage p("L99");
  p.SetField("WETFCT", &wetfct);
  p.SetField("IWETIT", &iwetit);
  p.SetField("IHDWET", &ihdwet);
  m_p->GetGlobal()->AddPackage(&p);
  CStr base = "0.5 3 4";
  CStr str = m_p->Line7();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpLpfT::testLine7

//------------------------------------------------------------------------------
//void NativeExpLpfT::testDesc ()
//{
//  TS_FAIL("");
//} // NativeExpLpfT::testDesc


#endif
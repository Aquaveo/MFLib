//------------------------------------------------------------------------------
// FILE      NativeExpHuf.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpHuf.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfPackageUtil.h>
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
NativeExpHuf::NativeExpHuf () :
  m_useWet(false)
, m_isGmsHuf(false)
{
} // MfNativeExpHuf::MfNativeExpHuf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpHuf::~NativeExpHuf ()
{
} // MfNativeExpHuf::~MfNativeExpHuf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpHuf::Export ()
{
  CStr comments;
  Packages::GetComments(Packages::HUF2, comments);
  if (comments.Find("GMS_HGU_MAT") != -1) m_isGmsHuf = true;

  std::vector<Param> par;
  GetParams(par);
  AddToStoredLinesDesc(Line1(par), Desc1());
  AddToStoredLinesDesc(Line2(), Desc2());
  AddToStoredLinesDesc(Line3(), Desc3());
  if (m_useWet)
  {
    AddToStoredLinesDesc(Line4(), Desc4());
    Line5();
  }
  Line6to8();
  AddToStoredLinesDesc(Line9(), Desc9());

  for (size_t i=0; i<par.size(); ++i)
  {
    AddToStoredLinesDesc(Line10(par[i]), Desc10());
    AddToStoredLinesDesc(Line11(par[i]), Desc11(par[i]));
  }

  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpHuf::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Line1 (const std::vector<Param>& a_par)
{
  CStr rval;
  const int *IHUFCB(0), *NHUF(0), *NPHUF(0), *IOHUFHEADS(0), *IOHUFFLOWS(0);
  const Real *HDRY(0);

  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::HUFPack::HDRY, &HDRY) || !HDRY ||
      !a_p->GetField(Packages::HUFPack::IHUFCB, &IHUFCB) || !IHUFCB ||
      !a_p->GetField(Packages::HUFPack::NHUF, &NHUF) || !NHUF ||
      !a_p->GetField(Packages::HUFPack::NPHUF, &NPHUF) || !NPHUF ||
      !a_p->GetField(Packages::HUFPack::IOHUFHEADS, &IOHUFHEADS) || !IOHUFHEADS ||
      !a_p->GetField(Packages::HUFPack::IOHUFFLOWS, &IOHUFFLOWS) || !IOHUFFLOWS
     )
   return rval;

  // line 1 IHUFCB HDRY NHUF NPHUF IOHUFHEADS IOHUFFLOWS
  rval.Format("%d %s %d %d %d %d",
              *IHUFCB,
              STR(*HDRY),
              *NHUF,
              a_par.size(),
              *IOHUFHEADS,
              *IOHUFFLOWS);
  return rval;
} // NativeExpHuf::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Desc1 ()
{
  CStr rval = " 1. IHUFCB HDRY NHUF NPHUF IOHUFHEADS IOHUFFLOWS";
  return rval;
} // NativeExpHuf::Desc1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Line2 ()
{
  CStr rval;
  const int* LTHUF(0);
  if (!GetPackage()->GetField(Packages::HUFPack::LTHUF, &LTHUF) || !LTHUF
     )
   return rval;

  std::stringstream ss;
  for (int i=0; i<GetGlobal()->NumLay(); ++i)
  {
    ss << LTHUF[i] << " ";
  }
  rval = ss.str().c_str();
  return rval;
} // NativeExpHuf::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Desc2 ()
{
  CStr rval = " 2. LTHUF(NLAY)";
  return rval;
} // NativeExpHuf::Desc2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Line3 ()
{
  CStr rval;
  const int* LAYWT(0);
  if (!GetPackage()->GetField(Packages::HUFPack::LAYWT, &LAYWT) || !LAYWT
    )
    return rval;

  std::stringstream ss;
  for (int i=0; i<GetGlobal()->NumLay(); ++i)
  {
    ss << LAYWT[i] << " ";
    if (LAYWT[i] == 1) m_useWet = true;
  }
  rval = ss.str().c_str();
  return rval;
} // NativeExpHuf::Line3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Desc3 ()
{
  CStr rval = " 3. LAYWT(NLAY)";
  return rval;
} // NativeExpHuf::Desc3
//-----------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Line4 ()
{
  CStr rval;
  const int *IWETIT(0), *IHDWET(0);
  const Real *WETFCT(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::HUFPack::WETFCT, &WETFCT) || !WETFCT ||
      !a_p->GetField(Packages::HUFPack::IWETIT, &IWETIT) || !IWETIT ||
      !a_p->GetField(Packages::HUFPack::IHDWET, &IHDWET) || !IHDWET
    )
    return rval;

  // line 4 WETFCT IWETIT IHDWET
  rval.Format("%s %d %d", STR(*WETFCT), *IWETIT, *IHDWET);
  return rval;
} // NativeExpHuf::Line4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Desc4 ()
{
  CStr rval = " 4. WETFCT IWETIT IHDWET";
  return rval;
} // NativeExpHuf::Desc4
//-----------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpHuf::Line5 ()
{
  CStr rval;
  MfPackage* p = GetGlobal()->GetPackage(ARR_LPF_WET);
  if (!p || p->StringsToWrite().empty())
  {
    ASSERT(0);
    return;
  }
  const int* LAYWT(0);
  if (!GetPackage()->GetField(Packages::HUFPack::LAYWT, &LAYWT) || !LAYWT)
    return;

  int cnt(0);
  for (int i=0; i<GetGlobal()->NumLay(); ++i)
  {
    if (LAYWT[i] != 0)
    {
      rval = p->StringsToWrite()[cnt];
      AddToStoredLinesDesc(rval, Desc5());
      cnt++;
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), rval))
      {
        rval = p->StringsToWrite()[cnt];
        AddToStoredLinesDesc(rval, "");
        cnt++;
      }
    }
  }
} // NativeExpHuf::Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Desc5 ()
{
  CStr rval(" 5. WETDRY(NCOL,NROW)");
  return rval;
} // NativeExpHuf::Desc5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpHuf::Line6to8 ()
{
  CStr rval;
  const int *NHUF(0);
  const char *HGUNAM(0);
  if (!GetPackage()->GetField(Packages::HUFPack::NHUF, &NHUF) || !NHUF ||
      !GetPackage()->GetField(Packages::HUFPack::HGUNAM, &HGUNAM) || !HGUNAM)
    return;

  // get the HGU names
  std::vector<CStr> desc = Desc6to8();
  std::vector<CStr> names;
  char c[11] = {0};
  int i, j;
  for (i=0; i<*NHUF; i++)
  {
    for (j=0; j<10; j++)
      c[j] = HGUNAM[(i*10)+j];
    names.push_back(c);
    names.back().Trim();
  }

  for (i=0; i<*NHUF; i++)
  {
    AddToStoredLinesDesc(names[i], desc[0]);
    CStr pName = ARR_HUF_TOP;
    pName += names[i];
    MfPackage* pack = GetGlobal()->GetPackage(pName);
    if (pack)
    {
      rval = pack->StringsToWrite()[0];
      AddToStoredLinesDesc(rval, desc[1]);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), rval))
      {
        AddToStoredLinesDesc(pack->StringsToWrite()[1], "");
      }
    }
    else ASSERT(0);

    pName = ARR_HUF_THCK;
    pName += names[i];
    pack = GetGlobal()->GetPackage(pName);
    if (pack)
    {
      rval = pack->StringsToWrite()[0];
      AddToStoredLinesDesc(rval, desc[2]);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), rval))
      {
        AddToStoredLinesDesc(pack->StringsToWrite()[1], "");
      }
    }
    else ASSERT(0);
  }
} // NativeExpHuf::Line6to8
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpHuf::Desc6to8 ()
{
  std::vector<CStr> rval;
  const int *NHUF(0);
  if (!GetPackage()->GetField(Packages::HUFPack::NHUF, &NHUF) || !NHUF)
    return rval;

  for (int i=0; i<*NHUF; ++i)
  {
    rval.push_back(" 6. HGUNAM");
    rval.push_back(" 7. TOP(NCOL,NROW)");
    rval.push_back(" 8. THCK(NCOL,NROW)");
  }
  return rval;
} // NativeExpHuf::Desc6to8
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpHuf::Line9IsAll ()
{
  using namespace MfData::Packages;
  const Real *HGUHANI(0), *HGUVANI(0);
  const int *NHUF(0);
  if (!GetPackage()->GetField(HUFPack::NHUF, &NHUF) || !NHUF ||
      !GetPackage()->GetField(HUFPack::HGUHANI, &HGUHANI) || !HGUHANI ||
      !GetPackage()->GetField(HUFPack::HGUVANI, &HGUVANI) || !HGUVANI)
    return false;

  bool all(true);
  for (int i=0; i<*NHUF; ++i)
  {
    if (HGUHANI[0] != HGUHANI[i])
      all = false;
    if (HGUVANI[0] != HGUVANI[i])
      all = false;
  }
  return all;
} // NativeExpHuf::Line9IsAll
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpHuf::Line9 ()
{
  std::vector<CStr> rval;
  using namespace MfData::Packages;
  const Real *HGUHANI(0), *HGUVANI(0);
  const int *NHUF(0);
  if (!GetPackage()->GetField(HUFPack::NHUF, &NHUF) || !NHUF ||
      !GetPackage()->GetField(HUFPack::HGUHANI, &HGUHANI) || !HGUHANI ||
      !GetPackage()->GetField(HUFPack::HGUVANI, &HGUVANI) || !HGUVANI)
    return rval;

  if (Line9IsAll())
  {
    CStr line;
    line.Format("ALL %s %s", STR(HGUHANI[0]), STR(HGUVANI[0]));
    rval.push_back(line);
  }
  else
  {
    const char *HGUNAM(0);
    if (!GetPackage()->GetField(Packages::HUFPack::HGUNAM, &HGUNAM) || !HGUNAM)
      return rval;
    // get the HGU names
    std::vector<CStr> names;
    char c[11] = {0};
    int i, j;
    for (i=0; i<*NHUF; i++)
    {
      for (j=0; j<10; j++)
        c[j] = HGUNAM[(i*10)+j];
      names.push_back(c);
      names.back().Trim();
    }

    CStr line;
    for (int i=0; i<*NHUF; ++i)
    {
      line.Format("%s %s %s", names[i], STR(HGUHANI[i]), STR(HGUVANI[i]));
      rval.push_back(line);
    }
  }
  return rval;
} // NativeExpHuf::Line9
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpHuf::Desc9 ()
{
  std::vector<CStr> rval(Line9().size()," 9. HGUNAM HGUHANI HGUVANI");
  return rval;
} // NativeExpHuf::Desc9
//-----------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Line10 (Param& a_par)
{
  CStr rval;
  rval.Format("%s %s %s %d", a_par.m_name, a_par.m_type,
              STR((Real)a_par.m_value), a_par.m_clust.size());
  return rval;
} // NativeExpHuf::Line10
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::Desc10 ()
{
  CStr rval = "10. PARNAM PARTYP Parval NCLU";
  return rval;
} // NativeExpHuf::Line10
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpHuf::Line11 (Param& a_par)
{
  std::vector<CStr> rval;
  for (size_t i=0; i<a_par.m_clust.size(); ++i)
  {
    std::stringstream ss;
    ss << a_par.m_clust[i].m_hgu << " " << a_par.m_clust[i].m_mlt << " "
       << a_par.m_clust[i].m_zon << " ";
    for (size_t j=0; j<a_par.m_clust[i].m_iz.size(); ++j)
    {
      ss << a_par.m_clust[i].m_iz[j] << " ";
    }
    rval.push_back(ss.str().c_str());
  }
  return rval;
} // NativeExpHuf::Line10to11
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpHuf::Desc11 (Param& a_par)
{
  std::vector<CStr> rval(a_par.m_clust.size(), "11. HGUNAM Mltarr Zonarr IZ");
  return rval;
} // NativeExpHuf::Desc10to11
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpHuf::GetParams (std::vector<Param>& a_par)
{
  std::set<CStr> types = MfExportUtil::HufParamTypes();
  Param p, p1, p2;
  ParamList* list;
  Parameters::GetParameterList(&list);
  CStr type;
  for (size_t i=0; i<list->Size(); ++i)
  {
    list->At(i, &p);
    type = p.m_type;
    type.ToLower();
    if (types.find(type) != types.end())
    {
      if (!m_isGmsHuf)
        a_par.push_back(p);
      else
      {
        if (m_isGmsHuf)
        {
          if(p.m_clust.size() == 1 &&
             p.m_clust[0].m_mlt == p.m_name)
          {
            double key = KeyFromMultArray(p.m_name);
            HandleParameter(key, list, p, a_par);
          }
          else if (!p.m_clust.empty())
          {
            a_par.push_back(p);
          }
        }
        else if (!p.m_clust.empty()) a_par.push_back(p);
      }
    }
  }
} // NativeExpHuf::GetParams
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
double NativeExpHuf::KeyFromMultArray (const CStr& a_name)
{
  double key = -9999999;
  CStr name;
  name.Format("%s %s", ARR_MLT, a_name);
  MfPackage* p = GetGlobal()->GetPackage(name);
  if (p && !p->StringsToWrite().empty())
  {
    std::stringstream ss;
    ss << p->StringsToWrite().front();
    CStr str;
    ss >> str;
    if (str.CompareNoCase("CONSTANT") == 0)
    {
      ss >> key;
    }
  }
  return key;
}  // NativeExpHuf::KeyFromMultArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpHuf::HandleParameter (double a_key,
                                    ParamList* a_list,
                                    Param& a_p,
                                    std::vector<Param>& a_par)
{
  Param p1;
  if (!a_list->FindByKey(a_key, &p1)) return;

  PClust clst("NONE", "ALL");
  clst.m_hgu = a_p.m_clust[0].m_hgu;
  p1.m_clust.push_back(clst);

  if (!p1.m_pilotPoints)
  {
    a_list->UpdateParameter(&p1);
    a_par.push_back(p1);
  }
  else
  { // least amount of work is to call NativeExpArr2d
    ExportPilotPar(a_key, p1.m_type);
    std::vector<double> ppVals;
    a_list->GetPilotPtValues(p1.m_scatIndex, ppVals);
    for (size_t i=0; i<ppVals.size(); i++)
    {
      CStr name;
      name.Format("pp%d_%d", p1.m_scatIndex, i+1);
      p1.m_value = (Real)ppVals[i];
      p1.m_name = name;
      p1.m_clust[0].m_mlt = name;
      a_par.push_back(p1);
    }
  }
} // NativeExpHuf::HandleParameter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpHuf::ArrayNameFromParType (const CStr& a_type)
{
  CStr rval;
  std::map<CStr, CStr> map;
  map["HK"] = ARR_LPF_HK;
  map["HANI"] = ARR_LPF_HANI;
  map["VK"] = ARR_LPF_VK;
  map["VANI"] = ARR_LPF_VANI;
  map["SS"] = ARR_LPF_SS;
  map["SY"] = ARR_LPF_SY;
  CStr type(a_type);
  type.ToUpper();
  if (map.find(type) != map.end()) rval = map[type];
  return rval;
} // NativeExpHuf::ParTypeToArrayMap
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpHuf::ExportPilotPar (double a_key, const CStr& a_type)
{
  int nval = GetGlobal()->NumCol()*GetGlobal()->NumRow();
  std::vector<Real> vals(nval, (Real)a_key);
  Real mlt(1);
  int iprn(-1), lay(1);
  char name[24];
  strcpy(name, (LPCTSTR)ArrayNameFromParType(a_type));
  bool exists(true);
  MfPackage pack(name);
  MfPackage *p = GetGlobal()->GetPackage(name);
  if (!p)
  {
    exists = false;
    p = &pack;
  }
  p->SetField(Packages::Array::ARRAY, &vals[0]);
  p->SetField(Packages::Array::IPRN, &iprn);
  p->SetField(Packages::Array::MULT, &mlt);
  p->SetField(Packages::Array::LAYER, &lay);
  if (!exists) GetGlobal()->AddPackage(p);
  NativePackExp* exp = NativeUtil::CreatePackExp(GetNative(), GetGlobal(),
                                                 GetGlobal()->GetPackage(name));
  if (exp)
  {
    exp->Export();
    delete(exp);
  }
  // the above may mess up the pointer to our current package
  SetData(GetNative(), GetGlobal(), GetGlobal()->GetPackage(Packages::HUF));
} // NativeExpHuf::ExportPilotPar

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpHuf.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpHufT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::HUF);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpHuf*>(p);
} // NativeExpHufT::setUp
//------------------------------------------------------------------------------
void NativeExpHufT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpHufT::tearDown
//------------------------------------------------------------------------------
void NativeExpHufT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpHufT::testCreateClass

#endif
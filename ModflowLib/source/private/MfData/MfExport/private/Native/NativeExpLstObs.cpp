//------------------------------------------------------------------------------
// FILE      NativeExpLstObs.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpLstObs.h>

//#include <private/MfData/MfExport/private/Native/NativeExpNam.h>
//#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/MfData/Packages/ObsHd.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLstObs::NativeExpLstObs () :
  m_f(0)
, m_isMf2k(true)
{
} // MfNativeExpLstObs::MfNativeExpLstObs
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLstObs::~NativeExpLstObs ()
{
} // MfNativeExpLstObs::~MfNativeExpLstObs
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpLstObs::Export ()
{
  if (GetGlobal()->ModelType() != MfData::MF2K)
  {
    m_isMf2k = false;
  }

  if (GetPackage() && GetPackage()->PackageName() != "FOB")
  {
    SaveObsVar();
    return true;
  }

  m_f = &GetFLOB();
  if (!m_f) return false;

  if (!m_f->m_flob.empty())
  {
    if (ObsExist("CHOB")) WriteChob();
    if (ObsExist("DROB")) WriteDrob();
    if (ObsExist("DTOB")) WriteDtob();
    if (ObsExist("GBOB")) WriteGbob();
    if (ObsExist("RVOB")) WriteRvob();
    if (ObsExist("STOB")) WriteStob();
  }
  return true;
} // MfNativeExpLstObs::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstObs::SaveObsVar ()
{
  MfPackage* p = GetPackage();
  std::vector<CStr> fields = p->FieldNames();
  const Real* rPtr;
  const int*  iPtr;

  for (size_t i=0; i<fields.size(); ++i)
  {
    rPtr = 0;
    iPtr = 0;
    p->GetField(fields[i], &rPtr);
    p->GetField(fields[i], &iPtr);

    CStr nm;
    nm.Format("%s_%s", p->PackageName(), fields[i]);
    if (rPtr)
    {
      GetGlobal()->SetRealVar(nm, *rPtr);
    }
    else if (iPtr)
    {
      GetGlobal()->SetIntVar(nm, *iPtr);
    }
  }

} // NativeExpLstObs::SaveObsVar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpLstObs::ObsExist (const char * const a_type)
{
  if (!m_f) return false;

  bool rval = false;
  for (size_t i=0; !rval && i<m_f->m_flob.size(); ++i)
  {
    if (m_f->m_flob[i].m_type == a_type) rval = true;
  }
  return rval;
} // NativeExpLstObs::ObsExist
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstObs::Line1 (const char* const a_type,
                             const char* const a_p,
                             const char* const a_f1)
{
  if (!m_f) return "";

  std::set<int> usedIds;
  int nGrp(0), nCells(0), nObs(0);
  for (size_t i=0; i<m_f->m_flob.size(); ++i)
  {
    if (m_f->m_flob[i].m_type == a_type)
    {
      nObs++;

      int id = m_f->m_flob[i].m_factorId;
      if (usedIds.find(id) != usedIds.end()) continue;

      nGrp++;
      for (size_t j=0; j<m_f->m_fact.size(); ++j)
      {
        if (m_f->m_fact[j].m_factorId == id)
        {
          nCells++;
        }
      }
      usedIds.insert(id);
    }
  }

  CStr rval;
  rval.Format("%5d %5d %5d", nGrp, nCells, nObs);
  if (!m_isMf2k)
  {
    int i1;
    CStr nm1;
    nm1.Format("%s_%s", a_p, a_f1);
    if (!GetGlobal()->GetIntVar(nm1, i1)) return rval;
    nm1.Format(" %5d", i1);
    rval += nm1;
  }
  return rval;
} // NativeExpLstObs::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpLstObs::Line2 (const char* const a_p,
                             const char* const a_f1,
                             const char* const a_f2)
{
  CStr rval;

  CStr pack = a_p;
  if (!m_isMf2k) pack.Replace("OB", "OV");
  CStr nm1, nm2;
  nm1.Format("%s_%s", pack, a_f1);
  nm2.Format("%s_%s", pack, a_f2);
  Real f1, f2;
  if (!GetGlobal()->GetRealVar(nm1, f1)) return rval;


  int width = util::RealWidth();
  rval.Format("%s", STR(f1, -1, width, STR_FULLWIDTH));
  if (m_isMf2k)
  {
    if (!GetGlobal()->GetRealVar(nm2, f2)) return rval;
    CStr r;
    r.Format(" %s %5d", STR(f2, -1, width, STR_FULLWIDTH), 0);
    rval += r;
  }
  return rval;
} // NativeExpLstObs::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstObs::Lines3to5 (const char* const a_type,
                                 CStr a_desc[3])
{
  CStr fType = a_type;
  int wid = util::RealWidth();
  std::set<int> usedIds;
  for (size_t i=0; i<m_f->m_flob.size(); ++i)
  {
    if (m_f->m_flob[i].m_type != a_type) continue;
    int id = m_f->m_flob[i].m_factorId;
    if (usedIds.find(id) != usedIds.end()) continue;

    usedIds.insert(id);
    // count all the obs with this id
    std::vector<CStr> line4;
    for (size_t j=i; j<m_f->m_flob.size(); ++j)
    {
      if (m_f->m_flob[j].m_factorId == id)
      {
        Flob& f(m_f->m_flob[j]);
        CStr ln, name = f.m_name.Left(12);
        while (name.GetLength() < 12) name += " ";
        ln.Format("%s %5d %s %s", name, f.m_IREFSP,
                  STR(f.m_TOFFSET, -1, wid, STR_FULLWIDTH),
                  STR(f.m_HOB, -1, wid, STR_FULLWIDTH));
        if (m_isMf2k)
        {
          CStr ln1;
          ln1.Format(" %s %5d %5d", STR(f.m_STAT, -1, wid, STR_FULLWIDTH),
                     f.m_STATFLG, f.m_PLOT);
          ln += ln1;
        }
        line4.push_back(ln);
      }
    }

    // count the number of cells
    std::vector<CStr> line5;
    for (size_t j=0; j<m_f->m_fact.size(); ++j)
    {
      if (m_f->m_fact[j].m_factorId == id)
      {
        FlobFact& f(m_f->m_fact[j]);
        CStr ln;
        if ("STOB" != fType)
        {
          ln.Format("%5d %5d %5d %s", f.m_k, f.m_i, f.m_j,
                    STR(f.m_factor, -1, wid, STR_FULLWIDTH));
        }
        else
        {
          ln.Format("%5d %5d %s", f.m_k, f.m_i,
                    STR(f.m_factor, -1, wid, STR_FULLWIDTH));
        }
        
        line5.push_back(ln);
      }
    }

    CStr ln;
    ln.Format("%5d %5d", line4.size(), line5.size());
    AddToStoredLinesDesc(ln, a_desc[0]);
    CStr d4 = a_desc[1];
    if (!m_isMf2k)
    {
      CStr desc = d4.Left(a_desc[1].Find("HOB"));
      desc += "FLWOBS";
      d4 = desc; 
    }
    for (size_t j=0; j<line4.size(); ++j)
    {
      AddToStoredLinesDesc(line4[j], d4);
    }
    for (size_t j=0; j<line5.size(); ++j)
    {
      AddToStoredLinesDesc(line5[j], a_desc[2]);
    }
  }
} // NativeExpLstObs::Lines3to5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstObs::WriteChob ()
{
  CStr d = " 1. NQCH NQCCH NQTCH";
  if (!m_isMf2k) d += " IUCHOBSV";
  AddToStoredLinesDesc(Line1("CHOB", "OV6", "IUCHOBSV"), d);
  d = " 2. TOMULTCH";
  if (m_isMf2k) d += " EVFCH IOWTQCH";
  AddToStoredLinesDesc(Line2("OB6", "TOMULTCH", "EVFCH"), d);
  CStr desc[3] = {" 3. NQOBCH NQCLCH"
                 ," 4. OBSNAM IREFSP TOFFSET HOBS STATISTIC STAT-FLAG PLOT-SYMBOL"
                 ," 5. LAYER ROW COLUMN FACTOR"
                 };
  Lines3to5("CHOB", desc);

  // write the file
  TmpPackageNameChanger tmp(GetPackage(), "CHOB");
  WriteComments();
  WriteStoredLines();
} // NativeExpLstObs::WriteChob
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstObs::WriteDrob ()
{
  CStr d = " 1. NQDR NQCDR NQTDR";
  if (!m_isMf2k) d += " IUDROBSV";
  AddToStoredLinesDesc(Line1("DROB", "OV4", "IUDROBSV"), d);
  AddToStoredLinesDesc(Line2("OB4", "TOMULTDR", "EVFDR"),
                       " 2. TOMULTDR EVFDR IOWTQDR");
  CStr desc[3] = {" 3. NQOBDR NQCLDR"
                 ," 4. OBSNAM IREFSP TOFFSET HOBS STATISTIC STAT-FLAG PLOT-SYMBOL"
                 ," 5. LAYER ROW COLUMN FACTOR"
                 };
  Lines3to5("DROB", desc);

  // write the file
  TmpPackageNameChanger tmp(GetPackage(), "DROB");
  WriteComments();
  WriteStoredLines();
} // NativeExpLstObs::WriteDrob
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstObs::WriteDtob ()
{
  AddToStoredLinesDesc(Line1("DTOB", "", ""), " 1. NQDT NQCDT NQTDT");
  AddToStoredLinesDesc(Line2("OB8", "TOMULTDT", "EVFDT"),
                       " 2. TOMULTDT EVFDT IOWTQDT");
  CStr desc[3] = {" 3. NQOBDT NQCLDT"
                 ," 4. OBSNAM IREFSP TOFFSET HOBS STATISTIC STAT-FLAG PLOT-SYMBOL"
                 ," 5. LAYER ROW COLUMN FACTOR"
                 };
  Lines3to5("DTOB", desc);

  // write the file
  TmpPackageNameChanger tmp(GetPackage(), "DTOB");
  WriteComments();
  WriteStoredLines();
} // NativeExpLstObs::WriteDtob
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstObs::WriteGbob ()
{
  CStr d = " 1. NQGB NQCGB NQTGB";
  if (!m_isMf2k) d += " IUGBOBSV";
  AddToStoredLinesDesc(Line1("GBOB", "OV3", "IUGBOBSV"), d);
  AddToStoredLinesDesc(Line2("OB3", "TOMULTGB", "EVFGB"),
                       " 2. TOMULTGB EVFGB IOWTQGB");
  CStr desc[3] = {" 3. NQOBGB NQCLGB"
                 ," 4. OBSNAM IREFSP TOFFSET HOBS STATISTIC STAT-FLAG PLOT-SYMBOL"
                 ," 5. LAYER ROW COLUMN FACTOR"
                 };
  Lines3to5("GBOB", desc);

  // write the file
  TmpPackageNameChanger tmp(GetPackage(), "GBOB");
  WriteComments();
  WriteStoredLines();
} // NativeExpLstObs::WriteGbob
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstObs::WriteRvob ()
{
  CStr d = " 1. NQRV NQCRV NQTRV";
  if (!m_isMf2k) d += " IURVOBSV";
  AddToStoredLinesDesc(Line1("RVOB", "OV5", "IURVOBSV"), d);
  AddToStoredLinesDesc(Line2("OB5", "TOMULTRV", "EVFRV"),
                       " 2. TOMULTRV EVFRV IOWTQRV");
  CStr desc[3] = {" 3. NQOBRV NQCLRV"
                 ," 4. OBSNAM IREFSP TOFFSET HOBS STATISTIC STAT-FLAG PLOT-SYMBOL"
                 ," 5. LAYER ROW COLUMN FACTOR"
                 };
  Lines3to5("RVOB", desc);

  // write the file
  TmpPackageNameChanger tmp(GetPackage(), "RVOB");
  WriteComments();
  WriteStoredLines();
} // NativeExpLstObs::WriteRvob
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLstObs::WriteStob ()
{
  CStr d = " 1. NQST NQCST NQTST";
  if (!m_isMf2k) d += " IUSTOBSV";
  AddToStoredLinesDesc(Line1("STOB", "OV7", "IUSTOBSV"), d);
  AddToStoredLinesDesc(Line2("OB7", "TOMULTST", "EVFST"),
                       " 2. TOMULTST  EVFST IOWTQST");
  CStr desc[3] = {" 3. NQOBST NQCLST"
    ," 4. OBSNAM IREFSP TOFFSET HOBS STATISTIC STAT-FLAG PLOT-SYMBOL"
    ," 5. SEGMENT REACH FACTOR"
  };
  Lines3to5("STOB", desc);

  // write the file
  TmpPackageNameChanger tmp(GetPackage(), "STOB");
  WriteComments();
  WriteStoredLines();
} // NativeExpLstObs::WriteStob


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpLstObs.t.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpLstObsT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::FOB);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpLstObs*>(p);
} // NativeExpLstObsT::setUp
//------------------------------------------------------------------------------
void NativeExpLstObsT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpLstObsT::tearDown
//------------------------------------------------------------------------------
void NativeExpLstObsT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpLstObsT::testCreateClass

#endif
//------------------------------------------------------------------------------
// FILE      NativeExpMnw2.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMnw2.h>

#include <sstream>
//#include <private\MfData\MfExport\private\Native\NativeExpNam.h>
//#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
//#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
namespace
{
  std::vector<CStr>& Mnw2WellIds (int a_idx)
  {
    static std::map<int, std::vector<CStr> > ids;
    return ids[a_idx];
  } // Mnw2WellIds
  std::vector<int>& Mnw2PumpCap (int a_idx)
  {
    static std::map<int, std::vector<int> > pCap;
    return pCap[a_idx];
  } // Mnw2PumpCap
  std::vector<int>& Mnw2Qlimit (int a_idx)
  {
    static std::map<int, std::vector<int> > qLimit;
    return qLimit[a_idx];
  } // Mnw2Qlimit
} // unnamed namespace
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMnw2::NativeExpMnw2 ()
{
} // MfNativeExpMnw2::MfNativeExpMnw2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMnw2::~NativeExpMnw2 ()
{
} // MfNativeExpMnw2::~MfNativeExpMnw2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMnw2::Export ()
{
  CStr ln = GetPackage()->GetLineNumber();
  if ("1" == ln)        Line1();
  else if ("2ab" == ln) Line2ab();
  else if ("2c" == ln)  Line2c();
  else if ("2d" == ln)  Line2d();
  else if ("2e" == ln)  Line2e();
  else if ("2f" == ln)  Line2f();
  else if ("2g" == ln)  Line2g();
  else if ("2h" == ln)  Line2h();
  else if ("34" == ln)  Lines34();
  else if ("Export Final" == ln)
  {
    WriteComments();
    WriteStoredLines();
  }
  return true;
} // MfNativeExpMnw2::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr>& NativeExpMnw2::WellIds ()
{
  return Mnw2WellIds((int)GetGlobal()->CurModIdx());
} // NativeExpMnw2::WellIds
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<int>& NativeExpMnw2::PumpCap ()
{
  return Mnw2PumpCap((int)GetGlobal()->CurModIdx());
} // NativeExpMnw2::PumpCap
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<int>& NativeExpMnw2::Qlimit ()
{
  return Mnw2Qlimit((int)GetGlobal()->CurModIdx());
} // NativeExpMnw2::Qlimit
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMnw2::Desc (int a_line)
{
  CStr desc[] = { "   1. MNWMAX,IWL2CB,MNWPRNT,{OPTION}"
                 ,"  2a. WELLID,NNODES"
                 ,"  2b. LOSSTYPE,PUMPLOC,Qlimit,PPFFLAG,PUMPCAP"
                 ,"  4a. WELLID Qdes [CapMult] [Cprime] [xyz]"
                 ,"  4b. Hlim QCUT [Qfrcmn Qfrcmx]"
                };
  return desc[a_line-1];
} // NativeExpMnw2::Desc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw2::Line1 ()
{
  using namespace MfData::Packages;
  const int *MNWMAX(0),*IWL2CB(0),*MNWPRNT(0),*NAUX(0);
  const char *MNWAUX(0);

  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX ||
      !a_p->GetField(MNW2pack::IWL2CB, &IWL2CB) || !IWL2CB ||
      !a_p->GetField(MNW2pack::MNWPRNT, &MNWPRNT) || !MNWPRNT ||
      !a_p->GetField(MNW2pack::NAUX, &NAUX) || !NAUX ||
      !a_p->GetField(MNW2pack::MNWAUX, &MNWAUX) || !MNWAUX)
  {
    ASSERT(0);
    return;
  }

  CStr rval;
  rval.Format("%d %d %d", *MNWMAX, *IWL2CB, *MNWPRNT);

  // copy the aux variable names
  std::vector<CStr> auxNames;
  char              tmpAux[17];
  int               i, j, cnt(0);
  tmpAux[16] = '\0';
  for (i=0; i<*NAUX; i++)
  {
    for (j=0; j<16; j++)
    {
      tmpAux[j] = MNWAUX[cnt++];
    }
    auxNames.push_back(tmpAux);
    auxNames.back().Trim();
  }

  for (i=0; i<*NAUX; i++)
  {
    rval += " AUX ";
    rval += auxNames[i];
  }
  AddToStoredLinesDesc(rval, Desc(1));
} // NativeExpMnw2::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw2::Line2ab ()
{
  using namespace MfData::Packages;
  const int * NNODES(0),* PUMPLOC(0),* rQlimit(0),* PPFLAG(0),* PUMPCAP(0);
  const char* WELLID(0),*LOSSTYPE(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(MNW2pack::WELLID, &WELLID) || !WELLID ||
      !a_p->GetField(MNW2pack::NNODES, &NNODES) || !NNODES ||
      !a_p->GetField(MNW2pack::LOSSTYPE, &LOSSTYPE) || !LOSSTYPE ||
      !a_p->GetField(MNW2pack::PUMPLOC, &PUMPLOC) || !PUMPLOC ||
      !a_p->GetField(MNW2pack::Qlimit, &rQlimit) || !rQlimit ||
      !a_p->GetField(MNW2pack::PPFLAG, &PPFLAG) || !PPFLAG ||
      !a_p->GetField(MNW2pack::PUMPCAP, &PUMPCAP) || !PUMPCAP)
  {
    ASSERT(0);
    return;
  }

  CStr line, wellid(WELLID);
  if (wellid.find(" ") != -1)
  {
    wellid.Format("'%s'", WELLID);
  }
  WellIds().push_back(wellid); // store the well id for use later
  line.Format("%s %d", wellid, *NNODES);
  AddToStoredLinesDesc(line, Desc(2));

  line.Format("%s %d %d %d %d", LOSSTYPE,*PUMPLOC,*rQlimit,*PPFLAG,*PUMPCAP);
  Qlimit().push_back(*rQlimit);
  PumpCap().push_back(*PUMPCAP);
  AddToStoredLinesDesc(line, Desc(3));
} // NativeExpMnw2::Line2ab
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
namespace
{
void LineToSet (const char* LnDesc,
                std::set<CStr>& a_set)
{
  a_set.clear();
  std::stringstream os;
  std::string str;
  os << LnDesc;

  while (std::getline(os, str, ','))
  {
    if (!str.empty())
    {
      a_set.insert(str.c_str());
    }
  }

} // LineToSet
} // unnamed namespace
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw2::Line2c ()
{
  using namespace MfData::Packages;
  const double* Rw(0),* Rskin(0),* Kskin(0),* B(0),* C(0),* P(0),* CWC(0);
  const char* LnDesc1(0);

  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(MNW2pack::Rw, &Rw) || !Rw ||
      !a_p->GetField(MNW2pack::Rskin, &Rskin) || !Rskin ||
      !a_p->GetField(MNW2pack::Kskin, &Kskin) || !Kskin ||
      !a_p->GetField(MNW2pack::B, &B) || !B ||
      !a_p->GetField(MNW2pack::C, &C) || !C ||
      !a_p->GetField(MNW2pack::P, &P) || !P ||
      !a_p->GetField(MNW2pack::CWC, &CWC) || !CWC ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  LineToSet(LnDesc, aSet);

  std::stringstream os;
  if (aSet.find("Rw") != aSet.end())
    os << STR(*Rw) << " ";
  if (aSet.find("Rskin") != aSet.end())
    os << STR(*Rskin) << " ";
  if (aSet.find("Kskin") != aSet.end())
    os << STR(*Kskin) << " ";
  if (aSet.find("B") != aSet.end())
    os << STR(*B) << " ";
  if (aSet.find("C") != aSet.end())
    os << STR(*C) << " ";
  if (aSet.find("P") != aSet.end())
    os << STR(*P) << " ";
  if (aSet.find("CWC") != aSet.end())
    os << STR(*CWC) << " ";

  CStr line = os.str().c_str(), desc;
  if (!line.empty())
  {
    desc.Format("  2c. %s", LnDesc);
    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpMnw2::Line2c
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw2::Line2d ()
{
  using namespace MfData::Packages;
  const int* IL(0),* IR(0),* IC(0);
  const double* RwNode(0),* RskinNode(0),* KskinNode(0),* BNode(0),
              * CNode(0),* PNode(0),
              * CWCNode(0),* PP(0),* Ztop(0),* Zbotm(0);
  const char* LnDesc1(0);

  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(MNW2pack::IR, &IR) || !IR ||
      !a_p->GetField(MNW2pack::IC, &IC) || !IC ||
      !a_p->GetField(MNW2pack::RwNode, &RwNode) || !RwNode ||
      !a_p->GetField(MNW2pack::RskinNode, &RskinNode) || !RskinNode ||
      !a_p->GetField(MNW2pack::KskinNode, &KskinNode) || !KskinNode ||
      !a_p->GetField(MNW2pack::BNode, &BNode) || !BNode ||
      !a_p->GetField(MNW2pack::CNode, &CNode) || !CNode ||
      !a_p->GetField(MNW2pack::PNode, &PNode) || !PNode ||
      !a_p->GetField(MNW2pack::CWCNode, &CWCNode) || !CWCNode ||
      !a_p->GetField(MNW2pack::PP, &PP) || !PP ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }
  if (!a_p->GetField(MNW2pack::IL, &IL) || !IL)
  {
    if (!a_p->GetField(MNW2pack::Ztop, &Ztop) || !Ztop ||
        !a_p->GetField(MNW2pack::Zbotm, &Zbotm) || !Zbotm)
    {
      ASSERT(0);
      return;
    }
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  LineToSet(LnDesc, aSet);

  std::stringstream os;
  CStr label("2d-1.");
  if (IL)
  {
    os << *IL << " ";
  }
  else
  {
    label = "2d-2.";
    os << STR(*Ztop) << " " << STR(*Zbotm) << " ";
  }
  os  << *IR << " " << *IC << " ";

  if (aSet.find("RwNode") != aSet.end())
    os << STR(*RwNode) << " ";
  if (aSet.find("RskinNode") != aSet.end())
    os << STR(*RskinNode) << " ";
  if (aSet.find("KskinNode") != aSet.end())
    os << STR(*KskinNode) << " ";
  if (aSet.find("BNode") != aSet.end())
    os << STR(*BNode) << " ";
  if (aSet.find("CNode") != aSet.end())
    os << STR(*CNode) << " ";
  if (aSet.find("PNode") != aSet.end())
    os << STR(*PNode) << " ";
  if (aSet.find("CWCNode") != aSet.end())
    os << STR(*CWCNode) << " ";
  if (aSet.find("PP") != aSet.end())
    os << STR(*PP) << " ";

  CStr line = os.str().c_str(), desc;
  if (!line.empty())
  {
    desc.Format("%s %s", label, LnDesc);
    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpMnw2::Line2d
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw2::Line2e ()
{
  using namespace MfData::Packages;
  const int* PUMPLAY(0), * PUMPROW(0),* PUMPCOL(0);
  const double* Zpump(0);
  const char* LnDesc1(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(MNW2pack::PUMPLAY, &PUMPLAY) || !PUMPLAY ||
      !a_p->GetField(MNW2pack::PUMPROW, &PUMPROW) || !PUMPROW ||
      !a_p->GetField(MNW2pack::PUMPCOL, &PUMPCOL) || !PUMPCOL ||
      !a_p->GetField(MNW2pack::Zpump, &Zpump) || !Zpump ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  LineToSet(LnDesc, aSet);

  std::stringstream os;
  if (aSet.find("PUMPLAY") != aSet.end())
    os << *PUMPLAY << " ";
  if (aSet.find("PUMPROW") != aSet.end())
    os << *PUMPROW << " ";
  if (aSet.find("PUMPCOL") != aSet.end())
    os << *PUMPCOL << " ";
  if (aSet.find("Zpump") != aSet.end())
    os << STR(*Zpump) << " ";

  CStr line = os.str().c_str(), desc;
  if (!line.empty())
  {
    desc.Format("  2e. %s", LnDesc);
    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpMnw2::Line2e
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw2::Line2f ()
{
  using namespace MfData::Packages;
  const double* Hlim(0),* Qfrcmn(0),* Qfrcmx(0);
  const int* QCUT(0);
  const char* LnDesc1(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(MNW2pack::Hlim, &Hlim) || !Hlim ||
      !a_p->GetField(MNW2pack::QCUT, &QCUT) || !QCUT ||
      !a_p->GetField(MNW2pack::Qfrcmn, &Qfrcmn) || !Qfrcmn ||
      !a_p->GetField(MNW2pack::Qfrcmx, &Qfrcmx) || !Qfrcmx ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  LineToSet(LnDesc, aSet);

  std::stringstream os;
  if (aSet.find("Hlim") != aSet.end())
    os << STR(*Hlim) << " ";
  if (aSet.find("QCUT") != aSet.end())
    os << *QCUT << " ";
  if (aSet.find("Qfrcmn") != aSet.end())
    os << STR(*Qfrcmn) << " ";
  if (aSet.find("Qfrcmx") != aSet.end())
    os << STR(*Qfrcmx) << " ";

  CStr line = os.str().c_str(), desc;
  if (!line.empty())
  {
    desc.Format("  2f. %s", LnDesc);
    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpMnw2::Line2f
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw2::Line2g ()
{
  using namespace MfData::Packages;
  const double* Hlift(0),* LIFTq0(0),* LIFTqdes(0),* HWtol(0);
  const char* LnDesc1(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(MNW2pack::Hlift, &Hlift) || !Hlift ||
      !a_p->GetField(MNW2pack::LIFTq0, &LIFTq0) || !LIFTq0 ||
      !a_p->GetField(MNW2pack::LIFTqdes, &LIFTqdes) || !LIFTqdes ||
      !a_p->GetField(MNW2pack::HWtol, &HWtol) || !HWtol ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  LineToSet(LnDesc, aSet);

  std::stringstream os;
  if (aSet.find("Hlift") != aSet.end())
    os << STR(*Hlift) << " ";
  if (aSet.find("LIFTq0") != aSet.end())
    os << *LIFTq0 << " ";
  if (aSet.find("LIFTqdes") != aSet.end())
    os << STR(*LIFTqdes) << " ";
  if (aSet.find("HWtol") != aSet.end())
    os << STR(*HWtol) << " ";

  CStr line = os.str().c_str(), desc;
  if (!line.empty())
  {
    desc.Format("  2g. %s", LnDesc);
    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpMnw2::Line2g
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw2::Line2h ()
{
  using namespace MfData::Packages;
  const double* Liftn(0),* Qn(0);
  const char* LnDesc1(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(MNW2pack::Liftn, &Liftn) || !Liftn ||
      !a_p->GetField(MNW2pack::Qn, &Qn) || !Qn ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  LineToSet(LnDesc, aSet);

  std::stringstream os;
  if (aSet.find("Liftn") != aSet.end())
    os << STR(*Liftn) << " ";
  if (aSet.find("Qn") != aSet.end())
    os << STR(*Qn) << " ";

  CStr line = os.str().c_str(), desc;
  if (!line.empty())
  {
    desc.Format("  2h. %s", LnDesc);
    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpMnw2::Line2h
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw2::Lines34 ()
{
  using namespace MfData::Packages;
  using util::ForElement;

  int a_sp = GetGlobal()->GetCurrentPeriod();
  const int* ITMP(0),* NMNWVL(0),* MNWMAX(0),* NAUX(0);
  const double* MNW2d(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(MNW2pack::ITMP, &ITMP) || !ITMP ||
      !a_p->GetField(MNW2pack::MNW2, &MNW2d) || !MNW2d ||
      !a_p->GetField(MNW2pack::NMNWVL, &NMNWVL) || !NMNWVL ||
      !a_p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX ||
      !a_p->GetField(MNW2pack::NAUX, &NAUX) || !NAUX)
  {
    ASSERT(0);
    return;
  }
  CStr line, desc;
  // write the line with ITMP
  line.Format("%d", *ITMP);
  desc.Format("   3. ITMP (SP%d)", a_sp);
  AddToStoredLinesDesc(line, desc);

  if (*ITMP > 0)
  { // get the data from the MNW2 array
    for (int i=0; i<*MNWMAX; i++)
    {
      double active = ForElement(MNW2d,  1, i+1, *NMNWVL); // active
      if (0 != active)
      {
        std::stringstream os;
        double Qdes    = ForElement(MNW2d,  5, i+1, *NMNWVL); // Qdes
        double CapMult = ForElement(MNW2d, 24, i+1, *NMNWVL); // CapMult
        //Real Cprime  = ForElement(MNW2d, 12, i+1, *NMNWVL); // Cprime;
        os << WellIds()[i] << " " << STR(Qdes) << " ";
        if (PumpCap()[i] > 0) os << STR(CapMult) << " ";
        for (int j=0; j<*NAUX; j++) // AUX
        {
          os << STR(ForElement(MNW2d, 31+j, i+1, *NMNWVL)) << " ";
        }
        line = os.str().c_str();
        AddToStoredLinesDesc(line, Desc(4));
        if (Qlimit()[i] < 0)
        {
          std::stringstream ss;
          double Hlim = ForElement(MNW2d,  7, i+1, *NMNWVL); // Hlim
          double QCUT = ForElement(MNW2d,  8, i+1, *NMNWVL); // QCUT
          ss << STR(Hlim) << " " << STR(QCUT) << " ";
          if (QCUT != 0)
          {
            double Qfrcmn = ForElement(MNW2d,  9, i+1, *NMNWVL); // Qfrcmn
            double Qfrcmx = ForElement(MNW2d, 10, i+1, *NMNWVL); // Qfrcmx
            ss << STR(Qfrcmn) << " " << STR(Qfrcmx);
          }
          line = ss.str().c_str();
          AddToStoredLinesDesc(line, Desc(5));
        }
      }
    }
  }
} // NativeExpMnw2::Lines34


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpMnw2.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpMnw2T::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::MNW2);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpMnw2*>(p);
} // NativeExpMnw2T::setUp
//------------------------------------------------------------------------------
void NativeExpMnw2T::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpMnw2T::tearDown
//------------------------------------------------------------------------------
void NativeExpMnw2T::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpMnw2T::testCreateClass

#endif
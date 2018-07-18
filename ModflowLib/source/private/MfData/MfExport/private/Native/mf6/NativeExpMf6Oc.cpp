//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Oc.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Oc.h>

#include <sstream>

#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExporterImpl.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>



using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Oc::NativeExpMf6Oc (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Oc::MfNativeExpMf6Oc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Oc::~NativeExpMf6Oc ()
{
} // MfNativeExpMf6Oc::~MfNativeExpMf6Oc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Oc::Export ()
{
  if (!m_pack) return false;
  if (Packages::OC == m_pack->GetPackage()->PackageName()) return false;
  
  std::vector<CStr> lines, comments; 
  MfGlobal *g = m_pack->GetGlobal(); 
  if (!g) return false;
  MfPackage* p = m_pack->GetPackage();
  MfPackage* p1 = g->GetPackage(Packages::OC);

  std::vector<CStr>& linesP(m_pack->GetPackage()->StringsToWrite());
  if (linesP.empty())
  { 
    // comments
    lines.push_back(MfExportUtil::GetMf6CommentHeader());
    lines.push_back("BEGIN OPTIONS");       
    const int *IHEDFM,  *IBUDFL, *IHEDUN;
    const char* CHEDFM;
    if (p1->GetField(Packages::OCpack::IHEDFM, &IHEDFM) && IHEDFM &&    
        p1->GetField(Packages::OCpack::CHEDFM, &CHEDFM) && CHEDFM &&
        p->GetField(Packages::OCTpack::IBUDFL, &IBUDFL) && IBUDFL &&
        p1->GetField(Packages::OCpack::IHEDUN, &IHEDUN) && IHEDUN)
    {
      // Heads
      CStr baseName;
      util::StripPathFromFilename(m_pack->GetNative()->FileName(), baseName);
      util::StripExtensionFromFilename(baseName, baseName);

      // if any file has SAVE_FLOWS on then we will write Save budget
      int saveFlows(0);
      g->GetIntVar("MF6_SAVE_FLOWS", saveFlows);
      if (saveFlows) lines.push_back("  BUDGET FILEOUT " + baseName + ".ccf");

      // if head unit number is specified then we save the heads with
      // the basename.hed
      if (*IHEDUN > 0) lines.push_back("  HEAD FILEOUT " + baseName + ".hed");

      //   CStr iprn[22] = {"10G11.4","11G10.3","9G13.6","15F7.1","15F7.2","15F7.3",
      //                    "15F7.4","20F5.0","20F5.1","20F5.2","20F5.3","20F5.4",
      //                    "10G11.4","10F6.0","10F6.1","10F6.2","10F6.3","10F6.4",
      //                    "10F6.5","5G12.5","6G11.4","7G9.2"};
      // print format
      int format[22][3] = 
      { {10,11,4}, {11,10,3}, {9,13,6}, {15,7,1}, {15,7,2}, {15,7,3},
        {15,7,4}, {20,5,0}, {20,5,1}, {20,5,2}, {20,5,3}, {20,5,4},
        {10,11,4}, {10,6,0}, {10,6,1}, {10,6,20}, {10,6,3}, {10,6,4},
        {10,6,5}, {5,12,5}, {6,11,4}, {7,9,2}
      };
      if (*IHEDFM >= 0 && *IHEDFM < 22)
      {
        int i = *IHEDFM;
        std::stringstream ss;
        ss << "  HEAD PRINT_FORMAT COLUMNS " << format[i][0]
           << " WIDTH " << format[i][1] << " DIGITS " << format[i][2];
        lines.push_back(ss.str());
      }
    } 
    lines.push_back("END OPTIONS"); 
    lines.push_back("");
  }

  const int *spid, *tsnum;
  const int *ibudfl, *icbcfl, *hdpr, *hdsv;
  if (p->GetField(Packages::OCTpack::SPID, &spid) && spid &&
      p->GetField(Packages::OCTpack::TSNum, &tsnum) && tsnum &&
      p->GetField(Packages::OCTpack::IBUDFL, &ibudfl) && ibudfl &&
      p->GetField(Packages::OCTpack::ICBCFL, &icbcfl) && icbcfl &&
      p->GetField(Packages::OCTpack::Hdpr, &hdpr) && hdpr &&
      p->GetField(Packages::OCTpack::Hdsv, &hdsv) && hdsv)
  {
    std::stringstream ss;
    ss << "PERIOD " << *spid << " TS " << *tsnum
       << " PRINT_HEAD " << (0 != *hdpr) << " PRINT_BUDGET " << (0 != *ibudfl)
       << " SAVE_HEAD " << (0 != *hdsv) << " SAVE_BUDGET " << (0 != *icbcfl);
    lines.push_back(ss.str());
  }

  comments.assign(lines.size(), "");
  m_pack->AddToStoredLinesDesc(lines, comments);
  //m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Oc::ExportMf6Oc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Oc::WriteFinal ()
{
  std::vector<CStr>& lines2(m_pack->GetPackage()->StringsToWrite()), lines1(lines2), lines;
  std::vector<CStr>& desc2(m_pack->GetPackage()->StringDescriptions());
  lines2.clear();
  desc2.clear();

  std::vector< std::vector<int> > values;
  CStr tmpStr;
  for (size_t i=0; i<lines1.size(); ++i)
  {
    if (lines1[i].Find("PERIOD") == 0)
    {
      std::stringstream ss;
      ss << lines1[i];
      // format of the string
      //ss << "PERIOD " << *spid << " TS " << *tsnum
      // << " PRINT_HEAD " << !*hdpr << " PRINT_BUDGET " << !*ibudfl
      // << " SAVE_HEAD " << !*hdsv << " SAVE_BUDGET " << !icbcfl;
      std::vector<int> v(6, 0);
      for (int j=0; j < 6; ++j)
        ss >> tmpStr >> v[j];
      values.push_back(v);
    }
    else
      lines.push_back(lines1[i]);
  }

  // create a set of time steps for each stress period
  std::vector< std::set<int> >
    vecSp_TimeSteps(m_pack->GetGlobal()->NumPeriods(), std::set<int>());
  std::vector< std::vector<int> >
    vecSp_PrintOptions(vecSp_TimeSteps.size(), std::vector<int>(4, 0));
  for (size_t i=0; i<values.size(); ++i)
  {
    int spid = values[i][0];
    int tsnum = values[i][1];
    vecSp_TimeSteps[spid-1].insert(tsnum);
    // we need a union of all print/save options for a given stress period
    if (values[i][2]) vecSp_PrintOptions[spid-1][0] = 1; // print head
    if (values[i][3]) vecSp_PrintOptions[spid-1][1] = 1; // print budget
    if (values[i][4]) vecSp_PrintOptions[spid-1][2] = 1; // save head
    if (values[i][5]) vecSp_PrintOptions[spid-1][3] = 1; // save budget
  }
  // determine for each stress period if we should only write the first time step,
  // the last time step, or all time steps
  // 0 - FIRST, 1 - LAST, 2 - ALL
  std::vector<int> vecSpWriteOption(vecSp_TimeSteps.size(), -1);
  for (size_t i=0; i<vecSp_TimeSteps.size(); ++i)
  {
    // nothing specified for this stress period
    if (vecSp_TimeSteps[i].empty()) continue;

    int spid = (int)i + 1;
    int numTsInSp = GetNumTimeStepsForPeriod(spid);
    
    // if we have more than 1 time step then we will write ALL time steps
    if (vecSp_TimeSteps[i].size() > 1) vecSpWriteOption[spid-1] = 2;
    else
    {
      // if we only have 1 time step then we check if it is the first or the last
      // otherwise we do all
      int ts = *vecSp_TimeSteps[i].begin();
      if (1 == ts)              vecSpWriteOption[spid-1] = 0;
      else if (numTsInSp == ts) vecSpWriteOption[spid-1] = 1;
      else                      vecSpWriteOption[spid-1] = 2;
    }
  }

  std::vector<int> processedSp(m_pack->GetGlobal()->NumPeriods(), 0);
  for (size_t i=0; i<values.size(); ++i)
  {
    int spid = values[i][0];
    if (processedSp[spid-1]) continue;
    if (vecSpWriteOption[spid-1] < 0) continue;

    CStr option("ALL");
    if (0 == vecSpWriteOption[spid-1])      option = "FIRST";
    else if (1 == vecSpWriteOption[spid-1]) option = "LAST";

    std::stringstream ss;
    ss << "BEGIN PERIOD " << spid;
    lines.push_back(ss.str());

    if (vecSp_PrintOptions[spid-1][0]) lines.push_back("  PRINT HEAD " + option);
    if (vecSp_PrintOptions[spid-1][1]) lines.push_back("  PRINT BUDGET " + option);
    if (vecSp_PrintOptions[spid-1][2]) lines.push_back("  SAVE HEAD " + option);
    if (vecSp_PrintOptions[spid-1][3]) lines.push_back("  SAVE BUDGET " + option);

    lines.push_back("END PERIOD");
    lines.push_back("");
    processedSp[spid-1] = 1;
  }

  std::vector<CStr> desc(lines.size(), "");
  TmpPackageNameChanger tmp(m_pack->GetPackage(), "OC");
  m_pack->AddToStoredLinesDesc(lines, desc);
  m_pack->WriteStoredLines();
} // NativeExpMf6Oc::WriteFinal
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int NativeExpMf6Oc::GetNumTimeStepsForPeriod (int a_sp)
{
  int rval(1);

  MfPackage *p = m_pack->GetGlobal()->GetPackage(Packages::DIS);
  if (!p) p = m_pack->GetGlobal()->GetPackage(Packages::DISU);
  if (!p) return -1;

  const int* NSTP(0);
  if (p->GetField(Packages::DisPack::NSTP, &NSTP) && NSTP)
  {
    rval = NSTP[a_sp-1];
  }
  return rval;
} // NativeExpMf6Oc::GetNumTimeStepsForCurrentPeriod


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
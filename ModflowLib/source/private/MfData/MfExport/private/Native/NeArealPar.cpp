//------------------------------------------------------------------------------
// FILE      NeArealPar.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NeArealPar.h>

#include <sstream>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>

using namespace MfData::Export;
namespace
{
  CStr LineWithParDesc (CStr a_type)
  {
    CStr desc;
    int lineNum = 7;
    if ("EVT" == a_type) lineNum = 8;
    desc.Format(" %d. [Pname [Iname] [I%sPF]]", lineNum, a_type);
    return desc;
  }
} // unnamed namespace
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NeArealPar::NeArealPar ()
: m_type()
, m_laySpecified(false)
, m_NETSEG(-1)
, m_pack(NULL)
{
} // MfNeArealPar::MfNeArealPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NeArealPar::~NeArealPar ()
{
} // MfNeArealPar::~MfNeArealPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NeArealPar::Line1 ()
{
  if (!ClustersDefinedFromStart()) return;
  // count # parameters that have clusters
  int nPar = 0;
  ParamList* pList;
  Parameters::GetParameterList(&pList);
  std::vector<Param> params = MfExportUtil::GetParamsOfType(m_type);
  for (size_t i=0; i<params.size(); ++i)
  {
    if (!params[i].m_clust.empty()) nPar++;
  }
  if (nPar > 0)
  {
    CStr ln; ln.Format("PARAMETER %5d", nPar);
    CStr d; d.Format(" 1. [PARAMETER NP%s]", m_type);
    m_pack->AddToStoredLinesDesc(ln, d);
  }
} // NeArealPar::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NeArealPar::Lines3to4 (bool beginExport)
{
  if (!ClustersDefinedFromStart() && beginExport) return;

  int w = util::RealWidth();
  int flg = STR_FULLWIDTH;
  CStr ln, desc;
  ParamList* pList;
  Parameters::GetParameterList(&pList);

  std::vector<Param> params = MfExportUtil::GetParamsOfType(m_type);
  for (size_t i=0; i<params.size(); ++i)
  {
    Param& p = params[i];
    if (p.m_clust.empty()) continue;

    std::vector<CStr> names;
    std::vector<double> vals;
    if (p.m_pilotPoints)
    {
      pList->GetPilotPtValues(p.m_scatIndex, vals);
      for (size_t i=1; i<=vals.size(); i++)
      {
        CStr pName; pName.Format("pp%d_%d", p.m_scatIndex, i);
        names.push_back(pName);
      }
    }
    else
    {
      names.push_back(p.m_name);
      vals.push_back(p.m_value);
    }

    for (size_t q=0; q<names.size(); ++q)
    {
      CStr pName = names[q];
      MfExportUtil::InsertSingleQuotesInName(pName);
      while (pName.GetLength() < 12) pName += " ";
      ln.Format("%s %s %s %5d", pName, m_type, STR(vals[q],-1,w,flg), 1);
      desc = " 3. [PARNAM PARTYP Parval NCLU [INSTANCES NUMINST]]";

      int nInst = 1, nClst = 1;
      if (!p.m_instNames.empty()) nInst = (int)p.m_instNames.size();
      nClst = (int)p.m_clust.size();
      if (nClst % nInst) ASSERT(0);
      nClst = nClst / nInst;

      if (nInst > 1)
      {
        CStr tmp;
        tmp.Format(" INSTANCES %5d", p.m_clust.size());
        ln += tmp;
      }
      m_pack->AddToStoredLinesDesc(ln, desc);

      int cnt(0);
      for (int ii=0; ii<nInst; ++ii)
      {
        CStr iName;
        if (!p.m_instNames.empty() &&
            (ClustersDefinedFromStart() || p.m_instNames.size() > 1))
        {
          iName = p.m_instNames[ii];
          MfExportUtil::InsertSingleQuotesInName(iName);
          while (iName.GetLength() < 12) iName += " ";
          CStr cmt = "4a. INSTNAM";
          if (m_type == "ETS") cmt.Replace("4a", "3a");
          m_pack->AddToStoredLinesDesc(iName, cmt);
        }

        for (int cc=0; cc<nClst; ++cc)
        {
          PClust clst = p.m_clust[cnt];
          cnt++;

          CStr mltName = clst.m_mlt;
          if (names.size() > 1) mltName = names[q];
          while (mltName.GetLength() < 10) mltName += " ";

          while (clst.m_zon.GetLength() < 10) clst.m_zon += " ";

          ln.Format("%s %s %5d", mltName, clst.m_zon, clst.m_iz.front());
          desc = "4b. [Mltarr Zonarr IZ]";
          if (m_type == "ETS") desc.Replace("4b", "3b");
          m_pack->AddToStoredLinesDesc(ln, desc);
        }
      }
    }
  }
} // NeArealPar::Lines3to4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NeArealPar::LineWithPar (int a_sp, CStr& a_line5)
{
  ParamList* pList;
  Parameters::GetParameterList(&pList);
  // count number of RCH parameters
  std::vector<CStr> pnames;
  std::vector<Param> params = MfExportUtil::GetParamsOfType(m_type);
  for (size_t i=0; i<params.size(); ++i)
  {
    Param& p = params[i];
    if (p.m_clust.empty()) continue;

    // see if a cluster for this par is used in this stress period
    bool found(0);
    for (size_t i=0; !found && i<p.m_clust.size(); ++i)
    {
      if (p.m_clust[i].m_lay == a_sp)
      {
        found = true;

        std::vector<CStr> names;
        if (p.m_pilotPoints)
        {
          std::vector<double> vals;
          pList->GetPilotPtValues(p.m_scatIndex, vals);
          for (size_t q=1; q<=vals.size(); ++q)
          {
            CStr nm; nm.Format("pp%d_%d", p.m_scatIndex, q);
            names.push_back(nm);
          }
        }
        else
        {
          names.push_back(p.m_name);
        }

        for (size_t j=0; j<names.size(); ++j)
        {
          pnames.push_back(names[j]);
          MfExportUtil::InsertSingleQuotesInName(pnames.back());
          while (pnames.back().GetLength() < 12) pnames.back() += " ";
          if (p.m_clust.size() > 1)
          {
            CStr iNam;
            iNam.Format(" SP_%d", a_sp);
            pnames.back() += iNam;
          }
        }
      }
    }
  }

  CStr ln, desc, tmpStr;
  // modify line 5 - we had to count number of parameters
  std::stringstream ss;
  ss << a_line5;
  // read the first 2 variables on this line
  int i1, i2;
  ss >> i1 >> i2;
  std::getline(ss, tmpStr);
  if ("RCH" == m_type)
  {
    i1 = static_cast<int>(pnames.size());
  }
  else
  {
    i2 = static_cast<int>(pnames.size());
  }

  a_line5.Format("%5d %5d%s", i1, i2, tmpStr);

  // line 7 - parameter names
  desc = LineWithParDesc(m_type);
  for (size_t i=0; i<pnames.size(); ++i)
  {
    m_pack->AddToStoredLinesDesc(pnames[i], desc);
  }
} // NeArealPar::LineWithPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NeArealPar::SetPackage (NativePackExp* a_p)
{
  m_pack = a_p;
  if (m_pack)
  {
    MfPackage* p = m_pack->GetPackage();
    if (p->PackageName() == Packages::RCH) m_type = "RCH";
    else if (p->PackageName() == Packages::EVT) m_type = "EVT";
    else if (p->PackageName() == Packages::ETS)
    {
      m_type = "ETS";
      m_pack->GetGlobal()->GetIntVar("ETS NETSEG", m_NETSEG);
    }
    CStr var;
    var.Format("%s Spec Layer", m_type);
    int iVal(0);
    m_pack->GetGlobal()->GetIntVar(var, iVal);
    if (iVal) m_laySpecified = true;
  }
} // NeArealPar::SetPackage
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int NeArealPar::NumPar ()
{
  ParamList *pList;
  Parameters::GetParameterList(&pList);
  // count number of parameters
  std::vector<Param> params = MfExportUtil::GetParamsOfType(m_type);
  int npar = static_cast<int>(params.size());
  for (size_t i=0; i<params.size(); ++i)
  {
    Param& p = params[i];
    if (p.m_clust.empty()) continue;
    if (p.m_pilotPoints)
    {
      std::vector<double> vals;
      pList->GetPilotPtValues(p.m_scatIndex, vals);
      if (!vals.empty()) npar += ((int)vals.size() - 1);
    }
  }
  return npar;
} // NeArealPar::NumPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NeArealPar::ClustersDefinedFromStart ()
{
  ParamList* pList;
  Parameters::GetParameterList(&pList);
  if (pList->Size() < 1) return false;

  CStr pFile = pList->GetSourceFile();
  bool clstInFile(false), clstExist(false);
  std::vector<Param> rchList = MfExportUtil::GetParamsOfType(m_type);
  for (size_t i=0; i<rchList.size(); ++i)
  {
    if (rchList[i].m_clustInParamFile) clstInFile = true;
    if (!rchList[i].m_clust.empty()) clstExist = true;
  }
  // if we have a GMS parameter file then we return clstInFile because the RCH
  // parameters either ALL use clusters or ALL use key values
  if (!pFile.IsEmpty()) return clstInFile;

  // if there is no GMS parameter file we should have clusters
  if (pFile.IsEmpty() && !clstExist) ASSERT(0);
  return true;
} // NeArealPar::ClustersDefinedFromStart
//------------------------------------------------------------------------------
/// \brief Get the parameter name for this stress period
//------------------------------------------------------------------------------
static void ParStressName (CStr& name, CStr& nameToWrite,
                           std::vector<Param>::iterator& p)
{
  // find the correct instance name with the correct case that was
  // read when we read the parameter definition
  for (size_t j=0; nameToWrite.IsEmpty() && j<p->m_instNames.size(); j++)
  {
    if (name.CompareNoCase(p->m_instNames.at(j)) == 0)
      nameToWrite = p->m_instNames.at(j);
  }
  if (nameToWrite.IsEmpty() && !p->m_instNames.empty()) ASSERT(0);
} // ParStressName
//------------------------------------------------------------------------------
/// \brief write any parameters that have instances in this stress period
//------------------------------------------------------------------------------
bool NeArealPar::WriteStressPar (int a_sp)
{
  bool rval = false;
  if (!ClustersDefinedFromStart()) return rval;

  std::vector<Param> plist = MfExportUtil::GetParamsOfType(m_type);
  CStr desc = LineWithParDesc(m_type);
  std::vector<Param>::iterator p(plist.begin());
  for (; p != plist.end(); ++p)
  {
    // loop through the m_instStress map of the parameter
    // we want to find any instance used in this stress period
    std::map<CStr, std::vector<int> >::iterator i(p->m_instStress.begin());
    for (; i != p->m_instStress.end(); ++i)
    {
      CStr name(i->first), nameToWrite;
      ParStressName(name, nameToWrite, p);
      std::vector<int>& v = i->second;
      if (std::find(v.begin(), v.end(), a_sp) != v.end())
      {
        CStr pname(p->m_name);
        MfExportUtil::InsertSingleQuotesInName(pname);
        while (pname.GetLength() < 12) pname += " ";
        MfExportUtil::InsertSingleQuotesInName(nameToWrite);
        while (nameToWrite.GetLength() < 12) nameToWrite += " ";
        CStr ln; ln.Format("%s %s", pname, nameToWrite);
        m_pack->AddToStoredLinesDesc(ln, desc);
        rval = true;
      }
    }
  }
  return rval;
} // NeArealPar::WriteStressPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NeArealPar::RewriteFileWithParameters ()
{
  // check if parameters already written
  if (ClustersDefinedFromStart()) return;

  // get a copy of the current lines and descriptions
  MfPackage* p = m_pack->GetPackage();
  std::vector<CStr> oldLine = p->StringsToWrite(),
                    oldDesc = p->StringDescriptions();
  // clear the current lines
  std::vector<CStr> &lines(p->StringsToWrite()),
                    &descs(p->StringDescriptions());
  lines.clear();
  descs.clear();

  CStr ln, desc;
  if ("ETS" != m_type)
  {
    ln.Format("PARAMETER %5d", NumPar());
    desc.Format(" 1. [PARAMETER NP%s]", m_type);
  }
  else
  {
    std::stringstream ss;
    ss << oldLine[0];
    int iVar[4];
    for (int i=0; i<4; ++i) ss >> iVar[i];
    ln.Format("%5d %5d %5d %5d", iVar[0], iVar[1], NumPar(), iVar[3]);
    desc = oldDesc[0];
  }
  m_pack->AddToStoredLinesDesc(ln, desc);

  // the number of values on line5 depends on the package and if the
  // layer is specified
  int nValsLine5 = 1;
  if ("RCH" != m_type) nValsLine5 = 3;
  if (m_laySpecified) nValsLine5++;
  if (m_NETSEG > 1) nValsLine5++;
  if (m_NETSEG > 1 && !m_laySpecified) nValsLine5++;
  // which line in each stress period may have a parameter
  int parIdx = 0;
  if ("RCH" != m_type) parIdx = 1;

  // add back line 2
  if ("ETS" != m_type) m_pack->AddToStoredLinesDesc(oldLine[0], oldDesc[0]);
  int curLine = 1;
  if (oldDesc.size() > 1 && oldDesc[1].Find("2b") != -1)
  {
    m_pack->AddToStoredLinesDesc(oldLine[1], oldDesc[2]);
    curLine = 2;
  }
  Lines3to4(false);
  for (int i=0; i<m_pack->GetGlobal()->NumPeriods(); ++i)
  {
    // add line 5 back
    m_pack->AddToStoredLinesDesc(oldLine[curLine], oldDesc[curLine]);
    CStr& line5 = lines[lines.size()-1];
    curLine++; // move to next line

    std::stringstream ss;
    ss << line5;
    // get the values on line 5
    for (int j=0; j<nValsLine5; ++j)
    {
      int iVar(0);
      ss >> iVar;
      if ("ETS" == m_type && j == 3 && !m_laySpecified) continue;
      if (parIdx == j)
      {
        if (iVar > 0)
        {
          LineWithPar(i+1, line5);
          RemoveArrayFile(oldLine[curLine]);
          if (MfExportUtil::ArrayWriteNextLineInternal(m_pack->GetNative(),
                                                       oldLine[curLine])) curLine++;
          curLine++; // move to next line
        }
      }
      else if (iVar > -1)
      {
        // add back line 8
        m_pack->AddToStoredLinesDesc(oldLine[curLine], oldDesc[curLine]);
        if (MfExportUtil::ArrayWriteNextLineInternal(m_pack->GetNative(),
                                                     oldLine[curLine]))
        {
          curLine++;
          m_pack->AddToStoredLinesDesc(oldLine[curLine], "");
        }
        curLine++; // move to next line
        if ("ETS" == m_type && j+1 == nValsLine5)
        { // this is PETM
          m_pack->AddToStoredLinesDesc(oldLine[curLine], oldDesc[curLine]);
          if (MfExportUtil::ArrayWriteNextLineInternal(m_pack->GetNative(),
                                                       oldLine[curLine]))
          {
            curLine++;
            m_pack->AddToStoredLinesDesc(oldLine[curLine], "");
          }
          curLine++; // move to next line
        }
      }
    }
  }

} // NeArealPar::RewriteFileWithParameters
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NeArealPar::RemoveArrayFile (CStr a_line)
{
  CStr ln;
  std::stringstream ss;
  ss << a_line;
  ss >> ln >> ln;
  // we don't need this array file so remove it
  CStr fname = m_pack->GetNative()->FileName();
  util::StripFileFromFilename(fname, fname);
  fname += ln;
  remove(fname);
} // NeArealPar::RemoveArrayFile

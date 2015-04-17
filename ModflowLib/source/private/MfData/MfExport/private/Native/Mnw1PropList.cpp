//------------------------------------------------------------------------------
// FILE      H5BcList.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private\MfData\MfExport\private\Native\Mnw1PropList.h>

// 3. Standard library headers

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
//#include <private/H5DataReader/H5DataSetReader.h>
//#include <private/H5DataReader/H5DataSetWriter.h>
//#include <private/H5DataReader/H5DataSetWriterSetup.h>
//#include <private/MfData/MfExport/private/Mf2kNative.h>
//#include <private/MfData/MfExport/private/Native/NativePackExp.h>
//#include <private/MfData/MfExport/private/TxtExporter.h>
//#include <private/MfData/MfGlobal.h>
//#include <private/MfData/Packages/MfPackage.h>
//#include <private/MfData/Packages/MfPackFields.h>
//#include <private/MfData/Packages/MfPackStrings.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------

//------------------------------------------------------------------------------
namespace
{
} // unnamed namespace


//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
Mnw1PropList::Mnw1PropList()
: m_nextWellNumber(0)
{
} // Mnw1PropList::Mnw1PropList
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void Mnw1PropList::NewStressPeriod ()
{
  m_used.clear();
  m_used.resize(m_cellIds.size(), false);
} // Mnw1PropList::NewStressPeriod
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<int> Mnw1PropList::GetPropIndicees (
  const CStr& a_siteName
, const std::vector<int>& a_cellids)
{
  size_t startIndex;

  if (!a_siteName.empty() && MissingSiteName(a_siteName))
  {
    AppendSiteWell(a_siteName, a_cellids, startIndex);
    return BuildPropIndicees(startIndex, a_cellids);
  }
  else if (!a_siteName.empty() && 
           MatchingSiteCells(a_siteName, a_cellids, startIndex))
  {
    return BuildPropIndicees(startIndex, a_cellids);
  }
  else if (FoundCellSet(a_cellids, startIndex))
  {
    return BuildPropIndicees(startIndex, a_cellids);
  }
  else
  {
    AppendNewWell(a_cellids, startIndex);
    return BuildPropIndicees(startIndex, a_cellids);
  }
} // Mnw1PropList::GetPropIndicees
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
const std::vector<int>& Mnw1PropList::GetCellIds ()
{
  return m_cellIds;
} // Mnw1PropList::GetCellIds
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
const std::vector<int>& Mnw1PropList::GetWellIds ()
{
  return m_wellIds;
} // Mnw1PropList::GetWellIds
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
const std::vector<CStr>& Mnw1PropList::GetWellNames ()
{
  return m_names;
} // Mnw1PropList::GetWellNames
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
const std::vector<char>& Mnw1PropList::GetIsSiteName ()
{
  return m_isSiteName;
} // Mnw1PropList::GetWellNames
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool Mnw1PropList::MissingSiteName (
  const CStr& a_siteName)
{
  std::vector<CStr>::const_iterator nameIter = 
    std::find_if(m_names.begin(), m_names.end(),
                 util::CaseInsensitiveEqual(a_siteName));
  return nameIter == m_names.end();
} // Mnw1PropList::MissingSiteName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool Mnw1PropList::MatchingSiteCells (
  const CStr& a_siteName
, const std::vector<int>& a_cellids
, size_t& a_startIndex)
{
  bool found = false;

  // find first item with site name
  std::vector<CStr>::const_iterator nameIter = 
    std::find_if(m_names.begin(), m_names.end(),
                 util::CaseInsensitiveEqual(a_siteName));
  if (nameIter != m_names.end())
  {
    a_startIndex = nameIter - m_names.begin();
    if (!m_used[a_startIndex])
    {
      // build a set of existing cellids with the site name
      std::vector<CStr>::const_iterator nameEndIter = nameIter;
      util::CaseInsensitiveEqual equalToSiteName(a_siteName);
      for (; nameEndIter != m_names.end() && equalToSiteName(*nameEndIter);
           ++nameEndIter)
      {
      }
      std::set<int> existingCellIds(m_cellIds.begin() + a_startIndex, 
                                    m_cellIds.begin() + a_startIndex + 
                                    (nameEndIter - nameIter));

      // build a set of the checkedCellIds
      std::set<int> checkedCellIds(a_cellids.begin(), a_cellids.end());

      found = checkedCellIds == existingCellIds;
    }
  }
  return found;
} // Mnw1PropList::MatchingSiteCells
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<int> Mnw1PropList::BuildPropIndicees (
  size_t a_startIndex
, const std::vector<int>& a_cellids)
{
  std::vector<int> properties;
  std::map<int, int> cellIdsToProp;

  for (size_t i = 0; i < a_cellids.size(); ++i)
  {
    size_t propIndex = a_startIndex + i;
    cellIdsToProp[m_cellIds[propIndex]] = (int)propIndex;
    m_used[a_startIndex + i] = true;
  }

  for (std::vector<int>::const_iterator cell = a_cellids.begin();
       cell != a_cellids.end(); ++cell)
  {
    properties.push_back(cellIdsToProp[*cell]);
  }

  return properties;
} // Mnw1PropList::BuildPropIndicees
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void Mnw1PropList::AppendSiteWell (
  const CStr& a_siteName
, const std::vector<int>& a_cellids
, size_t& a_startIndex)
{
  std::vector<int> properties;
  a_startIndex = m_cellIds.size();
  m_nextWellNumber++;
  for (std::vector<int>::const_iterator cellId = a_cellids.begin();
       cellId != a_cellids.end(); ++cellId)
  {
    int wellId;
    if (a_cellids.size() == 1)
      wellId = 0;
    else
      wellId = m_nextWellNumber;
    AppendItem(*cellId, a_siteName, true, wellId);
  }
} // Mnw1PropList::AppendSiteWell
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool Mnw1PropList::FoundCellSet (
  const std::vector<int>& a_cellids
, size_t& a_startIndex)
{
  bool found = false;

  // build cell set to find
  std::set<int> cellsToFind(a_cellids.begin(), a_cellids.end());

  CStr currSetName;
  std::set<int> cellsToCheck;
  for (size_t i = 0; i < m_names.size(); ++i)
  {
    if (!m_isSiteName[i] && !m_used[i])
    {
      if (m_names[i] != currSetName)
      {
        if (cellsToCheck == cellsToFind)
        {
          found = true;
          break;
        }
        a_startIndex = i;
        currSetName = m_names[i];
        cellsToCheck.clear();
      }
      cellsToCheck.insert(m_cellIds[i]);
    }
  }

  if (cellsToCheck == cellsToFind)
    found = true;

  return found;
} // Mnw1PropList::FoundCellSet
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void Mnw1PropList::AppendNewWell (
  const std::vector<int>& a_cellids
, size_t& a_startIndex)
{
  // find an unused name
  CStr wellName;
  wellName.Format("Well-%d", ++m_nextWellNumber);
  std::vector<CStr>::iterator nameIter = std::find_if(m_names.begin(), m_names.end(),
                                          util::CaseInsensitiveEqual(wellName));
  while (nameIter != m_names.end())
  {
    wellName.Format("Well-%d", ++m_nextWellNumber);
    nameIter = std::find_if(m_names.begin(), m_names.end(),
                            util::CaseInsensitiveEqual(wellName));
  }

  // get index where new names will start
  a_startIndex = m_names.size();

  // add name and cellids
  for (std::vector<int>::const_iterator cellId = a_cellids.begin();
       cellId != a_cellids.end(); ++cellId)
  {
    int wellId;
    if (a_cellids.size() == 1)
      wellId = 0;
    else
      wellId = m_nextWellNumber;
    AppendItem(*cellId, wellName, false, wellId);
  }
} // Mnw1PropList::AppendNewWell
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void Mnw1PropList::AppendItem (
  int a_cellId
, const CStr& a_name
, bool a_isSite
, int  a_wellNumber)
{
    m_cellIds.push_back(a_cellId);
    m_wellIds.push_back(a_wellNumber);
    m_names.push_back(a_name);
    m_isSiteName.push_back(a_isSite);
    m_used.push_back(false);
} // Mnw1PropList::AppendItem




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
#include <private/MfData/MfExport/private/Native/Mnw1PropList.h>

// 3. Standard library headers

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers

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

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/Mnw1PropList.t.h>
#include <private/MfLibAsserts.h>
//------------------------------------------------------------------------------
void Mnw1PropListT::setUp ()
{
} // Mnw1PropListT::setUp
//------------------------------------------------------------------------------
void Mnw1PropListT::tearDown ()
{
} // Mnw1PropListT::setUp
//------------------------------------------------------------------------------
void Mnw1PropListT::testCreateClass ()
{
  Mnw1PropList *p = new Mnw1PropList();
  TS_ASSERT(p);
  if (p) delete(p);
} // Mnw1PropListT::testCreateClass
//------------------------------------------------------------------------------
void Mnw1PropListT::testIt ()
{
  Mnw1PropList wpl;

  int cells[]   = { 0, 1, 0, 3, 4, 5, 6, 7, 8, 9, 10 };
  int expectedCellIds[] = { 0, 1, 0, 3, 0, 1, 0, 1, 1, 0, 1 };
  int expectedProps[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  std::vector<int> expectedPropsVec;
  const int NUM_NAMES = 11;
  char *names[NUM_NAMES] = { "Site-1", "Site-1", "Site-2", "Site-2", "Well-3",
    "Well-3", "Well-4", "Well-4", "Well-5", "Well-5",
    "Well-6" };
  std::vector<CStr> namesVec(names, names+NUM_NAMES);
  CStr *expectedNames = &namesVec[0];
  int expectedWellIds[] = { 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 0 };
  char expectedIsSite[] = { true, true, true, true, false, false, false, false,
    false, false, false };
  std::vector<int> cellIds;
  std::vector<int> properties;

  cellIds.insert(cellIds.begin(), cells, cells+2);
  properties = wpl.GetPropIndicees("Site-1", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps, 2, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 2, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 2, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 2, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 2, wpl.GetIsSiteName());

  // second site entry with different name moves on to new properties position
  cellIds.clear();
  cellIds.insert(cellIds.begin(), cells+2, cells+4);
  properties = wpl.GetPropIndicees("Site-2", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps+2, 2, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 4, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 4, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 4, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 4, wpl.GetIsSiteName());

  // second site entry with same name and cells moves on to new position
  cellIds.clear();
  cellIds.insert(cellIds.begin(), cells, cells+2);
  properties = wpl.GetPropIndicees("Site-1", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps+4, 2, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 6, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 6, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 6, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 6, wpl.GetIsSiteName());

  // new well entry moves on to new properties position
  properties = wpl.GetPropIndicees("", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps+6, 2, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 8, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 8, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 8, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 8, wpl.GetIsSiteName());

  // new well entry with same cellid moves on to new properties position
  cellIds[0] = 1;
  cellIds[1] = 0;
  properties = wpl.GetPropIndicees("", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps+8, 2, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 10, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 10, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 10, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 10, wpl.GetIsSiteName());

  // new single cell well entry uses zero for wellid
  cellIds.clear();
  cellIds.push_back(1);
  properties = wpl.GetPropIndicees("", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps+10, 1, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());

  // new stess period should find the same places
  // also reversing cellIds above should reverse properties returned
  wpl.NewStressPeriod();
  cellIds.resize(2);
  cellIds[0] = 1;
  cellIds[1] = 0;
  properties = wpl.GetPropIndicees("Site-1", cellIds);
  expectedPropsVec.clear();
  expectedPropsVec.insert(expectedPropsVec.begin(), expectedProps, expectedProps+2);
  std::reverse(expectedPropsVec.begin(), expectedPropsVec.end());
  TS_ASSERT_EQUALS_VEC(expectedPropsVec, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());

  // second site entry with different name moves on to new properties position
  cellIds[0] = 3;
  cellIds[1] = 0;
  properties = wpl.GetPropIndicees("Site-2", cellIds);
  expectedPropsVec.clear();
  expectedPropsVec.insert(expectedPropsVec.begin(), expectedProps+2, expectedProps+4);
  std::reverse(expectedPropsVec.begin(), expectedPropsVec.end());
  TS_ASSERT_EQUALS_VEC(expectedPropsVec, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());

  // second site entry with same name moves on to new properties position
  cellIds[0] = 1;
  cellIds[1] = 0;
  properties = wpl.GetPropIndicees("Site-1", cellIds);
  expectedPropsVec.clear();
  expectedPropsVec.insert(expectedPropsVec.begin(), expectedProps+4, expectedProps+6);
  std::reverse(expectedPropsVec.begin(), expectedPropsVec.end());
  TS_ASSERT_EQUALS_VEC(expectedPropsVec, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());

  // new well entry moves on to new properties position
  properties = wpl.GetPropIndicees("", cellIds);
  expectedPropsVec.clear();
  expectedPropsVec.insert(expectedPropsVec.begin(), expectedProps+6, expectedProps+8);
  std::reverse(expectedPropsVec.begin(), expectedPropsVec.end());
  TS_ASSERT_EQUALS_VEC(expectedPropsVec, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());

  // new well entry with same cellid moves on to new properties position
  cellIds[0] = 0;
  cellIds[1] = 1;
  properties = wpl.GetPropIndicees("", cellIds);
  expectedPropsVec.clear();
  expectedPropsVec.insert(expectedPropsVec.begin(), expectedProps+8, expectedProps+10);
  std::reverse(expectedPropsVec.begin(), expectedPropsVec.end());
  TS_ASSERT_EQUALS_VEC(expectedPropsVec, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());
} // Mnw1PropListT::testIt

#endif

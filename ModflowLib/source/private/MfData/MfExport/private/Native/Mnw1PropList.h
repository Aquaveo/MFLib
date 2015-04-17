//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MNW1PROPLIST_H
#define MNW1PROPLIST_H
#include <private\util\util.h>

namespace MfData
{
  namespace Export
  {
    class Mnw1PropList
    {
    public:
      Mnw1PropList();

      void NewStressPeriod ();
      std::vector<int> GetPropIndicees (
        const CStr& a_siteName,
        const std::vector<int>& a_cellids);
      const std::vector<int>& GetCellIds ();
      const std::vector<int>& GetWellIds ();
      const std::vector<CStr>& GetWellNames ();
      const std::vector<char>& GetIsSiteName ();

    protected:
      bool MissingSiteName (
        const CStr& a_siteName);
      bool MatchingSiteCells (
        const CStr& a_siteName, 
        const std::vector<int>& a_cellids,
        size_t& a_startIndex);
      std::vector<int> BuildPropIndicees (
        size_t a_startIndex,
        const std::vector<int>& a_cellids);
      void AppendSiteWell (
        const CStr& a_siteName,
        const std::vector<int>& a_cellids,
        size_t& a_startIndex);
      bool FoundCellSet (
        const std::vector<int>& a_cellids,
        size_t& a_startIndex);
      void AppendNewWell (
        const std::vector<int>& a_cellids,
        size_t& a_startIndex);
      void AppendItem (
        int a_cellId,
        const CStr& a_name,
        bool a_isSite,
        int a_wellNumber);

      std::vector<int> m_cellIds;
      std::vector<int> m_wellIds;
      std::vector<CStr> m_names;
      std::vector<char> m_isSiteName;
      std::vector<char> m_used;
      int m_nextWellNumber;
    };
  } // namespace Export
} // namespace MfData
#endif

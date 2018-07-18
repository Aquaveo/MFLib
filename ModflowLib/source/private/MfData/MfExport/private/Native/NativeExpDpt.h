//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPDPT_H
#define NATIVEEXPDPT_H

#include <map>

class NativeExpDptT;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {

    class NativeExpDpt : public NativePackExp
    {
      friend NativeExpDptT;
    public:
      NativeExpDpt();
      ~NativeExpDpt();
      virtual bool Export();

    private:
      NativeExpDpt(const NativeExpDpt& rhs);
      const NativeExpDpt& operator=(const NativeExpDpt& rhs);
      virtual void   AddToStoredLinesDesc(const char* a_line,
                                          const char* a_desc);

      void ExportArray (const CStr& a_array, int a_speciesId);
      void SetUpDescriptions();
      CStr GetDescription (const CStr& a_key);

      void Line1();

      std::map<CStr, CStr> m_descriptions;
    };

  }
}

#endif


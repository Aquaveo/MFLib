//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPBCT_H
#define NATIVEEXPBCT_H

#include <map>

class NativeExpBctT;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    struct BctDescription
    {
    public:
      BctDescription(CStr unstructured, CStr structured)
        : m_unstructured(unstructured)
        , m_structured(structured)
      {
      }
      CStr m_unstructured;
      CStr m_structured;
    };

    class NativeExpBct : public NativePackExp
    {
      friend NativeExpBctT;
    public:
      NativeExpBct();
      ~NativeExpBct();
      virtual bool Export();

    private:
      NativeExpBct(const NativeExpBct& rhs);
      const NativeExpBct& operator=(const NativeExpBct& rhs);
      virtual void   AddToStoredLinesDesc(const char* a_line,
                                          const char* a_desc);

      void ExportArray (const CStr& a_array, int a_speciesId);
      void SetUpDescriptions();
      CStr GetDescription (const CStr& a_key);

      void Line1aAnd1b();

      std::map<CStr, BctDescription> m_descriptions;
    };

  }
}

#endif


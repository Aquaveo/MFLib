//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPCLN_H
#define NATIVEEXPCLN_H

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
class NativeExpClnT;
namespace MfData
{
  namespace Export
  {
    class NativeExpCln : public NativePackExp
    {
      friend NativeExpClnT;
    public:
      enum ArraySizeEnum {ASE_LN7 = 8, ASE_LN8 = 7, ASE_LN9 = 9};

      NativeExpCln();
      virtual ~NativeExpCln();
      virtual bool Export();
    private:
      NativeExpCln(const NativeExpCln& rhs);
      const NativeExpCln& operator=(const NativeExpCln& rhs);

      virtual void   AddToStoredLinesDesc(const char* a_line,
                                          const char* a_desc);

      CStr Line0();
      CStr Line1();
      void Line2();
      void Line3();
      void Line4();
      void Line5();
      void Line6();
      void Line7();
      void Line8();
      void Line9();
      void Line10();
      void Line11();
      void Line12();
      void Line13();
      void Line14();
      void Line15();
      void Line16();

      void WriteCommentsCln();
      void WriteStoredLinesCln();
      void InitDescriptionMap ();
      CStr Desc(const CStr& a_line);
      void AddArrayLines (const CStr& a_name, const CStr& a_desc);

      std::map<CStr, CStr> m_mapDesc;
    };

  }
}

#endif


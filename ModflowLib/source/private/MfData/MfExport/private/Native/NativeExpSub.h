//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPSUB_H
#define NATIVEEXPSUB_H

class NativeExpSubT;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpSub : public NativePackExp
    {
      friend NativeExpSubT;
    public:
      NativeExpSub();
      ~NativeExpSub();
      virtual bool Export();

    private:
      NativeExpSub(const NativeExpSub& rhs);
      const NativeExpSub& operator=(const NativeExpSub& rhs);

      void Lines1to3();
      void Lines4to14();
      void WriteArrays(const char* const a_name,
                       int a_num,
                       const int* a_lay,
                       const CStr& a_desc,
                       int a_idx);
      void Line15();
      void Line16();
    };

  }
}

#endif


//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMNW1_H
#define NATIVEEXPMNW1_H

class NativeExpMnw1T;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpMnw1 : public NativePackExp
    {
      friend NativeExpMnw1T;
    public:
      NativeExpMnw1();
      ~NativeExpMnw1();
      virtual bool Export();

    private:
      NativeExpMnw1(const NativeExpMnw1& rhs);
      const NativeExpMnw1& operator=(const NativeExpMnw1& rhs);

      virtual void   AddToStoredLinesDesc(const char* a_line,
                                          const char* a_desc);
      void Line1to3();
      void Line4();
      void Line5();

    };

  }
}

#endif


//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPLAK_H
#define NATIVEEXPLAK_H

class NativeExpLakT;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpMf6Lak;
    class NativeExpLak : public NativePackExp
    {
      friend NativeExpLakT;
      friend NativeExpMf6Lak;
    public:
      NativeExpLak();
      ~NativeExpLak();
      virtual bool Export();

    private:
      NativeExpLak(const NativeExpLak& rhs);
      const NativeExpLak& operator=(const NativeExpLak& rhs);
      virtual void   AddToStoredLinesDesc(const char* a_line,
                                          const char* a_desc);

      void Line1to2();
      void Line3();
      void Line4();
      void Line5();
      void Line6();
      void Line7to8();
      void Line9();
    };

  }
}

#endif


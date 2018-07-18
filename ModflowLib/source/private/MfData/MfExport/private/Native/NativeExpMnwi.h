//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMNWI_H
#define NATIVEEXPMNWI_H

class NativeExpMnwiT;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpMnwi : public NativePackExp
    {
      friend NativeExpMnwiT;
    public:
      NativeExpMnwi();
      ~NativeExpMnwi();
      virtual bool Export();

    private:
      NativeExpMnwi(const NativeExpMnwi& rhs);
      const NativeExpMnwi& operator=(const NativeExpMnwi& rhs);

      void Line1();
      void Line2();
      void Line3();
    };

  }
}

#endif


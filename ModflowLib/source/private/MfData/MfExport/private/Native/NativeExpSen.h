//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPSEN_H
#define NATIVEEXPSEN_H

class NativeExpSenT;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpSen : public NativePackExp
    {
      friend NativeExpSenT;
    public:
      NativeExpSen();
      ~NativeExpSen();
      virtual bool Export();

    protected:
      virtual void   LastChanceBeforeWriting();

    private:
      NativeExpSen(const NativeExpSen& rhs);
      const NativeExpSen& operator=(const NativeExpSen& rhs);

      CStr Line1();
      CStr Desc1();
      CStr Line2();
      CStr Desc2();
      std::vector<CStr> Line3();
      std::vector<CStr> Desc3();
    };

  }
}

#endif


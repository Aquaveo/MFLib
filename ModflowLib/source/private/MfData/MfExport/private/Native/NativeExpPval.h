//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPPVAL_H
#define NATIVEEXPPVAL_H

class NativeExpPvalT;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpPval : public NativePackExp
    {
      friend NativeExpPvalT;
    public:
      NativeExpPval();
      ~NativeExpPval();
      virtual bool Export();

    private:
      NativeExpPval(const NativeExpPval& rhs);
      const NativeExpPval& operator=(const NativeExpPval& rhs);

      CStr Line1();
      CStr Desc1();
      std::vector<CStr> Line2();
      std::vector<CStr> Desc2();
      void CheckParameters();
      void ConvertNameIfPilotPoint(CStr& a_name);
    };

  }
}

#endif


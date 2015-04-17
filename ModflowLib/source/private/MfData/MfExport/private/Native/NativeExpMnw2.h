//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMNW2_H
#define NATIVEEXPMNW2_H

class NativeExpMnw2T;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpMnw2 : public NativePackExp
    {
      friend NativeExpMnw2T;
    public:
      NativeExpMnw2(bool a_h5);
      ~NativeExpMnw2();
      virtual bool Export();

    private:
      NativeExpMnw2(const NativeExpMnw2& rhs);
      const NativeExpMnw2& operator=(const NativeExpMnw2& rhs);

      std::vector<CStr>& WellIds();
      std::vector<int>&  PumpCap();
      std::vector<int>&  Qlimit();
      CStr Desc(int a_line);
      void Line1();
      void Line2ab();
      void Line2c();
      void Line2d();
      void Line2e();
      void Line2f();
      void Line2g();
      void Line2h();
      void Lines34();

      bool m_h5;
    };

  }
}

#endif


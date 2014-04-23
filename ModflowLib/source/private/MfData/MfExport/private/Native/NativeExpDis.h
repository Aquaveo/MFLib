//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPDIS_H
#define NATIVEEXPDIS_H

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
class NativeExpDisT;
namespace MfData
{
  namespace Export
  {
    class NativeExpDis : public NativePackExp
    {
      friend NativeExpDisT;
    public:
      NativeExpDis();
      ~NativeExpDis();
      virtual bool Export();
    private:
      NativeExpDis(const NativeExpDis& rhs);
      const NativeExpDis& operator=(const NativeExpDis& rhs);

      virtual void OnSetData();

      CStr Desc(int a_line, int a_layer=-1);
      CStr Line1();
      CStr Line2();
      void Line3();
      void Line4();
      void Line5();
      void Line6();
      std::vector<CStr> Desc6and7(int a_);
      std::vector<CStr> Line7();

      int m_nLay, m_nSp;
      std::vector<int> m_layCbd;
    };

  }
}

#endif


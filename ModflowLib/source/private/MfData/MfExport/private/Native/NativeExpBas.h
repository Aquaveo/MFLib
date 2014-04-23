//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPBAS_H
#define NATIVEEXPBAS_H

class NativeExpBasT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  class MfPackage;
  namespace Export
  {
    class NativeExpBas : public NativePackExp
    {
      friend NativeExpBasT;
    public:
      NativeExpBas();
      ~NativeExpBas();
      virtual bool Export();

    private:
      NativeExpBas(const NativeExpBas& rhs);
      const NativeExpBas& operator=(const NativeExpBas& rhs);

      virtual void OnSetData();

      CStr Line1();
      CStr Desc(int a_line, int a_lay=-1);
      void Line2();
      CStr Line3();
      void Line4();
      void ArrayToFile(MfPackage* p, int a_line);

      int NumLines(int a_line);

      int m_nLay;
    };

  }
}

#endif


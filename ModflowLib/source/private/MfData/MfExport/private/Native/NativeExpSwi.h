//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NativeExpSwi_H
#define NativeExpSwi_H

#include <private\MfData\MfExport\private\Native\NativePackExp.h>

class NativeExpSwiT;
class Param;

namespace MfData
{
  namespace Export
  {
    class NativeExpSwi : public NativePackExp
    {
      friend NativeExpSwiT;
    public:
      NativeExpSwi();
      ~NativeExpSwi();
      virtual bool Export();

    private:
      NativeExpSwi(const NativeExpSwi& rhs);
      const NativeExpSwi& operator=(const NativeExpSwi& rhs);

      //virtual void LastChanceBeforeWriting();
      //virtual void OnSetData();

      void Line1();
      void Line2a();
      void Line2b();
      void Line3a();
      void Line3b();
      void Line4();
      void Line5();
      void Line6();
      void Line7();
      void Line8();

      const int* m_nsrf;
      const int* m_nobs;
      const int* m_nsolver;
      const int* m_adaptive;
    }; // class NativeExpSwi

  }
}

#endif


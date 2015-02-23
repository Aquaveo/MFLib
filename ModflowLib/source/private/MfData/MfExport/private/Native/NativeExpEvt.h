//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPEVT_H
#define NATIVEEXPEVT_H

class NativeExpEvtT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\MfExport\private\Native\NeArealPar.h>
namespace MfData
{
  class MfPackage;
  namespace Export
  {
    class NativeExpEvt : public NativePackExp
    {
      friend NativeExpEvtT;
    public:
      NativeExpEvt();
      ~NativeExpEvt();
      virtual bool Export();

    private:
      NativeExpEvt(const NativeExpEvt& rhs);
      const NativeExpEvt& operator=(const NativeExpEvt& rhs);

      virtual void LastChanceBeforeWriting();
      virtual void OnSetData();

      void Line2();
      void Line5();
      void Line6();
      void Line7();
      void Line9();
      void Line10();
      void ArrayToFile(MfPackage* p, const CStr& a_desc);

      NeArealPar m_par;
      bool m_usg, m_unstructured;
    };

  }
}

#endif


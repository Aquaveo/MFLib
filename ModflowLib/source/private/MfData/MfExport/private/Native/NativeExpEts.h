//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPETS_H
#define NATIVEEXPETS_H

class NativeExpEtsT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\MfExport\private\Native\NeArealPar.h>
namespace MfData
{
  class MfPackage;
  namespace Export
  {
    class NativeExpEts : public NativePackExp
    {
      friend NativeExpEtsT;
    public:
      NativeExpEts();
      ~NativeExpEts();
      virtual bool Export();

    private:
      NativeExpEts(const NativeExpEts& rhs);
      const NativeExpEts& operator=(const NativeExpEts& rhs);

      virtual void LastChanceBeforeWriting();
      virtual void OnSetData();

      void Line1();
      void Line4();
      void Line5();
      void Line6();
      void Line8();
      void Line9();
      void Line10and11();
      void ArrayToFile(MfPackage* p, const CStr& a_desc);

      NeArealPar m_par;
    };

  }
}

#endif


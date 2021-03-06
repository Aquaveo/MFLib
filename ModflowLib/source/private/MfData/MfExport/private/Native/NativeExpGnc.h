//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NativeExpGnc_H
#define NativeExpGnc_H

class NativeExpGncT;
class Param;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpGnc : public NativePackExp
    {
      friend NativeExpGncT;
    public:
      NativeExpGnc();
      ~NativeExpGnc();
      virtual bool Export();

    private:
      NativeExpGnc(const NativeExpGnc& rhs);
      const NativeExpGnc& operator=(const NativeExpGnc& rhs);

      //virtual void LastChanceBeforeWriting();
      //virtual void OnSetData();

      void Line1();
      void Line4();
    }; // class NativeExpGnc

  }
}

#endif


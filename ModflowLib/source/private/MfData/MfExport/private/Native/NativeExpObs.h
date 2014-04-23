//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPOBS_H
#define NATIVEEXPOBS_H

class NativeExpObsT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpObs : public NativePackExp
    {
      friend NativeExpObsT;
    public:
      NativeExpObs();
      ~NativeExpObs();
      virtual bool Export();

    private:
      NativeExpObs(const NativeExpObs& rhs);
      const NativeExpObs& operator=(const NativeExpObs& rhs);

      void ExportObsPack();
      void ExportHob();
      void HobLine1();
      void HobLine2();
      void HobLines3to6();

      bool m_isMf2k;
    };

  }
}

#endif


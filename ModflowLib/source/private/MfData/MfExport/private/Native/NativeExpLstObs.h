//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPLSTOBS_H
#define NATIVEEXPLSTOBS_H

class NativeExpLstObsT;
class FlowObs;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpLstObs : public NativePackExp
    {
      friend NativeExpLstObsT;
    public:
      NativeExpLstObs();
      ~NativeExpLstObs();
      virtual bool Export();

    private:
      NativeExpLstObs(const NativeExpLstObs& rhs);
      const NativeExpLstObs& operator=(const NativeExpLstObs& rhs);

      void SaveObsVar();
      bool ObsExist(const char* const a_type);
      CStr Line1(const char* const a_type,
                 const char* const a_p,
                 const char* const a_f1);
      CStr Line2(const char* const a_p,
                 const char* const a_f1,
                 const char* const a_f2);
      void Lines3to5(const char* const a_type,
                     CStr a_desc[3]);

      void WriteChob();
      void WriteDrob();
      void WriteDtob();
      void WriteGbob();
      void WriteRvob();
      void WriteStob();

      FlowObs* m_f;
      bool m_isMf2k;
    };

  }
}

#endif


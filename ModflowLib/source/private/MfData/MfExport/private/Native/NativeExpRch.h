//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPRCH_H
#define NATIVEEXPRCH_H

class NativeExpRchT;
class Param;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/MfExport/private/Native/NeArealPar.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpRch : public NativePackExp
    {
      friend NativeExpRchT;
    public:
      NativeExpRch();
      ~NativeExpRch();
      virtual bool Export();

    private:
      NativeExpRch(const NativeExpRch& rhs);
      const NativeExpRch& operator=(const NativeExpRch& rhs);

      virtual void LastChanceBeforeWriting();
      virtual void OnSetData();

      void Line2();
      void Line5();
      void Line6();
      void Line8();

      NeArealPar m_par;
      bool m_usg, m_unstructured;

      void RewriteFileWithParameters();
    };

  }
}

#endif


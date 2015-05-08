//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPUZF_H
#define NATIVEEXPUZF_H

class NativeExpUzfT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpUzf : public NativePackExp
    {
      friend NativeExpUzfT;
    public:
      NativeExpUzf(bool a_h5);
      ~NativeExpUzf();
      virtual bool Export();

    private:
      NativeExpUzf(const NativeExpUzf& rhs);
      const NativeExpUzf& operator=(const NativeExpUzf& rhs);

      void Line1();
      void Lines2to8();
      void Lines9to16();

      bool m_h5;
      int m_iuzfopt, m_irunflg, m_ietflg, m_nuzgag;
    };

  }
}

#endif


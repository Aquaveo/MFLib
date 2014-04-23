//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPLGR_H
#define NATIVEEXPLGR_H

class NativeExpLgrT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  class MfPackage;
  namespace Export
  {
    class NativeExpLgr : public NativePackExp
    {
      friend NativeExpLgrT;
    public:
      NativeExpLgr();
      ~NativeExpLgr();
      virtual bool Export();

    private:
      NativeExpLgr(const NativeExpLgr& rhs);
      const NativeExpLgr& operator=(const NativeExpLgr& rhs);

      void Lgr_1();
      void Lgr_2();
      void WriteFile();
      virtual void   AddToStoredLinesDesc(const char* a_line,
                                          const char* a_desc);
      virtual void   OnSetData();

      MfPackage* m_lgr;
    };

  }
}

#endif


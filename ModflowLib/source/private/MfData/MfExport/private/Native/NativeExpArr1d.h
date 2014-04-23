//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPARR1D_H
#define NATIVEEXPARR1D_H

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpArr1d : public NativePackExp
    {
    public:
      NativeExpArr1d();
      ~NativeExpArr1d();
      virtual bool Export();

    private:
      NativeExpArr1d(const NativeExpArr1d& rhs);
      const NativeExpArr1d& operator=(const NativeExpArr1d& rhs);

      void WriteExternal(const int* JJ, const int* IPRN, const Real* ARR,
                         const Real* MULT);
      void WriteInternal(const int* JJ, const int* IPRN, const Real* ARR,
                         const Real* MULT);
      void WriteToStream(std::ostream& a_os, const int* JJ,const Real* ARR);
    };

  }
}

#endif


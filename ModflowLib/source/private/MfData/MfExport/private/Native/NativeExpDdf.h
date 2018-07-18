//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPDDF_H
#define NATIVEEXPDDF_H

#include <map>

class NativeExpDdfT;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpDdf : public NativePackExp
    {
      friend NativeExpDdfT;
    public:
      NativeExpDdf();
      ~NativeExpDdf();
      virtual bool Export();

    private:
      NativeExpDdf(const NativeExpDdf& rhs);
      const NativeExpDdf& operator=(const NativeExpDdf& rhs);
      virtual void   AddToStoredLinesDesc(const char* a_line,
                                          const char* a_desc);

    };

  }
}

#endif


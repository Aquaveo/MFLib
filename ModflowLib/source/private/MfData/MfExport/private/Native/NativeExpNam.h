//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPNAM_H
#define NATIVEEXPNAM_H

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpNam : public NativePackExp
    {
    public:
      NativeExpNam();
      ~NativeExpNam();
      virtual bool Export();

      void WriteFileStp();

    private:
      NativeExpNam(const NativeExpNam& rhs);
      const NativeExpNam& operator=(const NativeExpNam& rhs);

      bool ExportFileType(const CStr& a_type);
    };

  }
}

#endif


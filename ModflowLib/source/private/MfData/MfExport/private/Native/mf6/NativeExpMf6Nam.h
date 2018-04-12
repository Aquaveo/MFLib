//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6GWFMN_H
#define NATIVEEXPMF6GWFMN_H

#include <private\util\StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Nam
    {
    public:
      NativeExpMf6Nam(NativePackExp* a_);
      ~NativeExpMf6Nam();
      virtual bool Export();

    private:
      NativeExpMf6Nam(const NativeExpMf6Nam& rhs);
      const NativeExpMf6Nam& operator=(const NativeExpMf6Nam& rhs); 
      bool DoNewton();
      bool ValidPackage(const CStr& a_ftype);

      NativePackExp* m_pack;
    };

  }
}

#endif


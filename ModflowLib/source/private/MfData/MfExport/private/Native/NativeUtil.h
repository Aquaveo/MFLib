//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEUTIL_H
#define NATIVEUTIL_H

namespace MfData
{
  class MfGlobal;
  class MfPackage;

  namespace Export
  {
    class Mf2kNative;
    class NativePackExp;

    class NativeUtil
    {
    public:
      static NativePackExp *CreatePackExp(Mf2kNative* a_native,
                                          MfGlobal* a_global,
                                          MfPackage* a_package);
    private:
    };
  }
}

#endif


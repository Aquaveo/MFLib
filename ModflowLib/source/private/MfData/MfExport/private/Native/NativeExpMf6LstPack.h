//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6LSTPACK_H
#define NATIVEEXPMF6LSTPACK_H

#include <private\util\StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativeExpLstPack;
    class NativeExpMf6LstPack
    {
    public:
      NativeExpMf6LstPack(NativeExpLstPack* a_);
      ~NativeExpMf6LstPack();
      virtual bool Export();

    private:
      NativeExpMf6LstPack(const NativeExpMf6LstPack& rhs);
      const NativeExpMf6LstPack& operator=(const NativeExpMf6LstPack& rhs);

      CStr GetAuxLine();
      bool CbFieldExists();
      CStr GetMaxBoundLine();
      CStr GetStressPeriodLine(int itmp);

      NativeExpLstPack* m_pack;
    };

  }
}

#endif


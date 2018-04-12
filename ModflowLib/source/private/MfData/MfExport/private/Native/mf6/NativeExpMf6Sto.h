//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6STO_H
#define NATIVEEXPMF6STO_H

#include <private\util\StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Sto
    {
    public:
      NativeExpMf6Sto(NativePackExp* a_);
      ~NativeExpMf6Sto();
      virtual bool Export();

    private:
      NativeExpMf6Sto(const NativeExpMf6Sto& rhs);
      const NativeExpMf6Sto& operator=(const NativeExpMf6Sto& rhs);

      bool SaveFlows();
      CStr GetIconvertLine(int a_numLay);
   
      NativePackExp* m_pack;
    };

  }
}

#endif


//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6NPF_H
#define NATIVEEXPMF6NPF_H

#include <private/util/StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Npf
    {
    public:
      NativeExpMf6Npf(NativePackExp* a_);
      ~NativeExpMf6Npf();
      virtual bool Export();

    private:
      NativeExpMf6Npf(const NativeExpMf6Npf& rhs);
      const NativeExpMf6Npf& operator=(const NativeExpMf6Npf& rhs);

      bool SaveFlows();
      bool WettingActive(int a_nLay);
      CStr GetWetOptionsLine();
      CStr GetAlternativeCellAveragingLine();
      CStr GetThickStrtLine();
      CStr GetVariableCvLine();
      CStr GetPerchedLine();

      void GenerateK22K33();

      NativePackExp* m_pack;
    };

  }
}

#endif


//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPBCF_H
#define NATIVEEXPBCF_H

class NativeExpBcfT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpBcf : public NativePackExp
    {
      friend NativeExpBcfT;
    public:
      NativeExpBcf();
      ~NativeExpBcf();
      virtual bool Export();

    private:
      NativeExpBcf(const NativeExpBcf& rhs);
      const NativeExpBcf& operator=(const NativeExpBcf& rhs);

      virtual void OnSetData();
      CStr Desc(int a_line, int a_lay=-1);
      CStr Line1();
      CStr Line2();
      CStr Line3();
      void Line4to9(int a_line, int a_lay);
      bool CanWriteLine(int a_line, int a_lay);
      CStr GetArrayName(int a_line);

      int              m_nLay;
      std::vector<int> m_lineCnt;
      bool             m_internalArrays;
    };

  }
}

#endif


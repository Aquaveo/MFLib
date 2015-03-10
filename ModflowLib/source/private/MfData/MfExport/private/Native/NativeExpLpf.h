//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPLPF_H
#define NATIVEEXPLPF_H

class NativeExpLpfT;
class ParamList;
class Param;
class PClust;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpLpf : public NativePackExp
    {
      friend NativeExpLpfT;
    public:
      NativeExpLpf();
      ~NativeExpLpf();
      virtual bool Export();

    private:
      NativeExpLpf(const NativeExpLpf& rhs);
      const NativeExpLpf& operator=(const NativeExpLpf& rhs);

      virtual void OnSetData();

      CStr Desc(int a_line);
      CStr Desc(int a_line, int a_lay);
      CStr Line1();
      CStr Line2to6(int a_line);
      bool CanWriteLine(int a_line, int a_lay=-1);
      CStr Line7();
      void WriteLpfParams();
      void WriteLpfPilotPar(ParamList* list, Param* p);
      CStr Line8(Param* a_p);
      CStr Line9(PClust* a_clust);
      void Line10to16(int a_line, int a_lay);
      void Line12(int a_lay);
      void Line10Usg();

      int m_nLay;
      int m_nPar;
      int m_ikcflag;
      bool m_usg, m_unstructured, m_anyChaniNotOne, m_stacked;
    };

  }
}

#endif


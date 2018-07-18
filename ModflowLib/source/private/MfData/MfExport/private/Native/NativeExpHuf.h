//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NativeExpHuf_H
#define NativeExpHuf_H

class NativeExpHufT;
class Param;
class ParamList;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpHuf : public NativePackExp
    {
      friend NativeExpHufT;
    public:
      NativeExpHuf();
      ~NativeExpHuf();
      virtual bool Export();

    private:
      NativeExpHuf(const NativeExpHuf& rhs);
      const NativeExpHuf& operator=(const NativeExpHuf& rhs);

      CStr Line1(const std::vector<Param>& a_par);
      CStr Desc1();
      CStr Line2();
      CStr Desc2();
      CStr Line3();
      CStr Desc3();
      CStr Line4();
      CStr Desc4();
      void Line5();
      CStr Desc5();
      void Line6to8();
      std::vector<CStr> Desc6to8();
      std::vector<CStr> Line9();
      std::vector<CStr> Desc9();
      bool Line9IsAll();
      CStr Line10(Param& a_par);
      CStr Desc10();
      std::vector<CStr> Line11(Param& a_par);
      std::vector<CStr> Desc11(Param& a_par);
      void GetParams(std::vector<Param>& a_par);
      double KeyFromMultArray(const CStr& a_name);
      void HandleParameter(double a_key, ParamList* a_list, Param& a_p,
                           std::vector<Param>& a_par);
      CStr ArrayNameFromParType(const CStr& a_type);
      void ExportPilotPar(double a_key, const CStr& a_type);

      bool m_useWet;
      bool m_isGmsHuf;
    };

  }
}

#endif


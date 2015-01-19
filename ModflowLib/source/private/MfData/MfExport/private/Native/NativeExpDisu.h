//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPDISU_H
#define NATIVEEXPDISU_H

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
class NativeExpDisuT;
namespace MfData
{
  namespace Export
  {
    class NativeExpDisu : public NativePackExp
    {
      friend NativeExpDisuT;
    public:
      NativeExpDisu();
      virtual ~NativeExpDisu();
      virtual bool Export();
    private:
      NativeExpDisu(const NativeExpDisu& rhs);
      const NativeExpDisu& operator=(const NativeExpDisu& rhs);

      virtual void OnSetData();

      CStr Line1();
      CStr Line2();
      void Line3();
      void Line4();
      void Line5();
      void Line6();
      void Line7();
      void Line8();
      void Line9();
      void Line10a();
      void Line10b();
      void Line11();
      void Line12();
      std::vector<CStr> Line13();

      void InitDescriptionMap ();
      CStr Desc(const CStr& a_line);
      void AddArrayLines (const CStr& a_name, const CStr& a_desc);

      int m_nLay, m_nSp;
      std::vector<int> m_layCbd;
      std::map<CStr, CStr> m_mapDesc;
    };

  }
}

#endif


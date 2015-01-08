//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPHFB_H
#define NATIVEEXPHFB_H

class NativeExpHfbT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpHfb : public NativePackExp
    {
      friend NativeExpHfbT;
    public:
      NativeExpHfb();
      ~NativeExpHfb();
      virtual bool Export();

    private:
      NativeExpHfb(const NativeExpHfb& rhs);
      const NativeExpHfb& operator=(const NativeExpHfb& rhs);

      int  NumPar();
      CStr Line1();
      CStr Desc(int a_line);
      std::vector<CStr> DescVec(int a_line, int a_num);
      void Lines2and3();
      std::vector<CStr> Line4();
      std::vector<CStr> Line6();
      CStr KijijFactToStr (const Real* a_, int a_idx, int a_rowSize);

      bool m_usg;
      int  m_nI, m_nJ;
      int  m_VAL_ROW_SIZE;
      int  m_HFB_ROW_SIZE;
    };

  }
}

#endif


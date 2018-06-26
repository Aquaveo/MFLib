//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPOC_H
#define NATIVEEXPOC_H

class NativeExpOcT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpOc : public NativePackExp
    {
      friend NativeExpOcT;
    public:
      NativeExpOc();
      ~NativeExpOc();
      virtual bool Export();

    private:
      NativeExpOc(const NativeExpOc& rhs);
      const NativeExpOc& operator=(const NativeExpOc& rhs);

      virtual void LastChanceBeforeWriting();

      CStr              Options();
      std::vector<CStr> Line1();
      std::vector<CStr> Desc1();
      CStr              Line2();
      CStr              Desc2();
      std::vector<CStr> Line3();
      std::vector<CStr> Desc3();

      CStr m_usgTransportLine2;
      bool m_usgTransport, m_usgTransportAts;
    };

  }
}

#endif


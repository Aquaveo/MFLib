//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPLSTPAR_H
#define NATIVEEXPLSTPAR_H

class NativeExpLstParT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpLstPar : public NativePackExp
    {
      friend NativeExpLstParT;
    public:
      NativeExpLstPar(bool a_h5);
      ~NativeExpLstPar();
      virtual bool Export();

    private:
      NativeExpLstPar(const NativeExpLstPar& rhs);
      const NativeExpLstPar& operator=(const NativeExpLstPar& rhs);

      bool m_h5;
    };

  }
}

#endif


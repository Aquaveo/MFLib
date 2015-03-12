//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef ARRTOH5_H
#define ARRTOH5_H
#include <private\util\util.h>


namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class ArrToh5
    {
    public:
      ArrToh5(NativePackExp* a_);
      ~ArrToh5();

      CStr WriteData();

    private:
      CStr H5Filename();
      CStr H5Path();
      int  GetDataType();
      int  GetNumDim();
      int  GetNumValsToWrite();

      NativePackExp* m_pack;
    };
  } // namespace Export
} // namespace MfData
#endif

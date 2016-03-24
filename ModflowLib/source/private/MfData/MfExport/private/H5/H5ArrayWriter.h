//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5ARRAYWRITER_H
#define H5ARRAYWRITER_H
#include <private\util\util.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class H5ArrayWriter
    {
    public:
      H5ArrayWriter (NativePackExp* a_);
      ~H5ArrayWriter ();

      CStr WriteData ();
      bool ForceToH5File ();

      void Extend3dDblArray (
        const char* a_path,
        int a_sp,
        int a_nCells);
      void Extend2dDblArray (
        const char* a_path,
        int a_sp,
        int a_nCells);
      static void WriteDataSetWithZeros (
        const char* a_file,
        const char* a_path,
        hsize_t dim[3],
        hsize_t start[3]);

    private:
      class impl;
      impl* m_p;
    };
  } // namespace Export
} // namespace MfData
#endif

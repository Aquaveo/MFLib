//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5USELASTWRITER_H
#define H5USELASTWRITER_H
#include <private\util\util.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class H5UseLastWriter
    {
    public:
      H5UseLastWriter(NativePackExp* a_);
      ~H5UseLastWriter();

      void WriteData(std::vector<int>& a_);
      void WriteEtsNetSeg(int a_);

      void CheckArealFromUseLast();

    private:
      class impl;
      impl* m_p;

      CStr GetH5DataPath();

      NativePackExp* m_pack;
    };
  } // namespace Export
} // namespace MfData
#endif

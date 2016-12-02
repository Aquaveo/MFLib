//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SQARRAYWRITER_H
#define SQARRAYWRITER_H
#include <private\util\util.h>

namespace MfData
{
  namespace Export
  {
    class NativeExpLstPack;
    class SqArrayWriter
    {
    public:
      SqArrayWriter(NativeExpLstPack* a_);
      ~SqArrayWriter();

      void EndWriteFile();
      void AddSqComment();

    private:
      class impl;
      impl *m_p;
      NativeExpLstPack* m_pack;
    };
  } // namespace Export
} // namespace MfData
#endif

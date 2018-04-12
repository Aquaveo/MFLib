//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6DISU_H
#define NATIVEEXPMF6DISU_H

#include <vector>
#include <private\util\util.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Disu
    {
    public:
      NativeExpMf6Disu(NativePackExp* a_);
      ~NativeExpMf6Disu();
      virtual bool Export();

    private:
      NativeExpMf6Disu(const NativeExpMf6Disu& rhs);
      const NativeExpMf6Disu& operator=(const NativeExpMf6Disu& rhs);

      void WriteOptions(std::vector<CStr>& a_lines);
      void WriteDimensions(std::vector<CStr>& a_lines);
      void WriteGridData(std::vector<CStr>& a_lines);
      void WriteConnections(std::vector<CStr>& a_lines);

      NativePackExp* m_pack;
    };

  }
}

#endif


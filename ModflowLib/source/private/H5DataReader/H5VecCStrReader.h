//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5VECCSTRREADER_H
#define H5VECCSTRREADER_H

#include <private\util\util.h>

class H5VecCStrReader
{
public:
  H5VecCStrReader(const CStr &a_file,
                  const CStr &a_path);

  bool FillInStrings(std::vector<CStr> &a_) const;

private:
  H5VecCStrReader(const H5VecCStrReader &rhs);
  const H5VecCStrReader& operator=(const H5VecCStrReader &rhs);

  CStr m_file, m_path;
};
#endif

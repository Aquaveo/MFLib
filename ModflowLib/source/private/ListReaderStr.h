//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef LISTREADERSTR_H
#define LISTREADERSTR_H

#include<private/ListReader.h>

class ListReaderStr : public ListReader
{
public:
  ListReaderStr(ListReaderSetUp &a_) : ListReader(a_) {}

  bool GetDataStr(Real *a_STRM,
                  int *a_ISTRM,
                  int *a_ITRBAR,
                  int *a_IDIVAR,
                  int a_NSS,
                  int a_NTRIB) const;
private:
  ListReaderStr(const ListReaderStr &rhs);
  const ListReaderStr& operator=(const ListReaderStr &rhs);
};
#endif


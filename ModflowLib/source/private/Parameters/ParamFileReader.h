//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PARAMFILEREADER_H
#define PARAMFILEREADER_H

#include <private/util/util.h>

class ParamList;
class ParamFileReaderT;

class ParamFileReader
{
  friend ParamFileReaderT;
public:
  ParamFileReader(const char *a_fName);
  ~ParamFileReader();

  bool FillInListFromFile(ParamList *a_);

private:
  enum { BEGPAR=1, NAME, TYPE, KEY, VALUE, BEGPILOT, INTERPLOG, SCATINDEX,
         ARRAYTYPE, ENDPAR, BEGTAB, BSCAL, LOGXFORM, TIED,
         NITEMS };

  int FileCardToInt(const CStr &a_card);

  ParamFileReader(const ParamFileReader &rhs);
  const ParamFileReader& operator=(const ParamFileReader &rhs);

  CStr m_fName;
  FILE *m_fp;
};

#endif


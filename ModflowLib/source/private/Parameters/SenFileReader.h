//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SENFILEREADER_H
#define SENFILEREADER_H

#include <private\util\util.h>

class ParamList;

class SenFileReader
{
public:
  SenFileReader(const char *a_fName);

  bool FillInStartingVals(ParamList *a_,
                          bool a_isPVAL) const;

private:
  SenFileReader(const SenFileReader &rhs);
  const SenFileReader& operator=(const SenFileReader &rhs);

  CStr m_fName;
  FILE *m_fp;
};

#endif

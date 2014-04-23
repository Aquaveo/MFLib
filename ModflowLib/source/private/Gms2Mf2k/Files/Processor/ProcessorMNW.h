//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#pragma once

#include <private/Gms2Mf2k/Files/Processor/Processor.h>

class ProcessorMNWT;

class ProcessorMNW : public Processor
{
friend ProcessorMNWT;
public:
  ProcessorMNW(const char * const a_inputFile,
               const char * const a_outputFile,
               const int& a_nRow,
               const int& a_nCol);
  virtual ~ProcessorMNW();

private:
  ProcessorMNW(const ProcessorMNW &rhs);
  const ProcessorMNW& operator=(const ProcessorMNW &rhs);

  virtual bool DoConvertFile();

  class impl;
  impl *m_p;
};

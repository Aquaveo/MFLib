//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PROCESSORSFR2_H
#define PROCESSORSFR2_H

#include <private/Gms2Mf2k/Files/Processor/Processor.h>

class ProcessorSFR2T;

class ProcessorSFR2 : public Processor
{
friend ProcessorSFR2T;
public:
  ProcessorSFR2(const char * const a_inputFile,
                  const char * const a_outputFile,
                  const int& a_nRow,
                  const int& a_nCol,
                  bool a_unstructured);
  ~ProcessorSFR2();

private:
  ProcessorSFR2(const ProcessorSFR2 &rhs);
  const ProcessorSFR2& operator=(const ProcessorSFR2 &rhs);

  virtual bool DoConvertFile();

  class impl;
  impl *m_p;
};

#endif

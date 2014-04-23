//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PROCESSORSTREAM_H
#define PROCESSORSTREAM_H

#include <private/Gms2Mf2k/Files/Processor/Processor.h>

class ProcessorStreamT;

class ProcessorStream : public Processor
{
friend ProcessorStreamT;
public:
  ProcessorStream(const char * const a_inputFile,
                  const char * const a_outputFile,
                  const int& a_nRow,
                  const int& a_nCol,
                  bool a_unstructured);
  ~ProcessorStream();

private:
  ProcessorStream(const ProcessorStream &rhs);
  const ProcessorStream& operator=(const ProcessorStream &rhs);

  virtual bool DoConvertFile();

  class impl;
  impl *m_p;
};

#endif

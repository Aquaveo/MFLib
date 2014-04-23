//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PROCESSOR_H
#define PROCESSOR_H

class ProcessorT;

class Processor
{
friend ProcessorT;
public:
  Processor(const char* const a_inputFile,
            const char* const a_outputFile,
            const int& a_nRow,
            const int& a_nCol,
            bool a_unstructured);
  virtual ~Processor();

  bool ConvertFile();

protected:
  const char * const InputFile() const;
  const char * const OutputFile() const;
  int NumRow() const;
  int NumCol() const;
  bool Unstructured() const;

private:
  Processor(const Processor &rhs);
  const Processor &operator=(const Processor &rhs);

  virtual bool DoConvertFile();

  class impl;
  impl *m_p;
};
#endif

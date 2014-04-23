//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PROCESSORMNW2_H
#define PROCESSORMNW2_H
//----- Included files ---------------------------------------------------------
#include <private/Gms2Mf2k/Files/Processor/Processor.h>

//----- Namespace declaration --------------------------------------------------

//----- Constants / Enumerations -----------------------------------------------

//----- Forward declarations ---------------------------------------------------

//----- Structs / Classes ------------------------------------------------------
class ProcessorMNW2 : public Processor
{
public:
  ProcessorMNW2(const char * const a_inputFile,
               const char * const a_outputFile,
               const int& a_nRow,
               const int& a_nCol);
  virtual ~ProcessorMNW2();

private:
  ProcessorMNW2(const ProcessorMNW2 &rhs);
  const ProcessorMNW2& operator=(const ProcessorMNW2 &rhs);

  virtual bool DoConvertFile();

  class impl;
  impl *m_p;
};

//----- Function prototypes ----------------------------------------------------


#endif

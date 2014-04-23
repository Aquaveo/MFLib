//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PROCESSORUTIL_H
#define PROCESSORUTIL_H

class Processor;

class ProcessorUtil
{
public:
  static Processor *CreateProcessor(const char* const a_inFile,
                                    const char* const a_outFile,
                                    const int& a_nRow,
                                    const int& a_nCol,
                                    bool a_unstructured);
};
#endif

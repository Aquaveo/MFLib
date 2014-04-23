//------------------------------------------------------------------------------
// FILE      ProcessorUtil.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/Gms2Mf2k/Files/Processor/ProcessorUtil.h>

#include <set>

#include <private/Gms2Mf2k/Files/Processor/Processor.h>
#include <private/Gms2Mf2k/Files/Processor/ProcessorMNW.h>
#include <private/Gms2Mf2k/Files/Processor/ProcessorMNW2.h>
#include <private/Gms2Mf2k/Files/Processor/ProcessorSFR2.h>
#include <private/Gms2Mf2k/Files/Processor/ProcessorStream.h>
#include <private/util/util.h>

///////////////////////////////////////////////////////////////////////////////
/// \brief Factory to create the right type of file processor
///////////////////////////////////////////////////////////////////////////////
Processor* ProcessorUtil::CreateProcessor (const char* const a_inFile,
                                           const char* const a_outFile,
                                           const int& a_nRow,
                                           const int& a_nCol,
                                           bool a_unstructured)
{
  Processor *p(0);

  CStr in(a_inFile);
  CStr extension;
  util::StripAllButExtension(a_inFile, extension);

  std::set<CStr> filesDoNotConvert;
  filesDoNotConvert.insert("glo");
  filesDoNotConvert.insert("out");
  filesDoNotConvert.insert("hed");
  filesDoNotConvert.insert("drw");
  filesDoNotConvert.insert("ccf");

  if (filesDoNotConvert.find(extension) != filesDoNotConvert.end())
  {
    // do nothing
  }
  else if (extension == "str")
  {
    p = new ProcessorStream(a_inFile, a_outFile, a_nRow, a_nCol, a_unstructured);
  }
  else if (extension == "sfr2")
  {
    p = new ProcessorSFR2(a_inFile, a_outFile, a_nRow, a_nCol, a_unstructured);
  }
  else if (extension == "mnw")
  {
    p = new ProcessorMNW(a_inFile, a_outFile, a_nRow, a_nCol);
  }
  else if (extension == "mnw2")
  {
    p = new ProcessorMNW2(a_inFile, a_outFile, a_nRow, a_nCol);
  }
  else // the default file processor
  {
    p = new Processor(a_inFile, a_outFile, a_nRow, a_nCol, a_unstructured);
  }

  return (p);
} // ProcessorUtil::CreateProcessor

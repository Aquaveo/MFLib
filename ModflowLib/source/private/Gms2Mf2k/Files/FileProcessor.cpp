//------------------------------------------------------------------------------
// FILE      FileProcessor.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/Gms2Mf2k/Files/FileProcessor.h>

#include <private/util/util.h>
#include <private/Gms2Mf2k/Files/DisFileReader.h>
#include <private/Gms2Mf2k/Files/Processor/Processor.h>
#include <private/Gms2Mf2k/Files/Processor/ProcessorUtil.h>

//lint -esym(1712,impl)
class FileProcessor::impl
{
public:
  impl(const char * const a_outPath,
       const DisFileReader &a_);
  bool ProcessFile(const char * const a_);

private:
  CStr m_outPath;
  bool m_noGrid;
  int  m_nRow, m_nCol;
  bool m_unstructured;
};

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
FileProcessor::FileProcessor(const char * const a_outPath,
                             const DisFileReader &a_) :
m_p(new FileProcessor::impl(a_outPath, a_))
{
}
///////////////////////////////////////////////////////////////////////////////
/// \brief Destructor
///////////////////////////////////////////////////////////////////////////////
FileProcessor::~FileProcessor ()
{
  try
  {
    if (m_p)
      delete(m_p);
  }
  catch (...) // scott meyers item 8
  {
  }
}
///////////////////////////////////////////////////////////////////////////////
/// \brief Converts the file
///////////////////////////////////////////////////////////////////////////////
bool FileProcessor::ProcessFile (const char * const a_)
{
  return(m_p->ProcessFile(a_));
} // FileProcessor::ProcessFile

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
FileProcessor::impl::impl (const char * const a_outPath,
                           const DisFileReader &a_)
: m_outPath(a_outPath)
, m_noGrid(false)
, m_nRow(0)
, m_nCol(0)
, m_unstructured(a_.Unstructured())
{
  if (!a_.GetNumCol(&m_nCol) ||
      !a_.GetNumRow(&m_nRow))
    m_noGrid = true;
} // FileProcessor::impl::impl
///////////////////////////////////////////////////////////////////////////////
/// \brief Converts the file
///////////////////////////////////////////////////////////////////////////////
bool FileProcessor::impl::ProcessFile (const char * const a_)
{
  if (m_noGrid)
  {
    ErrorStack::Get().PutError("No grid defined. Make sure dis file exists.");
    return false;
  }

  bool rval(true);
  CStr fileName, outFile;
  util::StripPathFromFilename(a_, fileName);

  outFile = m_outPath + "\\" + fileName;

  Processor *p(0);
  p = ProcessorUtil::CreateProcessor(a_,
                                     outFile,
                                     m_nRow,
                                     m_nCol,
                                     m_unstructured);
  if (p)
  {
    rval = p->ConvertFile();
    delete(p);
  }
  return(rval);
} // FileProcessor::impl::ProcessFile

#ifdef CXX_TEST
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#include <private/Gms2Mf2k/Files/FileProcessor.t.h>

//------------------------------------------------------------------------------
void FileProcessorT::testCreateClass ()
{
  DisFileReader d("stuff");
  FileProcessor *f = new FileProcessor("stuff", d);
  TS_ASSERT(f);
  if (f)
    delete(f);
}

#endif



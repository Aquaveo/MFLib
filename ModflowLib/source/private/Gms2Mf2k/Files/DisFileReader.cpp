//------------------------------------------------------------------------------
// FILE      DisFileReader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/Gms2Mf2k/Files/DisFileReader.h>

#include <private/util/EReadAsciiFile.h>

//lint -esym(1712,impl)
class DisFileReader::impl
{
public:
  explicit impl(const char * const a_, bool a_unstructured);

  bool ReadFile();
  bool ReadDisFile();
  bool ReadDisuFile();
  int  Row() const {return m_nRow;}
  void Row(const int &a_) {m_nRow = a_;}
  int  Col() const {return m_nCol;}
  void Col(const int &a_) {m_nCol = a_;}
  bool Unstructured() {return m_unstructured;}

private:
  CStr m_fileName;
  bool m_unstructured;
  int m_nRow, m_nCol;
};

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
DisFileReader::DisFileReader (const char * const a_, bool a_unstructured) :
m_p(new DisFileReader::impl(a_, a_unstructured))
{
} // DisFileReader::DisFileReader
///////////////////////////////////////////////////////////////////////////////
/// \brief Destructor
///////////////////////////////////////////////////////////////////////////////
DisFileReader::~DisFileReader ()
{
  try
  {
    if (m_p)
      delete(m_p);
  }
  catch (...)
  {
  }
} // DisFileReader::~DisFileReader
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads the number of rows and columns from the DIS file.
///////////////////////////////////////////////////////////////////////////////
bool DisFileReader::ReadFile ()
{
  return (m_p->ReadFile());
} // DisFileReader::ReadFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the number of rows in the grid
///////////////////////////////////////////////////////////////////////////////
bool DisFileReader::GetNumRow (int* a_nRow) const
{
  if (!a_nRow)
    return false;
  if (m_p->Row() < 0)
    return false;
  *a_nRow = m_p->Row();
  return true;
} // DisFileReader::GetNumRow
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the number of columns in the grid
///////////////////////////////////////////////////////////////////////////////
bool DisFileReader::GetNumCol (int* a_nCol) const
{
  if (!a_nCol)
    return false;
  if (m_p->Col()  < 0)
    return false;
  *a_nCol = m_p->Col();
  return true;
} // DisFileReader::GetNumCol
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the number of columns in the grid
///////////////////////////////////////////////////////////////////////////////
bool DisFileReader::Unstructured () const
{
  return m_p->Unstructured();
} // DisFileReader::Unstructured

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
DisFileReader::impl::impl (const char * const a_, bool a_unstructured)
: m_fileName(a_)
, m_unstructured(a_unstructured)
, m_nRow(-1)
, m_nCol(-1)
{
} // DisFileReader::impl::impl
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads the dis file to set the number of rows and columns
///////////////////////////////////////////////////////////////////////////////
bool DisFileReader::impl::ReadFile ()
{
  if (m_unstructured)
    return ReadDisuFile();
  else
    return ReadDisFile();
} // DisFileReader::impl::ReadFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads the dis file to set the number of rows and columns
///////////////////////////////////////////////////////////////////////////////
bool DisFileReader::impl::ReadDisFile ()
{
  EReadAsciiFile fp(m_fileName);
  if (!fp.OpenFile())
    return false;

  try
  {
    //lint --e{534} I am catching exceptions
    fp.UseExceptions();
    CStr line;
    bool done(false);
    while (fp.GetLine(&line) && !done)
    {
      if (line.IsEmpty())
        continue;
      if (line.at(0) == '#')
        continue;

      // read the nlay
      int tmpInt;
      fp.ReadData(tmpInt);
      // read the nrow
      fp.ReadData(m_nRow);
      // read the ncol
      fp.ReadData(m_nCol);
      done = true;
    }
  }
  catch (ioexception&)
  {
    m_nRow = m_nCol = -1;
    ErrorStack::Get().PutError("Error in the format of the DIS file. Aborting.");
    return false;
  }
  return true;
} // DisFileReader::impl::ReadDisFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads the disu file to set the number of rows (1) and columns (nodes)
///////////////////////////////////////////////////////////////////////////////
bool DisFileReader::impl::ReadDisuFile ()
{
  EReadAsciiFile fp(m_fileName);
  if (!fp.OpenFile())
    return false;

  try
  {
    //lint --e{534} I am catching exceptions
    fp.UseExceptions();
    CStr line;
    bool done(false);
    while (fp.GetLine(&line) && !done)
    {
      if (line.IsEmpty())
        continue;
      if (line.at(0) == '#')
        continue;

      // read the nnodes
      fp.ReadData(m_nCol);
      m_nRow = 1;
      done = true;
    }
  }
  catch (ioexception&)
  {
    m_nRow = m_nCol = -1;
    ErrorStack::Get().PutError("Error in the format of the DISU file. Aborting.");
    return false;
  }
  return true;
} // DisFileReader::impl::ReadDisuFile

#ifdef CXX_TEST
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#include <private/Gms2Mf2k/Files/DisFileReader.t.h>

//------------------------------------------------------------------------------
DisFileReaderT::DisFileReaderT ()
: m_p(NULL)
, m_p1(NULL)
, m_p2(NULL)
{
}
//------------------------------------------------------------------------------
void DisFileReaderT::setUp ()
{
  CStr str, str1, str2;
  util::GetTestFilesDirectory(str);
  str2 = str1 = str;
  str += "\\mfInputs1\\run2.dis";
  str1 += "\\mfInputs1\\run2_WrongFormat.dis";
  str2 += "\\mfInputs1\\run2.disu";
  m_p = new DisFileReader(str);
  m_p1 = new DisFileReader(str1);
  m_p2 = new DisFileReader(str2, true);
}
//------------------------------------------------------------------------------
void DisFileReaderT::tearDown ()
{
  delete m_p;
  delete m_p1;
  delete m_p2;
  m_p = m_p1 = m_p2 = NULL;
}
//------------------------------------------------------------------------------
void DisFileReaderT::testCreateClass ()
{
  DisFileReader *d = new DisFileReader("stuff");
  TS_ASSERT(d);
  if (d)
    delete(d);
}
//------------------------------------------------------------------------------
void DisFileReaderT::testReadDisFile ()
{
  TS_ASSERT(m_p->ReadFile());
  TS_ASSERT(!m_p1->ReadFile());
}
//------------------------------------------------------------------------------
void DisFileReaderT::testReadDisuFile ()
{
  TS_ASSERT(m_p2->ReadFile());
  int row, col;
  TS_ASSERT(m_p2->GetNumRow(&row));
  TS_ASSERT(m_p2->GetNumCol(&col));
  TS_ASSERT_EQUALS(row, 1);
  TS_ASSERT_EQUALS(col, 53422);
}
//------------------------------------------------------------------------------
void DisFileReaderT::testGetNumRow ()
{
  int *rptr(0);
  int row;
  TS_ASSERT(!m_p->GetNumRow(rptr));
  TS_ASSERT(!m_p->GetNumRow(&row));
  TS_ASSERT(m_p->ReadFile());
  TS_ASSERT(m_p->GetNumRow(&row));
  TS_ASSERT_EQUALS(row, 300);
}
//------------------------------------------------------------------------------
void DisFileReaderT::testGetNumCol ()
{
  int *cptr(0);
  int col;
  TS_ASSERT(!m_p->GetNumCol(cptr));
  TS_ASSERT(!m_p->GetNumCol(&col));
  TS_ASSERT(m_p->ReadFile());
  TS_ASSERT(m_p->GetNumCol(&col));
  TS_ASSERT_EQUALS(col, 125);
}

#endif

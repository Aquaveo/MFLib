//------------------------------------------------------------------------------
// FILE      EReadAsciiFile.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private/util/EReadAsciiFile.h>

#define WHITESPACE " ,\t\r\n"
#define ENDLINE "\r\n"

//------------------------------------------------------------------------------
// CLASS EReadAsciiFile
//------------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::EReadAsciiFile
//------------------------------------------------------------------------------
EReadAsciiFile::EReadAsciiFile (const char * a_filename/*NULL*/) :
  m_strDelimiters(WHITESPACE)
{
  if (a_filename != NULL && a_filename[0] != '\0') {
    m_strFilename = a_filename;
  }

    // initialize these flags
  m_isunicode = FALSE;
  m_useexceptions = FALSE;
  m_linecount = 0;
  m_filesize = 0.0;
  m_linesize = 0;
  m_fp = NULL;

    // allocate default buffer
  m_bufsize = 512;
  m_buffer = new char[512];
  m_buffer[0] = '\0';
  m_ch = NULL;
  m_chDelimiters = m_strDelimiters.GetBuffer(m_strDelimiters.GetLength()+1);
} // EReadAsciiFile::EReadAsciiFile
//------------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::~EReadAsciiFile
//------------------------------------------------------------------------------
EReadAsciiFile::~EReadAsciiFile ()
{
    // close file if need be
  CloseFile();

  if (m_chDelimiters != NULL) {
    m_strDelimiters.ReleaseBuffer(int(strlen(m_chDelimiters) + 1));
  }

  if (m_buffer != NULL) {
    delete [] m_buffer;
    m_buffer = NULL;
  }
} // EReadAsciiFile::~EReadAsciiFile
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::Attach
// PURPOSE   Attaches an open file pointer for use with this class
// -----------------------------------------------------------------------------
void EReadAsciiFile::Attach (FILE *a_fp)
{
    // close previous file if it was open
  CloseFile();
    // blank out filename since we don't know it.
  m_strFilename = "";
    // store the file pointer
  m_fp = a_fp;
} // EReadAsciiFile::Attach
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::Detach
// PURPOSE   detaches the file pointer without closing the file
// NOTES     the previously-assigned file pointer is returned
// -----------------------------------------------------------------------------
FILE *EReadAsciiFile::Detach ()
{
  FILE *fp = m_fp;
    // detaches open file without closing it
  m_fp = NULL;

  return fp;
} // EReadAsciiFile::Detach
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::SetFilename
// NOTES     returns TRUE on success, FALSE on failure
// -----------------------------------------------------------------------------
bool EReadAsciiFile::SetFilename (const char * a_filename, bool a_opennow)
{
   if (a_filename == NULL)
     return false;

    // close previous file if it was open
  CloseFile();

    // set new file; open if needed
  m_strFilename = a_filename;
  if (a_opennow) {
    return OpenFile();
  }
  return TRUE;
} // EReadAsciiFile::SetFilename
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::OpenFile
// PURPOSE   opens the file, sets some flags
// NOTES     returns TRUE on success, FALSE on failure
// -----------------------------------------------------------------------------
bool EReadAsciiFile::OpenFile ()
{
  if (m_strFilename.IsEmpty())
    return false;
  if (m_fp != NULL)
    return true;

    // open file
  m_fp = fopen(m_strFilename, "r");
  if (m_fp == NULL) {
    return FALSE;
  }

    // read first line, check file type
  m_isunicode = UC_ASCII;
  GetLine();
    // check for UTF8 format
  if (m_line.GetLength() > 2) {
    if (m_line.GetAt(0) == 0xef && m_line.GetAt(1) == 0xbb &&
             m_line.GetAt(2) == 0xbf) {
      m_isunicode = UC_UTF8;
    }
  }
    // check for UNICODE formats
  if (m_line.GetLength() > 1) {
    if (m_line.GetAt(0) == 0xff && m_line.GetAt(1) == 0xfe) {
      m_isunicode = UC_SMALLENDIAN;
    }
    else if (m_line.GetAt(0) == 0xfe && m_line.GetAt(1) == 0xff) {
      m_isunicode = UC_SMALLENDIAN;
    }
  }

    // store file size (only works on files of size ((2^31)-1)
  fseek(m_fp, 0, SEEK_END);
  m_filesize = (double)ftell(m_fp);

    // reset file to beginning of first line
  RewindFile();
  return TRUE;
} // EReadAsciiFile::OpenFile
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::RewindFile
// PURPOSE   rewinds file to the beginning
// -----------------------------------------------------------------------------
void EReadAsciiFile::RewindFile ()
{
  if (m_fp != NULL) {
      // release buffer if it was open
    ReleaseBuffer();
  
    rewind(m_fp);
  
    m_linecount = 0;
  }
} // EReadAsciiFile::RewindFile
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::CloseFile
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
void EReadAsciiFile::CloseFile ()
{
    // release buffer if it was open
  ReleaseBuffer();

  if (m_fp != NULL) {
    fclose(m_fp);
    m_fp = NULL;
  }

  m_linecount = 0;
  m_filesize = 0.0;
} // EReadAsciiFile::CloseFile
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::UseExceptions
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::UseExceptions (bool a_useexceptions/* =TRUE*/)
{
  bool old = m_useexceptions;

  m_useexceptions = a_useexceptions;

  return old;
} // EReadAsciiFile::UseExceptions
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::SetDelimiters
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
CStr EReadAsciiFile::SetDelimiters (const char * a_delimiters)
{
  m_strDelimiters.ReleaseBuffer(int(strlen(m_chDelimiters) + 1));

  CStr old(m_strDelimiters);
  
  m_strDelimiters = a_delimiters;

  m_chDelimiters = m_strDelimiters.GetBuffer(m_strDelimiters.GetLength()+1);
  
  return old;
} // EReadAsciiFile::SetDelimiters
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::SetBufferSize
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
void EReadAsciiFile::SetBufferSize (int a_bufsize)
{
  if (a_bufsize > 0) {
      // allocate new buffer
    char *newbuf = new char[a_bufsize];
    newbuf[0] = '\0';

      // replace buffer, delete old one
    delete [] m_buffer;
    m_buffer = newbuf;
    m_bufsize = a_bufsize;
  }
} // EReadAsciiFile::SetBufferSize
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::GetLine
// PURPOSE   read next line from the file
// -----------------------------------------------------------------------------
bool EReadAsciiFile::GetLine (CStr *a_line/* =NULL*/)
{
  int len=0;

    // should have file to call this function
  if (m_fp == NULL) {
    return FALSE;
  }

    // release buffer - pass zero to simultaneously clear out line
  ReleaseBuffer();
  //m_line.Empty();

  while (fgets(m_buffer, m_bufsize, m_fp) != NULL) {
      // add next set of data to string
    m_line += m_buffer;

      // get buffer length. if we read a newline character, we're done
    len = (int)strlen(m_buffer);
    if (m_buffer[len-1] == '\n') {
      break;
    }
  }

    // make sure we got some data [even a blank line will have the
    // '\n' character so at this point thestring will not be empty].
  if (!m_line.IsEmpty()) {
      // increment line count
    m_linecount++;

//      // remove any of these trailing characters
//SLOW!!    m_line.TrimRight("\n\r");

      // get the buffer
    m_linesize = m_line.GetLength();
    m_ch = m_line.GetBuffer(m_linesize+1); // add 1 for the null terminator

      // remove any of these trailing characters
    char *chend = m_ch + (m_linesize - 1);
    while ((chend >= m_ch) &&
           (*chend == '\n' || *chend == '\r')) {
      *chend = '\0';
      chend--;
    }

      // pass back line if needed
    if (a_line != NULL) {
      *a_line = m_ch;
    }

      // return TRUE
    return TRUE;
  }

    // if we get here, file had no data: return FALSE
  return FALSE;
} // EReadAsciiFile::GetLine
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::GetFilePercent
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
double EReadAsciiFile::GetFilePercent () const
{
  if (m_fp == NULL) {
    return 0.0;
  }
  else if (m_filesize <= 0.0) {
    return 0.0;
  }

  return ftell(m_fp) / m_filesize;
} // EReadAsciiFile::GetFilePercent
//------------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   send the next char
// NOTES     this version does not skip delimiters! Just reads char by char!
//------------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (char &a_val)
{
  if (m_ch != NULL && *m_ch != '\0') {
    a_val = *m_ch;
    m_ch++;
    return TRUE;
  }

  return FALSE;
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (bool &a_val)
{
  int ival;

    // try reading value
  if (ReadData(ival)) {
    a_val = ival ? TRUE : FALSE;
    return TRUE;
  }

    // return FALSE if no value was read
  return FALSE;
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (boolean &a_val)
{
  int ival;

    // try reading value
  if (ReadData(ival)) {
    a_val = (boolean)ival;
    return TRUE;
  }

    // return FALSE if no value was read
  return FALSE;
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (int &a_val)
{
    // store previous location
  char *prevch=m_ch;

    // read next piece of data as string
  if (ReadData(m_data)) {
      // convert data to integer
    if (sscanf(m_data, "%d", &a_val) == 1) {
      return TRUE;
    }
  }

    // error: restore previous string location
  m_ch = prevch;

    // if using exceptions, we'll only get here if there was data in the
    // string but the data was not an integer.
  if (m_useexceptions) {
    throw ioTextNotANumber();
  }
  else {
    return FALSE;
  }
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (Uint &a_val)
{
  int ival;

    // try reading value
  if (ReadData(ival)) {
    a_val = (Uint)ival;
    return TRUE;
  }

    // return FALSE if no value was read
  return FALSE;
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (float &a_val)
{
    // store previous location
  char *prevch=m_ch;

    // read next piece of data as string
  if (ReadData(m_data)) {
      // convert data to float
    if (sscanf(m_data, "%f", &a_val) == 1) {
      return TRUE;
    }
  }

    // error: restore previous string location
  m_ch = prevch;

    // if using exceptions, we'll only get here if there was data in the
    // string but the data was not a float.
  if (m_useexceptions) {
    throw ioTextNotANumber();
  }
  else {
    return FALSE;
  }
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (double &a_val)
{
    // store previous location
  char *prevch=m_ch;

    // read next piece of data as string
  if (ReadData(m_data)) {
      // convert data to double
    if (sscanf(m_data, "%lf", &a_val) == 1) {
      return TRUE;
    }
  }

    // error: restore previous string location
  m_ch = prevch;

    // if using exceptions, we'll only get here if there was data in the
    // string but the data was not a double.
  if (m_useexceptions) {
    throw ioTextNotANumber();
  }
  else {
    return FALSE;
  }
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (CStr &a_val)
{
  if (m_ch == NULL) {
    if (m_useexceptions) {
      throw ioNoText();
    }
    else {
      return FALSE;
    }
  }

  char *chend=NULL;

    // skip leading whitespace
    // explicit char loop much faster than delimiter loop
  char *chFind=NULL;
  int done=FALSE;
  while (!done) {
    done = TRUE;
    if (*m_ch != '\0') {
      chFind = m_chDelimiters;
      while (*chFind != '\0') {
        if (*m_ch == *chFind) {
          done = FALSE;
          break;
        }
        chFind++;
      }
      if (!done) {
        m_ch++;
      }
    }
  }

    // if we have nothing, return false
  if (*m_ch == '\0') {
    if (m_useexceptions) {
      throw ioNoText();
    }
    else {
      return FALSE;
    }
  }


  chend = m_ch;
  int quote=0;
    // see if we have a quote
  if (*m_ch == '\"') {
    quote = 1;
    m_ch++;
    do {
      chend++;
    } while (*chend != '\0' && *chend != '\"');
  }
  else {
      // find end or next whitespace/delimiter
    do {
      done=FALSE;
      if (*chend != '\0') {
        chFind = m_chDelimiters;
        while (*chFind != '\0') {
          if (*chend == *chFind) {
            done = TRUE;
            break;
          }
          chFind++;
        }
        if (!done) {
          chend++;
        }
      }
      else {
        done = TRUE;
      }
    } while (!done);
  }

    // copy string
  int size = int(chend - m_ch);
  char *buf = a_val.GetBuffer(size+1);  // add 1 to allocate null terminator
  strncpy(buf, m_ch, size);
  a_val.ReleaseBuffer(size);

    // skip end quote if we have it
  if (quote) {
    if (*chend == '\"') {
      chend++;
    }
    else if (m_useexceptions) {
      throw ioNoCloseQuote();
    }
  }

    // tell where to start next time (delimiters will be skipped next time)
  m_ch = chend;

  return TRUE;
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (int a_begpos, int a_endpos, bool &a_val)
{
  int ival;

    // try reading value
  if (ReadData(a_begpos, a_endpos, ival)) {
    a_val = ival ? TRUE : FALSE;
    return TRUE;
  }

    // return FALSE if no value was read
  return FALSE;
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (int a_begpos, int a_endpos, boolean &a_val)
{
  int ival;

    // try reading value
  if (ReadData(a_begpos, a_endpos, ival)) {
    a_val = (boolean)ival;
    return TRUE;
  }

    // return FALSE if no value was read
  return FALSE;
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (int a_begpos, int a_endpos, int &a_val)
{
    // read next piece of data as string
  if (ReadData(a_begpos, a_endpos, m_data)) {
      // convert data to int
    if (sscanf(m_data, "%d", &a_val) == 1) {
      return TRUE;
    }
  }

    // if using exceptions, we'll only get here if there was data in the
    // string but the data was not an int.
  if (m_useexceptions) {
    throw ioTextNotANumber();
  }
  else {
    return FALSE;
  }
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (int a_begpos, int a_endpos, Uint &a_val)
{
  int ival;

    // try reading value
  if (ReadData(a_begpos, a_endpos, ival)) {
    a_val = (Uint)ival;
    return TRUE;
  }

    // return FALSE if no value was read
  return FALSE;
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (int a_begpos, int a_endpos, float &a_val)
{
    // read next piece of data as string
  if (ReadData(a_begpos, a_endpos, m_data)) {
      // convert data to double
    if (sscanf(m_data, "%f", &a_val) == 1) {
      return TRUE;
    }
  }

    // if using exceptions, we'll only get here if there was data in the
    // string but the data was not a float.
  if (m_useexceptions) {
    throw ioTextNotANumber();
  }
  else {
    return FALSE;
  }
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   
// NOTES     
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (int a_begpos, int a_endpos, double &a_val)
{
    // read next piece of data as string
  if (ReadData(a_begpos, a_endpos, m_data)) {
      // convert data to double
    if (sscanf(m_data, "%lf", &a_val) == 1) {
      return TRUE;
    }
  }

    // if using exceptions, we'll only get here if there was data in the
    // string but the data was not a double.
  if (m_useexceptions) {
    throw ioTextNotANumber();
  }
  else {
    return FALSE;
  }
} // EReadAsciiFile::ReadData
// -----------------------------------------------------------------------------
// FUNCTION  EReadAsciiFile::ReadData
// PURPOSE   fixed format reading
// -----------------------------------------------------------------------------
bool EReadAsciiFile::ReadData (int a_begpos, int a_endpos, CStr &a_val)
{
  if (m_ch == NULL) {
    if (m_useexceptions) {
      throw ioNoText();
    }
    else {
      return FALSE;
    }
  }

    // make sure line is long enough
  if (a_begpos >= m_linesize || a_endpos >= m_linesize) {
    if (m_useexceptions) {
      throw ioNoText();
    }
    else {
      return FALSE;
    }
  }

    // get start and end positions (end is one past the range)
  char *chbeg = m_ch + min(a_begpos, a_endpos);
  char *chend = m_ch + (max(a_begpos, a_endpos) + 1);

    // copy string
  int size = (int)(chend - chbeg);
  char *buf = a_val.GetBuffer(size+1); // add 1 to allocate null terminator
  strncpy(buf, chbeg, size);
  a_val.ReleaseBuffer(size);

    // success
  return TRUE;
} // EReadAsciiFile::ReadData




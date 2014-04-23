#ifndef EREADASCIIFILE_H
#define EREADASCIIFILE_H

#include <private\util\util.h>

class ioexception : public std::exception {
  public:
    virtual const char* what() const throw() {
      return "";
    }
};

class ioNoCloseQuote : public ioexception {
  public:
    virtual const char* what() const throw() {
      return "no close quote";
    }
};

class ioNoText : public ioexception {
  public:
    virtual const char* what() const throw() {
      return "no text";
    }
};

class ioTextNotANumber : public ioexception {
  public:
    virtual const char* what() const throw() {
      return "text not a number";
    }
};

// Perhaps future use with daGetLine
class ioNothingRead : public ioexception {
  public:
    virtual const char* what() const throw() {
      return "nothing read";
    }
};

// Use this for any other i/o error you might encounter.  You can pass in the
// error string.  Throw this yourself - it's not thrown by dataios.cpp
class ioError : public ioexception {
  public:
    ioError (LPCTSTR a_str="error") { m_str = a_str; }

    virtual const char* what() const throw() {
      return (LPCTSTR)m_str;
    }

  private:
    CStr m_str;
};

class EReadAsciiFile
{
  public:
    EReadAsciiFile (const char *a_filename=NULL);
   ~EReadAsciiFile (); // destructor will close the file if open

   void SetLine(const CStr &a_)
   {
     m_line = a_;
     m_ch = m_line.GetBuffer(m_linesize+1); // add 1 for the null terminator
   }

      // attach an open file pointer to start reading
    void Attach (FILE *a_fp);
      // detach a file pointer (WARNING: leaves the file pointer open)
    FILE *Detach ();

    bool SetFilename(const char *a_filename, bool a_opennow=TRUE);
    bool OpenFile();
    void    RewindFile();
    void    CloseFile();
    double  GetFileSize() { return m_filesize; }
    inline bool IsFileOpen() const    { return (bool)(m_fp != NULL); }
    inline int IsFileUnicode() const { return m_isunicode; }

      // options
    bool UseExceptions(bool a_useexceptions=TRUE); // returns prev status
    CStr SetDelimiters(const char *a_delimeters); // returns previous delimiters
    inline CStr GetDelimiters() const { return m_strDelimiters; }

      // this usually won't be needed
      // when a line is read, we do it in buffer blocks of a certain size. The
      // default size is 512 bytes. If the the lines are much longer than the
      // buffer size, reading can slow down slightly. If you know that your
      // file will have lines of a certain length, you can request a larger
      // buffer size to try speeding up file I/O. I do not recommend ever
      // specifying a smaller buffer size, as this will not be faster. -DSG
    void SetBufferSize(int a_bufsize);

      // this loads the next line from the file.
      // the line can be retrieved but that should rarely be done.
      // if you want to scan the data in, say, an sscanf statement, you
      // should use the LineData() function.
    bool GetLine(CStr *a_line=NULL);
    inline const char *LineData () { return m_ch; }

      // number of lines read from the file so far
    inline long GetLineCount() const { return m_linecount; }

      // this computes exactly how far into the file we are.
      // use this wisely (can greatly slow down reading if used at every line!)
    double GetFilePercent() const;

      // this reads the next char - does not skip delimiters! No exceptions are
      // sent by this version!
    bool ReadData(char &a_val);
      // these read the next piece of data from the current line using
      // the current delimiters as separators. these DO NOT automatically
      // get the next line! On successful read, these return TRUE.
      // if line has no data or data is not the correct
      // type, and exceptions were turned on by UseExceptions(), then these
      // throw exceptions. Otherwise, these return FALSE
    bool ReadData(bool &a_val);
    bool ReadData(boolean &a_val);
    bool ReadData(Uint &a_val);
    bool ReadData(int &a_val);
    bool ReadData(float &a_val);
    bool ReadData(double &a_val);
    bool ReadData(CStr &a_val);

      // these read data from the current line assuming a fixed format file
      // specify the start and end position from where to read the data
      // these should never be used with the non-fixed-format versions because
      // those versions edit the line while these leave the line in-tact.
      // to get a single character, beg and end should be equal to each other
    bool ReadData(int a_begpos, int a_endpos, bool &a_val);
    bool ReadData(int a_begpos, int a_endpos, boolean &a_val);
    bool ReadData(int a_begpos, int a_endpos, int &a_val);
    bool ReadData(int a_begpos, int a_endpos, Uint &a_val);
    bool ReadData(int a_begpos, int a_endpos, float &a_val);
    bool ReadData(int a_begpos, int a_endpos, double &a_val);
    bool ReadData(int a_begpos, int a_endpos, CStr &a_val);

      // template functions for GMS
    template <class T> bool ReadNext(T &data);
    template <class T> bool ReadArray(int num, T *array);

  private:
      // optimization: simultaneously reset line to zero when releasing buffer
      // so we don't have to call Reset() when reading next line. This saves
      // 10% of the time from GetLine().
    inline void ReleaseBuffer() { if (m_ch != NULL) {
      m_line.ReleaseBuffer(0); m_ch = NULL; m_linesize = 0; } }
      // flags
    enum { UC_ASCII, UC_SMALLENDIAN, UC_BIGENDIAN, UC_UTF8 };
    int   m_isunicode;
    bool  m_useexceptions;

      // data for reading files
    int   m_bufsize;
    char  *m_buffer;
    FILE  *m_fp;

      // data for reading lines
    int   m_linesize;
    char  *m_ch;
    char  *m_chDelimiters;
    CStr  m_strDelimiters;
    CStr  m_line, m_data, m_strFilename;
    long  m_linecount; // lines read so far
    double m_filesize; // size in bytes
};


// template function for GMS
// this version DOES read additional lines of data until the next piece of
// information can be found. This can still fail if, for instance, we are
// looking for a number but the next piece of information is a text string.
template <class T> bool EReadAsciiFile::ReadNext(T &a_val)
{
    // make sure file is open
  if (m_fp == NULL) {
    ASSERT(FALSE);
    return FALSE;
  }

    // if we don't have a line, or if our line has no data, get next line.
  while (m_ch == NULL || *m_ch == '\0') {
    if (!GetLine()) {
      return FALSE;
    }
  }

    // try to get data from the line
  return ReadData(a_val);
}

// template function for GMS
// this version DOES read additional lines of data until the next piece of
// information can be found. This can still fail if, for instance, we are
// looking for a number but the next piece of information is a text string.
template <class T> bool EReadAsciiFile::ReadArray(int num, T *a_array)
{
  int count=0;

  while (count < num) {
    if (!ReadNext(a_array[count])) {
      return FALSE;
    }
    count++;
  }
}

#endif


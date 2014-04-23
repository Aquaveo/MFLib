//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5DataSetWriterSetup_H
#define H5DataSetWriterSetup_H

#include <private\util\util.h>

// if you are writing a dataset to a multidimesional array then you
// send in this information before calling the function to write the data
class H5DSWriterDimInfo
{
public:
  std::vector<hsize_t> m_startLoc; // the location to begin writing the values
                                   // in each dimension
  std::vector<hsize_t> m_nToWrite; // the number of values to write in each
                                   // dimension
  H5DSWriterDimInfo(const std::vector<hsize_t> &a_startLoc,
                    const std::vector<hsize_t> &a_nToWrite) :
    m_startLoc(a_startLoc),
    m_nToWrite(a_nToWrite)
    {}

  H5DSWriterDimInfo(const H5DSWriterDimInfo &rhs) :
    m_startLoc(rhs.m_startLoc),
    m_nToWrite(rhs.m_nToWrite)
    {}

  const H5DSWriterDimInfo &operator=(const H5DSWriterDimInfo &rhs)
  {
    if (this != &rhs)
    {
      m_startLoc = rhs.m_startLoc;
      m_nToWrite = rhs.m_nToWrite;
    }
    return (*this);
  }

private:
};
class H5DataSetWriterSetup
{
public:
  H5DataSetWriterSetup(const char *a_file="",
                       const char *a_path="",
                       const hid_t a_type=H5T_NATIVE_INT,
                       const int a_nDim=1,
                       const bool a_compressed=true) :
    m_file(a_file),
    m_path(a_path),
    m_type(a_type),
    m_nDim(a_nDim),
    m_compressed(a_compressed)
    {}
  ~H5DataSetWriterSetup() {}

  H5DataSetWriterSetup(const H5DataSetWriterSetup &rhs) :
    m_file(rhs.m_file),
    m_path(rhs.m_path),
    m_type(rhs.m_type),
    m_nDim(rhs.m_nDim),
    m_compressed(rhs.m_compressed),
    m_chunkSize(rhs.m_chunkSize)
    {}
  const H5DataSetWriterSetup &operator=(const H5DataSetWriterSetup &rhs)
  {
    if (this != &rhs)
    {
      m_file = rhs.m_file;
      m_path = rhs.m_path;
      m_type = rhs.m_type;
      m_nDim = rhs.m_nDim;
      m_compressed = rhs.m_compressed;
      m_chunkSize = rhs.m_chunkSize;
    }
    return (*this);
  }
  void SetChunkSize(const std::vector<int> &a_chunkSize)
  {
    m_chunkSize = a_chunkSize;
  }

  CStr m_file, m_path;
  hid_t m_type;
  int m_nDim;
  bool m_compressed;
  std::vector<int> m_chunkSize;

private:
};

#endif

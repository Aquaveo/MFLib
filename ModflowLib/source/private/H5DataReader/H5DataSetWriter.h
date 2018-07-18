//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5DATASETWRITER_H
#define H5DATASETWRITER_H

#include <private/util/util.h>

class H5DataSetWriterT;
class H5DataSetWriterSetup;
class H5DSWriterDimInfo;

namespace H5DataReader
{
  void CloseAllH5FilesOpenForWriting();
  hid_t GetFileId(const char *a_);
  hid_t GetFileIdIfOpen(const char *a_);
}

class H5DataSetWriter
{
friend H5DataSetWriterT;
public:
  H5DataSetWriter(const H5DataSetWriterSetup *a_);
  ~H5DataSetWriter();

  void AllowTypeConversions(bool a_);

  void SetDimInfoForWriting(H5DSWriterDimInfo *a_);

  bool WriteData(const float *a_, size_t a_num);
  bool WriteData(const double *a_, size_t a_num);
  bool WriteData(const int *a_, size_t a_num);
  bool WriteData(const char *a_, size_t a_num);

  bool WriteAtt(const char * const a_str,
                int a_i);
  bool WriteAtt(const char* const a_str,
                const char* const a_val);
  bool WriteAtt(const char* const a_str,
                double a_val);

  bool CreateGroup(const char *a_);

  //bool WriteData(const char *a_path,
  //               int a_numDim,
  //               int *a_dimSize,
  //               int *a_startLoc,
                 
private:
  H5DataSetWriter(const H5DataSetWriter &rhs);
  const H5DataSetWriter& operator=(const H5DataSetWriter &rhs);

  class impl;
  impl *m_p;
};

#endif

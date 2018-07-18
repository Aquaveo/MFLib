//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef DATASETREADER_H
#define DATASETREADER_H

#include <private/util/util.h>

class H5DataSetReaderImpl;
////////////////////////////////////////////////////////////////////////////////
/// \class DataSetReader
/// \brief Used to read data from an HDF5 file given the name of the file, the
///  path to the data set and a vector of indices that define the offsets into
///  the dataset.
////////////////////////////////////////////////////////////////////////////////
class H5DataSetReader
{
friend class H5DataSetReaderT;
public:
  H5DataSetReader(const CStr &a_file,
                  const CStr &a_path,
                  const VEC_INT_PAIR &a_indices=VEC_INT_PAIR());
  ~H5DataSetReader();

  bool DataSetExists();
  bool GetDataSetDimensions(std::vector<hsize_t> &a_size);

  bool GetData(double *a_, size_t a_size);
  bool GetData(float *a_, size_t a_size);
  bool GetData(int *a_, size_t a_size);
  bool GetData(char *a_, size_t a_size);

  bool GetData(std::vector<double> &a_);
  bool GetData(std::vector<float> &a_);
  bool GetData(std::vector<int> &a_);

  bool GetAllData(std::vector<double> &a_);
  bool GetAllData(std::vector<float> &a_);
  bool GetAllData(std::vector<char> &a_);
  bool GetAllData(std::vector<int> &a_);

  bool GetAtt(const char * const a_str, int &a_i);

  void AllowTypeConversions(bool a_);

private:
  H5DataSetReader(const H5DataSetReader &rhs);
  const H5DataSetReader& operator=(const H5DataSetReader &rhs);

  H5DataSetReaderImpl *m_p;
};

namespace H5Reader
{
  void CloseAllH5Files();
  hid_t OpenFile(const char * const a_fname);
  void SetH5FilePath(const CStr& a_path);
};

#endif

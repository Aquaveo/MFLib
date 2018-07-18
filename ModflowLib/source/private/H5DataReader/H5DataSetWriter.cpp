//------------------------------------------------------------------------------
// FILE      H5DataSetWriter.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/H5DataReader/H5DataSetWriter.h>

#include <map>

#include <private/H5DataReader/H5DataReaderUtil.h>
#include <private/H5DataReader/H5DataSetWriterSetUp.h>
#include <private/MfData/MfExport/private/H5/H5Util.h>
#include <private/util/util.h>

class iWriteH5
{
public:
  template <class T>
static bool WriteData(H5DataSetWriter::impl &a_impl,
                        T *a_,
                        size_t a_num);
private:
  template <class T, class U>
static bool WriteData(H5DataSetWriter::impl &a_impl,
                        T *a_,
                        size_t a_num);
};

class H5DataSetWriter::impl
{
friend H5DataSetWriterT;
friend iWriteH5;
public:
  impl(const H5DataSetWriterSetup *a_);
  ~impl();

  void AllowTypeConversions(bool a_) { m_allowConversions = a_; }
  bool OpenFile();
  bool DatasetExists();
  bool OpenDatasetAndCreateIfNeeded();
  bool OpenDataset();
  herr_t SetFillValue(hid_t a_paramId);
  bool CreateDataset();
  bool OpenDataspace();

  template <class T>
  bool WriteDataT(const T *a_, size_t a_num);
  template <class T>
  bool DoErrorChecksT(const T *a_, size_t a_num);
  bool CheckDimensionsAndExtendDataIfNeeded(size_t a_);
  bool DataClassTypeMatches();
  bool DataDimensionsMatch();
  bool CreateFilespaceAndMemoryspace();

  void SetDimInfoForWriting(H5DSWriterDimInfo *a_);

  bool WriteAtt(const char * const a_str, int a_i);
  bool WriteAtt(const char * const a_str,
                const char * const a_val);
  bool WriteAtt(const char * const a_str,
                double a_val);

  bool CreateGroup(const char *a_);

private:
  H5DataSetWriterSetup m_setup;
  hid_t m_fid, m_dataId, m_spaceId, m_fSpace, m_mSpace;
  bool m_allowConversions;
  H5DSWriterDimInfo m_dimInfo;
};

//------------------------------------------------------------------------------
/// \brief Function to write the data to an H5 File
//------------------------------------------------------------------------------
template <class T>
bool iWriteH5::WriteData (H5DataSetWriter::impl &a_impl,
                          T *a_,
                          size_t a_num)
{
  bool rval(false);
  if (a_impl.m_setup.m_type == H5T_NATIVE_INT)
  {
    rval = WriteData<T, int>(a_impl, a_, a_num);
  }
  else if (a_impl.m_setup.m_type == H5T_NATIVE_FLOAT)
  {
    rval = WriteData<T, float>(a_impl, a_, a_num);
  }
  else if (a_impl.m_setup.m_type == H5T_NATIVE_DOUBLE)
  {
    rval = WriteData<T, double>(a_impl, a_, a_num);
  }
  else if (a_impl.m_setup.m_type == H5T_NATIVE_CHAR)
  {
    rval = WriteData<T, char>(a_impl, a_, a_num);
  }
  return rval;
} // iWriteH5::WriteData
//------------------------------------------------------------------------------
/// \brief Function to write the data to an H5 File
//------------------------------------------------------------------------------
template <class T, class U>
bool iWriteH5::WriteData (H5DataSetWriter::impl &a_impl,
                          T *a_,
                          size_t a_num)
{
  U *data(0);
  std::vector<U> vData;

  if (a_impl.m_setup.m_type == H5DataReader::Get_H5_nativetype(a_))
    data = (U*)a_;
  else
  {
    vData.reserve(a_num);
    for (size_t i=0; i<a_num; i++)
      vData.push_back((U)a_[i]);
    data = &vData[0];
  }

  if (H5Dwrite(a_impl.m_dataId, a_impl.m_setup.m_type, a_impl.m_mSpace,
               a_impl.m_fSpace, H5P_DEFAULT, data) < 0)
    throw std::runtime_error("H5Dwrite failed.");

  return true;
} // iWriteH5:: WriteData
//------------------------------------------------------------------------------
/// \brief Gets the file global
//------------------------------------------------------------------------------
static std::map<CStr, hid_t>& GetFileMap()
{
  static std::map<CStr, hid_t> m_map; // ok to leave static
  return m_map;
}
//------------------------------------------------------------------------------
/// \brief Opens H5 files for writing and saves the file in a map
//------------------------------------------------------------------------------
static hid_t OpenH5ForWriting (const char *a_fname)
{
  std::map<CStr, hid_t>::iterator it;
  hid_t fid;
  CStr file(a_fname);

  it = GetFileMap().find(file);
  if (it == GetFileMap().end())
  {
    // open the file
    fid = H5Fcreate(file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid < 0) {
      std::string err = "Unable to open " + file + ".";
      throw std::runtime_error(err);
    }
    GetFileMap().insert(std::make_pair(file, fid));
  }
  else
    fid = it->second;
  return fid;
} // OpenH5ForWriting
//------------------------------------------------------------------------------
/// \brief Closes all of the open files
//------------------------------------------------------------------------------
static void CloseAllOpenH5Files ()
{
  std::map<CStr, hid_t>::iterator it;
  for (it = GetFileMap().begin(); it != GetFileMap().end(); it++)
  {
    H5Fclose(it->second);
  }
  GetFileMap().clear();
} // CloseAllOpenH5Files
//------------------------------------------------------------------------------
/// \brief Closes all of the open files
//------------------------------------------------------------------------------
void H5DataReader::CloseAllH5FilesOpenForWriting ()
{
  CloseAllOpenH5Files();
} // H5DataReader::CloseAllH5FilesOpenForWriting
//------------------------------------------------------------------------------
/// \brief Gets an hdf5 file id from the file name
//------------------------------------------------------------------------------
hid_t H5DataReader::GetFileId (const char * const a_)
{
  return(OpenH5ForWriting(a_));
} // H5DataReader::GetFileId
//------------------------------------------------------------------------------
/// \brief Gets an hdf5 file id from the file name
//------------------------------------------------------------------------------
hid_t H5DataReader::GetFileIdIfOpen (const char * const a_)
{
  hid_t fid(-1);
  std::map<CStr, hid_t>::iterator it;
  CStr file(a_);

  it = GetFileMap().find(file);
  if (it != GetFileMap().end())
  {
    fid = it->second;
  }
  return(fid);
} // H5DataReader::GetFileId
//------------------------------------------------------------------------------
/// \brief constructor
//------------------------------------------------------------------------------
H5DataSetWriter::H5DataSetWriter (const H5DataSetWriterSetup *a_) :
m_p(new H5DataSetWriter::impl(a_))
{
} // H5DataSetWriter::H5DataSetWriter
//------------------------------------------------------------------------------
/// \brief Destructor
//------------------------------------------------------------------------------
H5DataSetWriter::~H5DataSetWriter ()
{
  try
  {
    if (m_p)
      delete(m_p);
  }
  catch (...) {}
} // H5DataSetWriter::~H5DataSetWriter
//------------------------------------------------------------------------------
/// \brief This allows type conversion. So if the dataset on disk is a of type
/// double but I passed an array of floats or ints to this class it will write
/// that data to the dataset if this is set. Otherwise it will not try to write
/// data that does not match the type on disk.
//------------------------------------------------------------------------------
void H5DataSetWriter::AllowTypeConversions (bool a_)
{
  m_p->AllowTypeConversions(a_);
} // H5DataSetWriter::AllowTypeConversions
//------------------------------------------------------------------------------
/// \brief If you are writing data to a multidimensional array then this
/// should be called before called the write data method.
//------------------------------------------------------------------------------
void H5DataSetWriter::SetDimInfoForWriting (H5DSWriterDimInfo *a_)
{
  m_p->SetDimInfoForWriting(a_);
} // H5DataSetWriter::AllowTypeConversions
//------------------------------------------------------------------------------
/// \brief Writes data to h5 file
//------------------------------------------------------------------------------
bool H5DataSetWriter::WriteData (const double *a_, size_t a_num)
{
  return (m_p->WriteDataT(a_, a_num));
} // H5DataSetWriter::WriteData
bool H5DataSetWriter::WriteData (const float *a_, size_t a_num)
{
  return (m_p->WriteDataT(a_, a_num));
} // H5DataSetWriter::WriteData
bool H5DataSetWriter::WriteData (const int *a_, size_t a_num)
{
  return (m_p->WriteDataT(a_, a_num));
} // H5DataSetWriter::WriteData
bool H5DataSetWriter::WriteData (const char *a_, size_t a_num)
{
  return (m_p->WriteDataT(a_, a_num));
} // H5DataSetWriter::WriteData
//------------------------------------------------------------------------------
/// \brief Writes an integer attribute to h5 file
//------------------------------------------------------------------------------
bool H5DataSetWriter::WriteAtt (const char * const a_str,
                                int a_i)
{
  return m_p->WriteAtt(a_str, a_i);
} // H5DataSetWriter::WriteAtt
//------------------------------------------------------------------------------
/// \brief Writes a string attribute to h5 file
//------------------------------------------------------------------------------
bool H5DataSetWriter::WriteAtt (const char * const a_str,
                                const char* const a_val)
{
  return m_p->WriteAtt(a_str, a_val);
} // H5DataSetWriter::WriteAtt
//------------------------------------------------------------------------------
/// \brief Writes a double attribute to h5 file
//------------------------------------------------------------------------------
bool H5DataSetWriter::WriteAtt (const char * const a_str,
                                double a_val)
{
  return m_p->WriteAtt(a_str, a_val);
} // H5DataSetWriter::WriteAtt
//------------------------------------------------------------------------------
/// \brief Creates a group in an h5 file
//------------------------------------------------------------------------------
bool H5DataSetWriter::CreateGroup (const char *a_)
{
  return(m_p->CreateGroup(a_));
} // H5DataSetWriter::CreateGroup

//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
H5DataSetWriter::impl::impl (const H5DataSetWriterSetup *a_) :
m_setup(a_ ? *a_ : H5DataSetWriterSetup()),
m_fid(-1),
m_dataId(-1),
m_spaceId(-1),
m_fSpace(-1),
m_mSpace(-1),
m_allowConversions(false),
m_dimInfo(H5DSWriterDimInfo(std::vector<hsize_t>(), std::vector<hsize_t>()))
{
} // H5DataSetWriter::impl::impl
//------------------------------------------------------------------------------
/// \brief Destructor
//------------------------------------------------------------------------------
H5DataSetWriter::impl::~impl ()
{
  try
  {
    if (m_dataId > -1)
      H5Dclose(m_dataId);
    if (m_spaceId > -1)
      H5Sclose(m_spaceId);
    if (m_fSpace > -1)
      H5Sclose(m_fSpace);
    if (m_mSpace > -1)
      H5Sclose(m_mSpace);
  }
  catch (...) {}
} // H5DataSetWriter::impl::~impl
//------------------------------------------------------------------------------
/// \brief Opens an HDF5 file for writing
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::OpenFile ()
{
  try
  {
    m_fid = OpenH5ForWriting(m_setup.m_file);
  }
  catch (std::exception &e)
  {
    ErrorStack::Get().PutError(e.what());
    return false;
  }
  return true;
} // H5DataSetWriter::impl::OpenFile
//------------------------------------------------------------------------------
/// \brief Opens a data set in preparation for writing
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::DatasetExists ()
{
  if (m_fid < 0)
    return false;
  hid_t id(H5Dopen(m_fid, m_setup.m_path));
  bool rval(id > -1);
  H5Dclose(id);
  return rval;
} // H5DataSetWriter::impl::DatasetExists
//------------------------------------------------------------------------------
/// \brief Opens a data set in preparation for writing
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::OpenDatasetAndCreateIfNeeded ()
{
  if (DatasetExists())
    return OpenDataset();
  else
    return CreateDataset();
} // H5DataSetWriter::impl::OpenDatasetAndCreateIfNeededfunction
//------------------------------------------------------------------------------
/// \brief Opens a data set in preparation for writing
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::OpenDataset ()
{
  if (m_fid < 0)
    return false;
  if (m_dataId > -1)
    return true;

  m_dataId = H5Dopen(m_fid, m_setup.m_path);
  if (m_dataId > -1 &&
      !DataClassTypeMatches())
  {
    H5Dclose(m_dataId);
    m_dataId = -1;
    return false;
  }
  return (m_dataId > -1);
} // H5DataSetWriter::impl::OpenDataset
//------------------------------------------------------------------------------
/// \brief Function to write the data to an H5 File
//------------------------------------------------------------------------------
herr_t H5DataSetWriter::impl::SetFillValue (hid_t a_paramId)
{
  herr_t status(-1);
  if (m_setup.m_type == H5T_NATIVE_INT)
  {
    int val(0);
    status = H5Pset_fill_value(a_paramId, m_setup.m_type, &val);
  }
  else if (m_setup.m_type == H5T_NATIVE_FLOAT)
  {
    float val(0);
    status = H5Pset_fill_value(a_paramId, m_setup.m_type, &val);
  }
  else if (m_setup.m_type == H5T_NATIVE_DOUBLE)
  {
    double val(0);
    status = H5Pset_fill_value(a_paramId, m_setup.m_type, &val);
  }
  else if (m_setup.m_type == H5T_NATIVE_CHAR)
  {
    char val(0);
    status = H5Pset_fill_value(a_paramId, m_setup.m_type, &val);
  }
  return status;
} // H5DataSetWriter::impl::SetFillValue
//------------------------------------------------------------------------------
/// \brief Creates a data set in an h5 file
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::CreateDataset ()
{
  if (m_fid < 0)
    return false;
  if (DatasetExists())
    return false;

  bool rval(true);
  hid_t spaceId(-1), paramId(-1);
  herr_t status(0);

  try
  {
    const int rank(m_setup.m_nDim);
    // For chunk size below we're going with 3 for now.  We tried 50 and it
    // slows down the intermediate tests exporting H5.  Also tried 10 which
    // seems to be only slightly slower than 3.
    std::vector<hsize_t> dim(rank,1), maxdim(rank, H5S_UNLIMITED),
                         chunk(rank, 3);
    
    //if (chunk.size() == m_setup.m_chunkSize.size())
    //{
    //  for (size_t i = 0; i < chunk.size(); ++i)
    //    chunk[i] = m_setup.m_chunkSize[i];
    //}
    //else if (m_setup.m_path.Find("Arrays") == -1)
    //{
    //  // Need to set appropriate chunksize for this dataset
    //  ASSERT(false);
    //}
    // kluge for better chunking
    {
      if (m_setup.m_path.Find("Arrays") != -1
          || m_setup.m_path.Find("02. Cell IDs") != -1
          || m_setup.m_path.Find("03. Name") != -1
          || m_setup.m_path.Find("04. Map ID") != -1
          || m_setup.m_path.Find("06. IFACE") != -1
          || m_setup.m_path.Find("08. Str reach segment ID") != -1
          || m_setup.m_path.Find("09. Layer") != -1
          || m_setup.m_path.Find("09. Segment ID") != -1
          || m_setup.m_path.Find("10. Segment Flow") != -1
          || m_setup.m_path.Find("11. ITRIB") != -1
          || m_setup.m_path.Find("12. Upstream ID") != -1
          || m_setup.m_path.Find("12. IUZFBND") != -1
          || m_setup.m_path.Find("14. IRUNBND") != -1
          || m_setup.m_path.Find("16. VKS") != -1
          || m_setup.m_path.Find("18. EPS") != -1
          || m_setup.m_path.Find("20. THTS") != -1
          || m_setup.m_path.Find("22. THTI") != -1
          )
      {
        chunk[0] = 5000;
      }

      if (m_setup.m_path.Find("14. Segment Property") != -1
          || m_setup.m_path.Find("15. Segment Flow Table") != -1)
      {
        chunk[1] = 5000;
      }

      if (m_setup.m_path.Find("07. Property") != -1
          || m_setup.m_path.Find("16. Ext Depth") != -1
          || m_setup.m_path.Find("18. Evap Rate") != -1
          )
      {
        chunk[0] = 3;
        chunk[1] = 5000;
        if (rank > 2)
          chunk[2] = 1;
        if (m_setup.m_path.Find("Recharge") != -1 ||
            m_setup.m_path.Find("ET") != -1 ||
            m_setup.m_path.Find("ETS") != -1)
        {
          chunk[0] = 3;      // Layer/MultiIndex
          chunk[1] = 5000;   // Cells
        }
      }

      if (rank == 2 && m_setup.m_path.find("Parameter ") != -1 &&
        (m_setup.m_path.find("Values") != -1 ||
        m_setup.m_path.find("Active")))
      {
        //printf("Chunking for parameter data sets. Rank = %d\n", rank);
        //printf("Data set name: %s\n", m_setup.m_path.c_str());
        //fflush(stdout);
        chunk[0] = 1;
        chunk[1] = 5000;
      }
    }

    // create the data space
    spaceId = H5Screate_simple(rank, &dim[0], &maxdim[0]);
    if (spaceId < 0)
      throw std::runtime_error("Unable to create data space in file: " +
                           m_setup.m_file);

    // setup the dataset creation properties
    paramId = H5Pcreate(H5P_DATASET_CREATE);
    if (paramId < 0)
      throw std::runtime_error("Unable to create dataset parameters in file: " +
                           m_setup.m_file);
    status = H5Pset_chunk(paramId, rank, &chunk[0]);
    if (status < 0)
      throw std::runtime_error("Unable to set chunk property in file: " +
                           m_setup.m_file);
    
    if (m_setup.m_compressed)
    {
      status = H5Pset_deflate(paramId, 1);
      if (status < 0)
        throw std::runtime_error("Unable to set compression property in file: " +
                             m_setup.m_file);
    }

    status = SetFillValue(paramId);
    if (status < 0)
      throw std::runtime_error("Unable to set fill value property in file: " +
                           m_setup.m_file);

    // create the dataset
    m_dataId = H5Dcreate(m_fid, m_setup.m_path, m_setup.m_type, spaceId, paramId);
    if (m_dataId < 0)
      throw std::runtime_error("Unable to create dataset in file: " +
                           m_setup.m_file);

  }
  catch (std::exception &e)
  {
    ErrorStack::Get().PutError(e.what());
    rval = false;
  }
  if (paramId > -1)
    H5Pclose(paramId);
  if (spaceId > -1)
    H5Sclose(spaceId);
  return rval;
} // H5DataSetWriter::impl::CreateDataset
//------------------------------------------------------------------------------
/// \brief Opens the dataspace associated with a dataset in an h5 file
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::OpenDataspace ()
{
  if (m_fid < 0 || m_dataId < 0)
    return false;
  if (m_spaceId > -1)
    return true;
  m_spaceId = H5Dget_space(m_dataId);
  if (m_spaceId > -1 &&
      ( H5Sis_simple(m_spaceId) <= 0 || // data space must be simple
        !DataDimensionsMatch() ) ) // the dimensions must match
  {
    H5Sclose(m_spaceId);
    m_spaceId = -1;
  }
  return (m_spaceId > -1);
} // H5DataSetWriter::impl::OpenDataspace
//------------------------------------------------------------------------------
/// \brief Writes data to the hdf5 file
//------------------------------------------------------------------------------
template <class T>
bool H5DataSetWriter::impl::WriteDataT (const T *a_, size_t a_num)
{
  bool rval(true);
  try
  {
    DoErrorChecksT(a_, a_num); // basic error checks
    if (a_num > 0)
    {
      CheckDimensionsAndExtendDataIfNeeded(a_num);
      CreateFilespaceAndMemoryspace();
      iWriteH5::WriteData(*this, a_, a_num);
    }
  }
  catch (std::exception &e)
  {
    CStr msg(e.what());
    if (!msg.empty())
      ErrorStack::Get().PutError(msg);
    rval = false;
  }
  return rval;
} // H5DataSetWriter::impl::WriteDataT
//------------------------------------------------------------------------------
/// \brief Checks to make sure that everything is ok before we try to write
/// to the h5 file.
//------------------------------------------------------------------------------
template <class T>
bool H5DataSetWriter::impl::DoErrorChecksT (const T *a_, size_t a_num)
{
  // check if the data that was passed in can be written to this dataset
  if (!m_allowConversions &&
      m_setup.m_type != H5DataReader::Get_H5_nativetype(a_))
    throw std::runtime_error("Data types do not match.");

  // make sure the dataset and the dataspace are open
  if (!OpenFile() ||
      !OpenDatasetAndCreateIfNeeded() ||
      !OpenDataspace())
    throw std::exception();

  // if we have info for dimensions make sure the arrays are the same size
  if (m_dimInfo.m_startLoc.size() != m_dimInfo.m_nToWrite.size())
    throw std::runtime_error("Error in dimension information.");

  // see if the number we are writing is consistent with the dimension info
  if (!m_dimInfo.m_nToWrite.empty())
  {
    if (static_cast<size_t>(m_setup.m_nDim) != m_dimInfo.m_nToWrite.size())
      throw std::runtime_error("Invalid dimensions for dataset.");
    hsize_t num(1);
    for (size_t i=0; i<m_dimInfo.m_nToWrite.size(); i++)
      num *= m_dimInfo.m_nToWrite[i];
    if (num != a_num)
      throw std::runtime_error("Error in the size of the data to write.");
  }

  return true;
} // H5DataSetWriter::impl::DoErrorChecks
//------------------------------------------------------------------------------
/// \brief Checks the dimensions of the dataset and sees if we need to 
/// extend the dataset
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::CheckDimensionsAndExtendDataIfNeeded (size_t a_)
{
  if (m_fid < 0 || m_dataId < 0 || m_spaceId < 0)
    return false;
  if (a_ == 0) return true;

  int stat;
  std::vector<hsize_t> dims(m_setup.m_nDim, 0), maxDims(m_setup.m_nDim, 0);
  stat = H5Sget_simple_extent_dims(m_spaceId, &dims[0], &maxDims[0]);
  if (stat != m_setup.m_nDim)
    throw std::runtime_error("Invalid dimensions for dataset.");

  if (m_dimInfo.m_startLoc.empty() && m_setup.m_nDim == 1)
  {
    m_dimInfo.m_startLoc.push_back(0);
    m_dimInfo.m_nToWrite.push_back(static_cast<hid_t>(a_));
  }
  else if (m_dimInfo.m_startLoc.empty())
    throw std::runtime_error("Dimension information must be provided.");

  size_t i;
  bool extend(false);
  std::vector<hsize_t> newDims(dims);
  for (i=0; i<dims.size(); i++)
  {
    if (dims[i] < (m_dimInfo.m_startLoc[i] + m_dimInfo.m_nToWrite[i]))
    {
      extend = true;
      newDims[i] = m_dimInfo.m_startLoc[i] + m_dimInfo.m_nToWrite[i];
    }
  }

  if (extend)
  {
    // see if we can extend
    for (i=0; i<dims.size(); i++)
    {
      if (maxDims[i] < newDims[i])
        throw std::runtime_error("Max dimensions of dataset are too small.");
    }
    // extend the dataset
    if (H5Dextend(m_dataId, &newDims[0]) < 0)
      throw std::runtime_error("Unable to extend dataset: " + m_setup.m_path + ".");
  }
  return true;
} // H5DataSetWriter::impl::CheckDimensionsAndExtendDataIfNeeded
//------------------------------------------------------------------------------
/// \brief Checks to make sure that data type matches that of the dataset in
/// the h5 file.
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::DataClassTypeMatches ()
{
  if (m_fid < 0 || m_dataId < 0)
    return false;

  try
  {
    // check that the data type matches
    hid_t dataTypeId = H5Dget_type(m_dataId);
    if (dataTypeId < 0)
      throw std::runtime_error("Unable to get datatype.");
    H5T_class_t classType = H5Tget_class(dataTypeId);
    H5Tclose(dataTypeId);
    if (classType != H5DataReader::Get_H5T_class_t(m_setup.m_type))
      throw std::runtime_error("The data types do not match.");
  }
  catch (std::exception &e)
  {
    ErrorStack::Get().PutError(e.what());
    return false;
  }
  return true;
} // H5DataSetWriter::impl::DataClassTypeMatches
//------------------------------------------------------------------------------
/// \brief Checks to make sure that the number of dimensions match the
/// number of dimensions of the dataset in the h5 file.
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::DataDimensionsMatch ()
{
  if (m_fid < 0 || m_dataId < 0 || m_spaceId < 0)
    return false;

  return (m_setup.m_nDim == H5Sget_simple_extent_ndims(m_spaceId));
} // H5DataSetWriter::impl::DataDimensionsMatch
//------------------------------------------------------------------------------
/// \brief Checks to make sure that the number of dimensions match the
/// number of dimensions of the dataset in the h5 file.
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::CreateFilespaceAndMemoryspace ()
{
  if (m_fid < 0 || m_dataId < 0 || m_spaceId < 0)
    return false;

  m_fSpace = H5Dget_space(m_dataId);
  if (m_fSpace < 0)
    throw std::runtime_error("Unable to get file space.");

  m_mSpace = H5Screate_simple(m_setup.m_nDim, &m_dimInfo.m_nToWrite[0], 0);
  if (m_mSpace < 0)
    throw std::runtime_error("Unable to get memory space.");

  if (H5Sselect_hyperslab(m_fSpace, H5S_SELECT_SET, &m_dimInfo.m_startLoc[0],
                          0, &m_dimInfo.m_nToWrite[0], 0) < 0)
    throw std::runtime_error("Unable to select hyperslab.");

  return true;
} // H5DataSetWriter::impl::CreateFilespaceAndMemoryspace
//------------------------------------------------------------------------------
/// \brief If you are writing data to a multidimensional array then this
/// should be called before called the write data method.
//------------------------------------------------------------------------------
void H5DataSetWriter::impl::SetDimInfoForWriting (H5DSWriterDimInfo *a_)
{
  if (a_)
    m_dimInfo = *a_;
} // H5DataSetWriter::impl::SetDimInfoForWriting
//------------------------------------------------------------------------------
/// \brief Writes an integer attribute to h5 file
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::WriteAtt(const char * const a_str, int a_i)
{
  bool rval(true);
  try
  {
    if (!OpenFile() || !OpenDataspace())
      throw std::exception();

    hsize_t dim(1);
    hid_t spaceId = H5Screate_simple(1, &dim, &dim);
    if (spaceId < 0)
      return false;

    hid_t attId = H5Aopen_name(m_dataId, a_str);
    if (attId < 0)
    {
      attId = H5Acreate(m_dataId, a_str, H5T_NATIVE_INT, m_spaceId, 
                        H5P_DEFAULT);
      if (attId < 0)
      {
        H5Sclose(spaceId);
        return false;
      }
    }

    herr_t status = H5Awrite(attId, H5T_NATIVE_INT, &a_i);
    if (status < 0)
      rval = false;

    H5Sclose(spaceId);
    H5Aclose(attId);
  }
  catch (std::exception &)
  {
    rval = false;
  }
  return rval;
} // H5DataSetWriter::WriteAtt
//------------------------------------------------------------------------------
/// \brief Writes an integer attribute to h5 file
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::WriteAtt (const char * const a_str,
                                      const char * const a_val)
{
  bool rval(true);
  try
  {
    if (!OpenFile() || !OpenDataspace())
      throw std::exception();

    xfpWriteAttributeString(m_dataId, a_str, a_val);
  }
  catch (std::exception&)
  {
    rval = false;
  }
  return rval;
} // H5DataSetWriter::impl::WriteAtt
//------------------------------------------------------------------------------
/// \brief Writes an integer attribute to h5 file
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::WriteAtt (const char * const a_str,
                                      double a_val)
{
  bool rval(true);
  try
  {
    if (!OpenFile() || !OpenDataspace())
      throw std::exception();

    xfpWriteAttributeDouble(m_dataId, a_str, 1, &a_val);
  }
  catch (std::exception&)
  {
    rval = false;
  }
  return rval;
} // H5DataSetWriter::impl::WriteAtt
//------------------------------------------------------------------------------
/// \brief If you are writing data to a multidimensional array then this
/// should be called before calling the write data method.
//------------------------------------------------------------------------------
bool H5DataSetWriter::impl::CreateGroup (const char *a_)
{
  bool rval(true);
  try
  {
    if (!OpenFile())
      throw std::exception();

    // see if the group exists
    hid_t grp(-1);
    grp = H5Gopen(m_fid, a_);
    if (grp < 0)
    {
      // create the group
      grp = H5Gcreate(m_fid, a_, 0);
      if (grp > -1)
        H5Gclose(grp);
      else
        rval = false;
    }
  }
  catch (std::exception &)
  {
    rval = false;
  }
  return rval;
} // H5DataSetWriter::impl::CreateGroup


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#ifdef CXX_TEST

#include <private/H5DataReader/H5DataSetWriter.t.h>
static H5DataSetWriterSetup GetSetupTester()
{
  H5DataSetWriterSetup m_setup;
  if (m_setup.m_file.empty())
  {
    CStr file;
    util::GetTempDirectory(m_setup.m_file);
    m_setup.m_file += "\\h5writer.h5";
    m_setup.m_path = "newDS";
  }
  return m_setup;
}

//------------------------------------------------------------------------------
void H5DataSetWriterT::tearDown ()
{
  CloseAllOpenH5Files();
  FILE *fp(fopen(GetSetupTester().m_file, "r"));
  if (fp)
  {
    fclose(fp);
    TS_ASSERT(!remove(GetSetupTester().m_file));
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testCreateClass ()
{
  H5DataSetWriterSetup s(GetSetupTester());
  H5DataSetWriter *h = new H5DataSetWriter(&s);
  if (h)
  {
    delete(h);
    h = 0;
  }
  else
    TS_FAIL("alloc failed");
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testOpenH5ForWriting ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    s.m_file = "";
    H5DataSetWriter t(&s);
    TS_ASSERT(!t.m_p->OpenFile());
    ErrorStack::Get().ClearErrors();
  }
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(t.m_p->OpenFile());
    CloseAllOpenH5Files();
  }
  { // try creating it again
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(t.m_p->OpenFile());
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testDatasetExists ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(!t.m_p->DatasetExists()); // file not opened
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(!t.m_p->DatasetExists()); // no dataset
    TS_ASSERT(t.m_p->CreateDataset());
    TS_ASSERT(t.m_p->DatasetExists());
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testOpenDatasetAndCreateIfNeeded ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(!t.m_p->OpenDatasetAndCreateIfNeeded()); // file not opened
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(t.m_p->OpenDatasetAndCreateIfNeeded()); // this creates it
    TS_ASSERT(t.m_p->OpenDatasetAndCreateIfNeeded()); // this calls OpenDataset
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testOpenDataset ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(!t.m_p->OpenDataset()); // file not open
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(!t.m_p->OpenDataset()); // no data set yet
    TS_ASSERT(t.m_p->CreateDataset()); // creates and opens it
    TS_ASSERT(t.m_p->OpenDataset()); // already open

    H5Dclose(t.m_p->m_dataId);
    t.m_p->m_dataId = -1;
    TS_ASSERT(t.m_p->OpenDataset()); // this opens an existing dataset
  }
  {
    H5DataSetWriterSetup s(GetSetupTester());
    s.m_type = H5T_NATIVE_DOUBLE;
    H5DataSetWriter t(&s);
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(!t.m_p->OpenDataset()); // data class type wrong
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testCreateDataset ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(!t.m_p->CreateDataset());
    ErrorStack::Get().ClearErrors();
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(t.m_p->CreateDataset());
  }
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(!t.m_p->CreateDataset()); // dataset exists
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testAllowTypeConversions ()
{
  H5DataSetWriterSetup s(GetSetupTester());
  H5DataSetWriter t(&s);
  TS_ASSERT(!t.m_p->m_allowConversions);
  t.AllowTypeConversions(true);
  TS_ASSERT(t.m_p->m_allowConversions);
  t.AllowTypeConversions(false);
  TS_ASSERT(!t.m_p->m_allowConversions);
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testOpenDataspace ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(!t.m_p->OpenDataspace()); // file not open
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(t.m_p->CreateDataset());
    TS_ASSERT(t.m_p->OpenDataspace()); // should work
    TS_ASSERT(t.m_p->OpenDataspace()); // should work, it is already open
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testDoErrorChecks ()
{
  {
    double d[2] = {1,2};
    size_t s(2);
    H5DataSetWriterSetup su(GetSetupTester());
    H5DataSetWriter t(&su);
    TS_ASSERT_THROWS(t.m_p->DoErrorChecksT(d, s), std::exception&);
    t.AllowTypeConversions(true);
    t.m_p->m_dimInfo.m_startLoc.push_back(0);
    TS_ASSERT_THROWS(t.m_p->DoErrorChecksT(d, s), std::exception&);
    t.m_p->m_dimInfo.m_nToWrite.push_back(3);
    t.m_p->m_setup.m_nDim = 2;
    TS_ASSERT_THROWS(t.m_p->DoErrorChecksT(d, s), std::exception&);
    t.m_p->m_setup.m_nDim = 1;
    TS_ASSERT_THROWS(t.m_p->DoErrorChecksT(d, s), std::exception&);
    t.m_p->m_dimInfo.m_nToWrite[0] = 2;
    TS_ASSERT(t.m_p->DoErrorChecksT(d, s));
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testDataClassTypeMatches ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(t.m_p->CreateDataset());
  }
  {
    H5DataSetWriterSetup s(GetSetupTester());
    s.m_type = H5T_NATIVE_CHAR;
    H5DataSetWriter t(&s);
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(!t.m_p->DataClassTypeMatches()); // dataset not open
    TS_ASSERT(t.m_p->OpenDataset()); // this calls DataClassTypeMatches
  }
  {
    H5DataSetWriterSetup s(GetSetupTester());
    s.m_type = H5T_NATIVE_DOUBLE;
    H5DataSetWriter t(&s);
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(!t.m_p->OpenDataset()); // this calls DataClassTypeMatches
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testDataDimensionsMatch ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(!t.m_p->DataDimensionsMatch()); // file not open
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(!t.m_p->DataDimensionsMatch()); // no dataset
    TS_ASSERT(t.m_p->CreateDataset());
    TS_ASSERT(!t.m_p->DataDimensionsMatch()); // dataspace not open
    TS_ASSERT(t.m_p->OpenDataspace()); // this does the check
    TS_ASSERT(t.m_p->DataDimensionsMatch()); // call it again
  }
  {
    H5DataSetWriterSetup s(GetSetupTester());
    s.m_nDim = 2;
    H5DataSetWriter t(&s);
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(t.m_p->OpenDataset());
    TS_ASSERT(!t.m_p->OpenDataspace()); // this does the check
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testSetDimInfoForWriting ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    std::vector<hsize_t> start(2, 0), size(2, 3);
    H5DSWriterDimInfo i(start, size);
    t.SetDimInfoForWriting(&i);
    TS_ASSERT(t.m_p->m_dimInfo.m_startLoc.size() == 2);
    TS_ASSERT(t.m_p->m_dimInfo.m_nToWrite.size() == 2);
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testCheckDimensionsAndExtendDataIfNeeded ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(!t.m_p->CheckDimensionsAndExtendDataIfNeeded(10)); // no file
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(!t.m_p->CheckDimensionsAndExtendDataIfNeeded(10)); // no data
    TS_ASSERT(t.m_p->CreateDataset());
    TS_ASSERT(!t.m_p->CheckDimensionsAndExtendDataIfNeeded(10)); // no space
    TS_ASSERT(t.m_p->OpenDataspace());
    t.m_p->m_setup.m_nDim = 2;
    TS_ASSERT_THROWS(t.m_p->CheckDimensionsAndExtendDataIfNeeded(10),
                     std::exception&);
    t.m_p->m_setup.m_nDim = 1;
    TS_ASSERT(t.m_p->CheckDimensionsAndExtendDataIfNeeded(10));
  }
  CloseAllOpenH5Files();
  remove(GetSetupTester().m_file);
  {
    H5DataSetWriterSetup s(GetSetupTester());
    s.m_nDim = 2;
    H5DataSetWriter t(&s);
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(t.m_p->CreateDataset());
    TS_ASSERT(t.m_p->OpenDataspace());
    TS_ASSERT_THROWS(t.m_p->CheckDimensionsAndExtendDataIfNeeded(25),
                     std::exception&);
    std::vector<hsize_t> start(2, 1), n2write(2, 5);
    H5DSWriterDimInfo in(start, n2write);
    t.SetDimInfoForWriting(&in);
    TS_ASSERT(t.m_p->CheckDimensionsAndExtendDataIfNeeded(25));
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testCreateFilespaceAndMemoryspace ()
{
  {
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(!t.m_p->CreateFilespaceAndMemoryspace()); // no file
    TS_ASSERT(t.m_p->OpenFile());
    TS_ASSERT(!t.m_p->CreateFilespaceAndMemoryspace()); // no data
    TS_ASSERT(t.m_p->CreateDataset());
    TS_ASSERT(!t.m_p->CreateFilespaceAndMemoryspace()); // no space
    TS_ASSERT(t.m_p->OpenDataspace());
    t.m_p->m_dimInfo.m_startLoc.push_back(0);
    t.m_p->m_dimInfo.m_nToWrite.push_back(2);
    TS_ASSERT(t.m_p->CreateFilespaceAndMemoryspace());
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testWriteDataT ()
{
  {
    double d[2] = {1,2};
    H5DataSetWriterSetup s(GetSetupTester());
    H5DataSetWriter t(&s);
    TS_ASSERT(!t.WriteData(d, 2));
    t.AllowTypeConversions(true);
    TS_ASSERT(t.WriteData(d, 2));
  }
  CloseAllOpenH5Files();
  remove(GetSetupTester().m_file);
  { // multidim write
    double d[4] = {1,2,3,4};
    H5DataSetWriterSetup s(GetSetupTester());
    s.m_nDim = 2;
    H5DataSetWriter t(&s);
    std::vector<hsize_t> start(2,0), n2write(2, 2);
    H5DSWriterDimInfo dim(start, n2write);
    t.SetDimInfoForWriting(&dim);
    TS_ASSERT(!t.WriteData(d, 3));
    t.AllowTypeConversions(true);
    TS_ASSERT(!t.WriteData(d, 3));
    TS_ASSERT(t.WriteData(d, 4));
  }
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testCreateGroup ()
{
  H5DataSetWriterSetup s(GetSetupTester());
  H5DataSetWriter t(&s);
  t.CreateGroup("aGroup");
  hid_t grp(H5Gopen(t.m_p->m_fid, "aGroup"));
  TS_ASSERT(grp > -1);
  H5Gclose(grp);
}
//------------------------------------------------------------------------------
void H5DataSetWriterT::testWriteAtt ()
{
  const char *name = "attributeName";
  H5DataSetWriterSetup s(GetSetupTester());
  H5DataSetWriter t(&s);
  TS_ASSERT(!t.WriteAtt(name, 2));
  TS_ASSERT(t.m_p->OpenFile());
  TS_ASSERT(t.m_p->CreateDataset());
  TS_ASSERT(t.WriteAtt(name, 3));

  hid_t attId = H5Aopen_name(t.m_p->m_dataId, name);
  TS_ASSERT(attId > 0);
  int value(0);
  TS_ASSERT(!H5Aread(attId, H5T_NATIVE_INT, &value));
  TS_ASSERT_EQUALS(3, value);
  if (attId > 0)
    H5Aclose(attId);
}

#endif

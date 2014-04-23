//------------------------------------------------------------------------------
// FILE      H5DataSetReader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\H5DataReader\H5DataSetReader.h>

#include <map>

#include <private\H5DataReader\H5DataReaderUtil.h>
#include <private\H5DataReader\H5DataSetWriter.h>
#include <private\util\EReadAsciiFile.h>

////////////////////////////////////////////////////////////////////////////////
/// \class H5DataSetReaderImpl
////////////////////////////////////////////////////////////////////////////////
class H5DataSetReaderImpl
{
public:
  H5DataSetReaderImpl(const CStr &a_file,
                      const CStr &a_path,
                      const VEC_INT_PAIR &a_indices);
  ~H5DataSetReaderImpl();

  bool Throw(const CStr &a_) const;
  void DontThrow() { m_noThrow = true; }

  H5T_class_t GetDataSetClass();
  size_t      GetDataSetTypeSize();

  void CleanUp();

  bool OpenH5DataSet();
  bool OpenFile();
  bool OpenDataSet();
  bool OpenDataSpace();

  bool SelectHyperSlab();
  bool CheckDataSetType(const H5T_class_t &a_,
                        const size_t &a_size);
  bool CreateMemorySpace(const hsize_t &a_size);
  bool CheckDataSetDimensions() const;
  bool CheckArraySize(const size_t &a_size) const;

  template <class T>
  bool GetDataT(T *a_, size_t a_size);
  template <class T>
  bool ReadDataT(T *a_, size_t a_size);
  template <class T>
  bool GetDataVec(T &a_);
  template <class T>
  bool GetAllDataT(T &a_);
  template <class T>
  bool CheckForErrorsBeforeReadingData(T *a_, const size_t &a_size);

  void AllowTypeConversions(const bool a_) { m_allowTypeConversions = a_; }

  bool DataSetExists();
  bool GetDataSetDimensions(std::vector<hsize_t> &a_size);
  bool GetAtt(const char * const a_str, int &a_i);

private:
  H5DataSetReaderImpl();

  bool   m_noThrow, m_allowTypeConversions;
  CStr   m_file, m_path;
  hid_t  m_fileId, m_dataSetId, m_dataSpaceId, m_memSpaceId;
  VEC_INT_PAIR m_indices;
};

////////////////////////////////////////////////////////////////////////////////
/// \class H5FileManager
////////////////////////////////////////////////////////////////////////////////
class H5FileManager
{
public:
  H5FileManager();
  ~H5FileManager();

  bool OpenFile(const char* const a_,
                hid_t &a_fId,
                const H5DataSetReaderImpl &a_impl);
  void CloseAllFiles();
  void SetPath(const CStr& a_path) { m_path = a_path; }

private:
  std::map<CStr, hid_t> m_files;
  CStr m_path;
};

//------------------------------------------------------------------------------
/// \brief Gets a file manager
//------------------------------------------------------------------------------
//lint --e{1929} we intend to return a reference here
static H5FileManager& GetFileManager()
{
  static H5FileManager m_; // ok to leave static
  return m_;
} // GetFileManger
//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
H5FileManager::H5FileManager () :
m_files()
{
} // H5FileManager::H5FileManager
//------------------------------------------------------------------------------
/// \brief Destructor.
//------------------------------------------------------------------------------
H5FileManager::~H5FileManager ()
{
  try
  {
    // The following shouldn't be called since the H5 library has an atexit
    // function that closes all the files.  It could get called before this,
    // and if so it will spit out an error when CloseAllFiles gets called.
    // Instead could add back in the call and call H5dont_atexit() but it seems
    // more reliable to comment it out.
    //CloseAllFiles();
  }
  catch (...) {}
}// H5FileManager::~H5FileManager
//------------------------------------------------------------------------------
/// \brief Closes all of the files
//------------------------------------------------------------------------------
void H5FileManager::CloseAllFiles ()
{
  std::map<CStr, hid_t>::iterator it;

  for (it=m_files.begin(); it!=m_files.end(); it++)
  {
    if (H5Fclose(it->second) < 0)
    {
      CStr msg;
      msg.Format("There was a problem closing the HDF5 file: \n%s", it->first);
      ErrorStack::Get().PutError(msg);
    }
  }
  m_files.clear();
} // H5FileManager::CloseAllFiles
//------------------------------------------------------------------------------
/// \brief Opens a file and checks if it is an hdf5 file.
//------------------------------------------------------------------------------
bool H5FileManager::OpenFile (const char* const a_,
                              hid_t &a_fId,
                              const H5DataSetReaderImpl &a_impl)
{
  a_fId = -1;
  CStr file;
  file = m_path;
  file += a_;
  if (file.IsEmpty())
    return a_impl.Throw("Unable to open file. No file specified.");
  {
    FILE *fp = fopen(file.c_str(), "r");
    if (fp)
      fclose(fp);
    else
      return a_impl.Throw("Can not open file: " + file + ".");
  }

  std::map<CStr, hid_t>::iterator it;
  it = m_files.find(file);
  if (it != m_files.end())
  {
    a_fId = it->second;
    return true;
  }
  a_fId = H5DataReader::GetFileIdIfOpen(file);
  if (a_fId > -1)
    return true;

  htri_t isHdf5(false);
  // make sure file is an HDF5 file
  isHdf5 = H5Fis_hdf5(file);
  if (isHdf5 <= 0) {
    return a_impl.Throw("File: " + file + " is not an HDF5 file.");
  }

  a_fId = H5Fopen(file, H5F_ACC_RDONLY, H5P_DEFAULT);
  if (a_fId < 0)
    return a_impl.Throw("Unable to open " + file + ".");

  m_files.insert(std::make_pair(file, a_fId));
  return true;
} // H5FileManager::OpenFile

//------------------------------------------------------------------------------
/// \brief Closes all files managed by the file manager
//------------------------------------------------------------------------------
void H5Reader::CloseAllH5Files ()
{
  GetFileManager().CloseAllFiles();
} // H5Reader::CloseAllH5Files
//------------------------------------------------------------------------------
/// \brief Sets a path where any opened h5 file should be.
/// The string should look like this when it comes in: "" or "..\up\"
//------------------------------------------------------------------------------
void H5Reader::SetH5FilePath (const CStr& a_path)
{
  GetFileManager().SetPath(a_path);
} // H5Reader::SetH5FilePath
//------------------------------------------------------------------------------
/// \brief Closes all files managed by the file manager
//------------------------------------------------------------------------------
hid_t H5Reader::OpenFile (const char * const a_fname)
{
  hid_t fid(-1);
  CStr file, path;
  VEC_INT_PAIR indices;
  H5DataSetReaderImpl r(file, path, indices);
  r.DontThrow();
  GetFileManager().OpenFile(a_fname, fid, r);
  return(fid);
} // H5Reader::OpenFile

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
H5DataSetReaderImpl::H5DataSetReaderImpl (const CStr &a_file,
                                          const CStr &a_path,
                                          const VEC_INT_PAIR &a_indices) :
m_noThrow(false),
m_allowTypeConversions(false),
m_file(a_file),
m_path(a_path),
m_fileId(-1),
m_dataSetId(-1),
m_dataSpaceId(-1),
m_memSpaceId(-1),
m_indices(a_indices)
{
} // H5DataSetReaderImpl::H5DataSetReaderImpl
//------------------------------------------------------------------------------
/// \brief Destructor.
//------------------------------------------------------------------------------
H5DataSetReaderImpl::~H5DataSetReaderImpl ()
{
  try
  {
    CleanUp();
  }
  catch (...) {}
} // H5DataSetReaderImpl::~H5DataSetReaderImpl
//------------------------------------------------------------------------------
/// \brief Cleans up the class
//------------------------------------------------------------------------------
void H5DataSetReaderImpl::CleanUp ()
{
  //lint -e{534}
  if (m_memSpaceId > -1)
    H5Sclose(m_memSpaceId);
  if (m_dataSpaceId > -1)
    H5Sclose(m_dataSpaceId);
  if (m_dataSetId > -1)
    H5Dclose(m_dataSetId);
  m_fileId = m_dataSetId = m_dataSpaceId = m_memSpaceId = -1;
} // H5DataSetReaderImpl::CleanUp
//------------------------------------------------------------------------------
/// \brief Throws an exception if throwing is ok for this instance
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::Throw (const CStr &a_) const
{
  if (m_noThrow)
    return false;
  throw EException(a_);
} // H5DataSetReaderImpl::Throw 
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in vector
//------------------------------------------------------------------------------
H5T_class_t H5DataSetReaderImpl::GetDataSetClass ()
{
  H5T_class_t t(H5T_NO_CLASS);
  if (!OpenH5DataSet())
    return t;
  const hid_t dataTypeId(H5Dget_type(m_dataSetId));
  if (dataTypeId < 1)
    return t;
  t = H5Tget_class(dataTypeId);
  H5Tclose(dataTypeId);
  return t;
} // H5DataSetReaderImpl::GetDataSetClass
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in vector
//------------------------------------------------------------------------------
size_t H5DataSetReaderImpl::GetDataSetTypeSize ()
{
  size_t t(0);
  if (!OpenH5DataSet())
    return t;
  const hid_t dataTypeId(H5Dget_type(m_dataSetId));
  if (dataTypeId < 1)
    return t;
  t = H5Tget_size(dataTypeId);
  H5Tclose(dataTypeId);
  return t;
} // H5DataSetReaderImpl::GetDataSetClass
//------------------------------------------------------------------------------
/// \brief Opens all of the necessary items to read an HDF data set
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::OpenH5DataSet ()
{
  return ( OpenFile() &&
           OpenDataSet() &&
           OpenDataSpace() );
} // H5DataSetReaderImpl::OpenH5DataSet
//------------------------------------------------------------------------------
/// \brief Opens an HDF5 file
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::OpenFile ()
{
  if (m_fileId > 0)
    return true;
  return GetFileManager().OpenFile(m_file, m_fileId, *this);
} // H5DataSetReaderImpl::OpenFile
//------------------------------------------------------------------------------
/// \brief Opens an HDF5 data set in an open file
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::OpenDataSet ()
{
  if (m_dataSetId > 0)
    return true;

  if (m_fileId < 0)
    return Throw("Can not open data set the file: " + m_file + " is not open.");

  m_dataSetId = H5Dopen(m_fileId, m_path);
  if (m_dataSetId < 0)
    return Throw("Error opening data set: " + m_path + ".");
  return true;
} // H5DataSetReaderImpl::OpenDataSet
//------------------------------------------------------------------------------
/// \brief Opens an HDF5 data space in an open data set
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::OpenDataSpace ()
{
  if (m_dataSpaceId > 0)
    return true;

  if (m_dataSetId < 0)
    return Throw("Unable to open data space because data set: " + m_path + " is not open.");

  m_dataSpaceId = H5Dget_space(m_dataSetId);
  if (m_dataSpaceId < 0)
    return Throw("Unable to open data space for data set " + m_path + ".");
  return true;
} // H5DataSetReaderImpl::OpenDataSpace
//------------------------------------------------------------------------------
/// \brief Checks the type of data that is stored in the data set.
/// If we are allowing type conversions then the H5T_class_t must be
/// either H5T_FLOAT or H5T_INTEGER.
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::CheckDataSetType (const H5T_class_t &a_,
                                            const size_t &a_size)
{
  bool  retval(false);
  const H5T_class_t t(GetDataSetClass());
  if (m_allowTypeConversions)
  {
    if (t == H5T_FLOAT || t == H5T_INTEGER)
      retval = true;
  }
  else
  {
    const size_t size(GetDataSetTypeSize());
    if (t == a_ &&
        size == a_size)
      retval = true;
  }

  if (!retval)
    return Throw("Wrong type of data for data set " + m_path + ".");
  return retval;
} // H5DataSetReaderImpl::CheckDataSetType
//------------------------------------------------------------------------------
/// \brief Checks the dimensions of the data set
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::CheckDataSetDimensions () const
{
  // check to make sure we are not going to try to read outside
  // the bounds of the data
  std::vector<hsize_t> dims, maxDims;
  if (!H5DataReader::GetDataSetDimensions(m_dataSpaceId, dims, maxDims))
    return false;

  if (dims.size() != m_indices.size())
    return Throw("The number of dimension for data set: " + m_path + " are wrong");

  size_t i;
  for (i=0; i<m_indices.size(); i++)
  {
    const int ib(m_indices.at(i).first + m_indices.at(i).second);
    const hsize_t b(static_cast<hsize_t>(static_cast<hssize_t>(ib)));
    if (b > dims.at(i))
      return Throw("Attempting to read outside the valid bounds of data set: " + m_path + ".");
  }

  return true;
} // H5DataSetReaderImpl::CheckDataSetDimensions
//------------------------------------------------------------------------------
/// \brief check the size of the data that we are going to read the data into
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::CheckArraySize (const size_t &a_size) const
{
  // compute the size from the indices variable
  size_t i, numToRead(0);

  if (!m_indices.empty())
    numToRead = static_cast<size_t>(m_indices.front().second);
  for (i=1; i<m_indices.size(); i++)
  {
    numToRead *= m_indices.at(i).second;
  }

  if (a_size != numToRead)
    return Throw("The size of the array is inconsistent with the user input indices.");
  return true;
} // H5DataSetReaderImpl::CheckArraySize
//------------------------------------------------------------------------------
/// \brief Gets the number of dimensions for the data set.
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::GetDataSetDimensions (std::vector<hsize_t> &a_size)
{
  bool retval(true);

  try
  {
    OpenH5DataSet();

    std::vector<hsize_t> maxDims;
    if (!H5DataReader::GetDataSetDimensions(m_dataSpaceId, a_size, maxDims))
      throw EException();
  }
  catch (const EException &e)
  {
    retval = false;
    CStr msg(e.what());
    if (!msg.IsEmpty())
      ErrorStack::Get().PutError(msg);
  }

  return retval;
} // H5DataSetReaderImpl::GetNumDimensions
//------------------------------------------------------------------------------
/// \brief See if the data set exists
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::DataSetExists ()
{
  bool rval(false);
  m_noThrow = true;
  if (OpenFile() &&
      OpenDataSet())
    rval = true;
  m_noThrow = false;
  return rval;
} // H5DataSetReaderImpl::DataSetExists
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in array
//------------------------------------------------------------------------------
template <class T>
bool H5DataSetReaderImpl::GetDataVec (T &a_)
{
  int size(0);
  if (!m_indices.empty())
    size = m_indices.front().second;
  for (size_t i=1; i<m_indices.size(); i++)
    size *= m_indices.at(i).second;
  if (size < 1)
  {
    a_.clear();
    return false;
  }
  a_.assign(size, 0);
  return(GetDataT(&a_[0], a_.size()));
} // H5DataSetReaderImpl::GetDataVec
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in array
//------------------------------------------------------------------------------
template <class T>
bool H5DataSetReaderImpl::GetDataT (T *a_,
                                      size_t a_size)
{
  bool retval(true);
  try
  {
    if ( !OpenH5DataSet() ||
         !CheckForErrorsBeforeReadingData(a_, a_size) ||
         !ReadDataT<T>(a_, a_size) )
     throw EException("");
  }
  catch (EException &e)
  {
    retval = false;
    CStr msg(e.what());
    if (!msg.IsEmpty())
      ErrorStack::Get().PutError(msg);
  }
  CleanUp();
  return retval;
} // H5DataSetReader::GetData
//------------------------------------------------------------------------------
/// \brief Checks for various errors before we attempt to actually read the
/// data
//------------------------------------------------------------------------------
template <class T>
bool H5DataSetReaderImpl::CheckForErrorsBeforeReadingData (T *a_,
                                                             const size_t &a_size)
{
  return ( CheckDataSetType(H5DataReader::Get_H5T_class_t(a_), sizeof(T)) &&
           CheckDataSetDimensions() &&
           CheckArraySize(a_size) &&
           SelectHyperSlab() );
} // H5DataSetReaderImpl::CheckForErrorsBeforeReadingData
//------------------------------------------------------------------------------
/// \brief Opens an HDF5 file
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::SelectHyperSlab ()
{
  CStr msg("Can not select hyperslab");
  if (m_dataSpaceId < 0)
    return Throw(msg + ". Data set: " + m_path + " is not open.");

  std::vector<hsize_t> count(m_indices.size(), static_cast<hsize_t>(0));
  std::vector<hsize_t> offset(m_indices.size(), static_cast<hssize_t>(0));

  // select the hyperslab
  for (size_t i=0; i<m_indices.size(); i++)
  {
    count.at(i) = static_cast<hsize_t>(static_cast<hssize_t>(m_indices.at(i).second));
    offset.at(i) = static_cast<hssize_t>(m_indices.at(i).first);
  }

  herr_t status;
  status = H5Sselect_hyperslab(m_dataSpaceId, H5S_SELECT_SET, &offset[0],
                               NULL, &count[0], NULL);
  if (status < 0)
    return Throw(msg + " for data set: " + m_path + ".");
  // make sure the selection is valid
  if (H5Sselect_valid(m_dataSpaceId) <= 0)
    return Throw(msg + " for data set: " + m_path + ".");

  return true;
} // H5DataSetReaderImpl::SelectHyperSlab
//------------------------------------------------------------------------------
/// \brief Creates a memory space so we can read the data
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::CreateMemorySpace (const hsize_t &a_size)
{
  if (a_size < 1)
    return Throw("Can not create a memory space of size 0.");

  m_memSpaceId = H5Screate_simple(1, &a_size, NULL);
  if (m_memSpaceId < 0)
    return Throw("Error creating memory space.");
  return true;
} // H5DataSetReaderImpl::CreateMemorySpace
//------------------------------------------------------------------------------
/// \brief Reads the data from the HDF5 file
//------------------------------------------------------------------------------
template <class T>
bool H5DataSetReaderImpl::ReadDataT (T *a_, size_t a_size)
{
  CreateMemorySpace(a_size);
  if (m_dataSetId < 0 || m_memSpaceId < 0 || m_dataSpaceId < 0)
    return Throw("Unable to read data set: " + m_path + ". Data set not open.");

  try
  {
    herr_t status(0);
    H5T_class_t dClass(GetDataSetClass());
    if (dClass == H5DataReader::Get_H5T_class_t(a_))
      status = H5Dread(m_dataSetId, H5DataReader::Get_H5_nativetype(a_),
                       m_memSpaceId, m_dataSpaceId, H5P_DEFAULT, a_);
    else if (dClass == H5T_FLOAT)
    {
      std::vector<float> vFlt(a_size, 0);
      if (GetDataT(&vFlt.at(0), a_size))
      {
        for (size_t i=0; i<a_size; i++)
          a_[i] = (T)vFlt.at(i);
      }
    }
    else if (dClass == H5T_INTEGER)
    {
      std::vector<int> v(a_size, 0);
      if (GetDataT(&v.at(0), a_size))
      {
        for (size_t i=0; i<a_size; i++)
          a_[i] = (T)v.at(i);
      }
    }

    if (status < 0)
      return Throw("Error reading data set: " + m_path + ".");
  }
  catch (std::out_of_range)
  {
    return Throw("Error reading outside of array range.");
  }
  return true;
} // H5DataSetReaderImpl::ReadDataT
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in vector
//------------------------------------------------------------------------------
template <class T>
bool H5DataSetReaderImpl::GetAllDataT (T &a_)
{
  bool retval(true);
  try
  {
    OpenH5DataSet();

    std::vector<hsize_t> dims, maxDims;
    if (!H5DataReader::GetDataSetDimensions(m_dataSpaceId, dims, maxDims))
      throw EException();
    if (dims.empty())
      throw EException();

    m_indices.resize(0);
    std::pair<int, int> myPair(0,0);
    for (size_t i=0; i<dims.size(); i++)
    {
      myPair.second = (int)dims.at(i);
      m_indices.push_back(myPair);
    }

    GetDataVec<T>(a_);
  }
  catch (EException &e)
  {
    retval = false;
    CStr msg(e.what());
    if (!msg.IsEmpty())
      ErrorStack::Get().PutError(msg);
  }
  CleanUp();
  return retval;
} // H5DataSetReaderImpl::GetAllDataT
//------------------------------------------------------------------------------
/// \brief Reads the attribute associated with this data set
//------------------------------------------------------------------------------
bool H5DataSetReaderImpl::GetAtt (const char * const a_attStr, int &a_i)
{
  // We may want to refactor this class so that the data set reading and the
  // attribute reading are done by the same base class with some slight
  // specialization. We could also change this to be a template like we
  // did with read data.

  if (m_dataSetId < 0)
  {
    if (!OpenH5DataSet())
      return false;
  }

  bool retval(true);

  CStr path(a_attStr);
  if (path.IsEmpty())
    return Throw("Unable to get data set attribute because no attribute was specified.");

  CStr msg;
  msg.Format("Error attempting to read attribute: %s. ", path.c_str());

  hid_t attId(-1), dataSpaceId(-1), dataTypeId(-1);
  try
  {
    // open the attribute
    attId = H5Aopen_name(m_dataSetId, path.c_str());
    if (attId < 0)
      throw EException(msg + "Unable to open attribute.");

    // check the data type
    dataTypeId = H5Aget_type(attId);
    if (dataTypeId < 0)
      throw EException(msg + "Unable to get data type.");

    if (H5Tget_class(dataTypeId) != H5DataReader::Get_H5T_class_t(&a_i))
      throw EException(msg + "Wrong type of data requested.");

    // get the dataspace
    dataSpaceId = H5Aget_space(attId);
    if (dataSpaceId < 0)
      throw EException(msg + "Unable to get data space.");

    // check the data space
    if (!H5Sis_simple(dataSpaceId))
      throw EException(msg + "Data space is not simple.");
    std::vector<hsize_t> dims, maxDims;
    if (!H5DataReader::GetDataSetDimensions(dataSpaceId, dims, maxDims) ||
        dims.empty() ||
        dims.front() != 1)
      throw EException(msg + "Unable to get data space dimensions.");

    if (H5Aread(attId, H5DataReader::Get_H5_nativetype(&a_i), &a_i))
      throw EException(msg);

  }
  catch (const EException &e)
  {
    CStr err(e.what());
    if (!err.IsEmpty())
    {
      ErrorStack::Get().PutError(err);
    }
    retval = false;
  }

  if (dataTypeId > 0)
    H5Tclose(dataTypeId);
  if (dataSpaceId > 0)
    H5Sclose(dataSpaceId);
  if (attId > 0)
    H5Aclose(attId);

  return retval;
} // H5DataSetReaderImpl::GetAttInt



//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
H5DataSetReader::H5DataSetReader (const CStr &a_file,
                                  const CStr &a_path,
                                  const VEC_INT_PAIR &a_indices) :
m_p(new H5DataSetReaderImpl(a_file, a_path, a_indices))
{
} // H5DataSetReader::H5DataSetReader
//------------------------------------------------------------------------------
/// \brief Destructor.
//------------------------------------------------------------------------------
H5DataSetReader::~H5DataSetReader ()
{
  try
  {
    if (m_p) delete(m_p);
  }
  catch (...) {}
} // H5DataSetReader::~H5DataSetReader
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in array
//------------------------------------------------------------------------------
bool H5DataSetReader::GetData (double *a_, size_t a_size)
{
  return (m_p->GetDataT(a_, a_size));
} // H5DataSetReader::GetData
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in array
//------------------------------------------------------------------------------
bool H5DataSetReader::GetData (float *a_, size_t a_size)
{
  return (m_p->GetDataT(a_, a_size));
} // H5DataSetReader::GetData
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in array
//------------------------------------------------------------------------------
bool H5DataSetReader::GetData (int *a_, size_t a_size)
{
  return (m_p->GetDataT(a_, a_size));
} // H5DataSetReader::GetData
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in array
//------------------------------------------------------------------------------
bool H5DataSetReader::GetData (char *a_, size_t a_size)
{
  return (m_p->GetDataT(a_, a_size));
} // H5DataSetReader::GetData
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in vector
//------------------------------------------------------------------------------
bool H5DataSetReader::GetData (std::vector<double> &a_)
{
  return (m_p->GetDataVec(a_));
} // H5DataSetReader::GetData
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in vector
//------------------------------------------------------------------------------
bool H5DataSetReader::GetData (std::vector<float> &a_)
{
  return (m_p->GetDataVec(a_));
} // H5DataSetReader::GetData
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in vector
//------------------------------------------------------------------------------
bool H5DataSetReader::GetData (std::vector<int> &a_)
{
  return (m_p->GetDataVec(a_));
} // H5DataSetReader::GetData
//------------------------------------------------------------------------------
/// \brief Reads the attribute associated with this data set
//------------------------------------------------------------------------------
bool H5DataSetReader::GetAtt (const char * const a_attStr, int &a_i)
{
  return (m_p->GetAtt(a_attStr, a_i));
} // H5DataSetReader::GetAttInt
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in vector
//------------------------------------------------------------------------------
bool H5DataSetReader::GetAllData (std::vector<double> &a_)
{
  return(m_p->GetAllDataT(a_));
} // H5DataSetReader::GetAllData
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in vector
//------------------------------------------------------------------------------
bool H5DataSetReader::GetAllData (std::vector<float> &a_)
{
  return(m_p->GetAllDataT(a_));
} // H5DataSetReader::GetAllData
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in vector
//------------------------------------------------------------------------------
bool H5DataSetReader::GetAllData (std::vector<char> &a_)
{
  return(m_p->GetAllDataT(a_));
} // H5DataSetReader::GetAllData
//------------------------------------------------------------------------------
/// \brief Puts the data from the HDF5 file into the passed in vector
//------------------------------------------------------------------------------
bool H5DataSetReader::GetAllData (std::vector<int> &a_)
{
  return(m_p->GetAllDataT(a_));
} // H5DataSetReader::GetAllData
//------------------------------------------------------------------------------
/// \brief This makes it so whatever data is read it will be cast to the type
/// that the user has requested.
//------------------------------------------------------------------------------
void H5DataSetReader::AllowTypeConversions (bool a_)
{
  m_p->AllowTypeConversions(a_);
} // H5DataSetReader::AllowTypeConversions
//------------------------------------------------------------------------------
/// \brief See if the data set exists
//------------------------------------------------------------------------------
bool H5DataSetReader::DataSetExists ()
{
  return(m_p->DataSetExists());
} // H5DataSetReader::DataSetExists
//------------------------------------------------------------------------------
/// \brief Gets the dimensions of this data set
//------------------------------------------------------------------------------
bool H5DataSetReader::GetDataSetDimensions (std::vector<hsize_t> &a_size)
{
  return(m_p->GetDataSetDimensions(a_size));
} // H5DataSetReader::GetDataSetDimensions


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#ifdef CXX_TEST

#include <private\H5DataReader\H5DataSetReader.t.h>

#include <private\H5DataReader\H5DataSetReader.h>
#include <private\H5DataReader\H5DataReaderUtil.h>
#include <private\util\util.h>

//------------------------------------------------------------------------------
void H5DataSetReaderT::setUp ()
{
  H5Initialize::Init();
  m_file.Format("%s\\HDF5_InputFiles\\input.h5",
                util::GetTestFilesDirectory().c_str());
  m_file2.Format("%s\\HDF5_InputFiles\\smallGrid_Trans.h5",
                 util::GetTestFilesDirectory().c_str());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::tearDown ()
{
  if (m_p)
    delete(m_p);
  m_p = NULL;
  if (m_p1)
    delete(m_p1);
  m_p1 = NULL;
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::Create (const CStr &a_file,
                               const CStr &a_path,
                               const std::vector<std::pair<int, int>> &a_indices)
{
  m_p = new H5DataSetReaderImpl(a_file, a_path, a_indices);
  if (m_p)
    m_p->DontThrow();
} // H5DataSetReaderT::Create
//------------------------------------------------------------------------------
void H5DataSetReaderT::Create1 (const CStr &a_file,
                                const CStr &a_path,
                                const std::vector<std::pair<int, int>> &a_indices)
{
  m_p1 = new H5DataSetReader(a_file, a_path, a_indices);
  if (m_p1)
    m_p1->m_p->DontThrow();
} // H5DataSetReaderT::Create
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCreateClass ()
{
  VEC_INT_PAIR indices;
  Create("file", "path", indices);

  TS_ASSERT(m_p);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenFile_FileExists ()
{
  VEC_INT_PAIR indices;
  Create(m_file, "path", indices);

  TS_ASSERT(m_p->OpenFile());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenFile_FileDoesntExist ()
{
  VEC_INT_PAIR indices;
  CStr file;
  file = "crap";
  Create(file, "path", indices);
  TS_ASSERT(!m_p->OpenFile());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenDataSet_FileNotOpen ()
{
  VEC_INT_PAIR indices;
  Create(m_file, "path", indices);

  TS_ASSERT(!m_p->OpenDataSet())
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenDataSet_DSExists()
{
  VEC_INT_PAIR indices;
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenDataSet_DSDoesntExist()
{
  VEC_INT_PAIR indices;
  Create(m_file, "stuff", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(!m_p->OpenDataSet());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenDataSpace_DSNotOpen()
{
  VEC_INT_PAIR indices;
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  //TS_ASSERT(d.OpenDataSet());
  TS_ASSERT(!m_p->OpenDataSpace());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenDataSpace_DSOpen()
{
  VEC_INT_PAIR indices;
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCheckDataType()
{
  VEC_INT_PAIR indices;
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());

  TS_ASSERT(m_p->CheckDataSetType(H5T_FLOAT, sizeof(double)));

  TS_ASSERT(!m_p->CheckDataSetType(H5T_FLOAT, sizeof(float)));
  TS_ASSERT(!m_p->CheckDataSetType(H5T_INTEGER, sizeof(double)));

  // these should all work with type conversions
  m_p->AllowTypeConversions(true);
  TS_ASSERT(m_p->CheckDataSetType(H5T_FLOAT, sizeof(double)));
  TS_ASSERT(m_p->CheckDataSetType(H5T_FLOAT, sizeof(float)));
  TS_ASSERT(m_p->CheckDataSetType(H5T_INTEGER, sizeof(int)));
  TS_ASSERT(m_p->CheckDataSetType(H5T_INTEGER, sizeof(char)));
  // these should now fail
  m_p->AllowTypeConversions(false);
  TS_ASSERT(!m_p->CheckDataSetType(H5T_FLOAT, sizeof(float)));
  TS_ASSERT(!m_p->CheckDataSetType(H5T_INTEGER, sizeof(int)));
  TS_ASSERT(!m_p->CheckDataSetType(H5T_INTEGER, sizeof(char)));
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCheckDimensions1D ()
{
  std::pair<int, int> myPair;
  VEC_INT_PAIR indices(1, myPair);
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->CheckDataSetDimensions());
  // the number of dimensions will be wrong here
  delete(m_p);
  indices.clear();
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
  // we are trying to read outside the range here
  delete(m_p);
  myPair.first = 101;
  myPair.second = 27;
  indices.push_back(myPair);
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCheckDimensions2D ()
{
  std::pair<int, int> myPair(1, 1);
  VEC_INT_PAIR indices(2, myPair);
  Create(m_file, "2D_Data_Sets/Double2X50", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->CheckDataSetDimensions());
  delete(m_p);
  indices.clear();
  Create(m_file, "2D_Data_Sets/Double2X50", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
  // we are trying to read outside the range here
  delete(m_p);
  myPair.first = 0;
  myPair.second = 3;
  indices.push_back(myPair);
  indices.push_back(myPair);
  Create(m_file, "2D_Data_Sets/Double2X50", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCheckDimensions3D ()
{
  std::pair<int, int> myPair;
  myPair.first = 1;
  myPair.second = 1;
  VEC_INT_PAIR indices(3, myPair);
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->CheckDataSetDimensions());
  delete(m_p);
  indices.clear();
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
  // we are trying to read outside the range here
  delete(m_p);
  myPair.first = 0;
  myPair.second = 3;
  indices.push_back(myPair);
  indices.push_back(myPair);
  myPair.first = 25;
  myPair.second = 6;
  indices.push_back(myPair);
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCheckArraySize ()
{
  std::pair<int, int> myPair;
  myPair.second = 2;
  VEC_INT_PAIR indices(3, myPair);
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);

  TS_ASSERT(m_p->CheckArraySize(8));
  TS_ASSERT(!m_p->CheckArraySize(1));
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testSelectHyperslab ()
{
  std::pair<int, int> myPair;
  myPair.second = 4;
  VEC_INT_PAIR indices(3, myPair);
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);

  TS_ASSERT(!m_p->SelectHyperSlab());

  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  // we tried to select beyond the data bounds
  TS_ASSERT(!m_p->SelectHyperSlab());
  
  // this should be a valid select
  delete(m_p);
  indices.at(0).second = 3;
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->SelectHyperSlab());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCreateMemorySpace ()
{
  VEC_INT_PAIR indices;
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);

  TS_ASSERT(!m_p->CreateMemorySpace(0));

  delete(m_p);
  std::pair<int, int> myPair;
  myPair.second = 3;
  indices.assign(3, myPair);
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->CreateMemorySpace(27));
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testReadData1D ()
{
  std::pair<int, int> myPair(0, 127);
  VEC_INT_PAIR indices(1, myPair);
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->SelectHyperSlab());
  TS_ASSERT(m_p->CreateMemorySpace(127));
  std::vector<double> data(127, 0.0);
  TS_ASSERT(m_p->ReadDataT(&data[0], 127));
  TS_ASSERT_EQUALS(data.at(18), 19.0);
  TS_ASSERT_EQUALS(data.at(126), 127.0);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testReadData2D ()
{
  std::pair<int, int> myPair(1, 1);
  VEC_INT_PAIR indices(2, myPair);
  indices.at(1).second = 49;
  Create(m_file, "2D_Data_Sets/Double2X50", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->SelectHyperSlab());
  TS_ASSERT(m_p->CreateMemorySpace(49));
  std::vector<double> data(49, 0.0);
  TS_ASSERT(m_p->ReadDataT(&data[0], 49));
  TS_ASSERT_EQUALS(data.at(17), 76.0);
  TS_ASSERT_EQUALS(data.at(48), 200.0);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testReadData3D ()
{
  std::pair<int, int> myPair;
  myPair.first = 1;
  myPair.second = 1;
  VEC_INT_PAIR indices(3, myPair);
  indices.at(1).second = 9;
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->SelectHyperSlab());
  TS_ASSERT(m_p->CreateMemorySpace(9));
  std::vector<double> data(9, 0.0);
  TS_ASSERT(m_p->ReadDataT(&data[0], 9));
  TS_ASSERT_EQUALS(data.at(2), 40.0);
  TS_ASSERT_EQUALS(data.at(8), 100.0);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testReadDataVecChar ()
{
  VEC_INT_PAIR indices;
  Create1(m_file2, "General Head/04. Map ID", indices);
  std::vector<char> vChar;
  m_p1->GetAllData(vChar);
  TS_ASSERT_EQUALS((int)vChar.size(), (int)684);
  TS_ASSERT_EQUALS(vChar.at(0), 98);
  TS_ASSERT_EQUALS(vChar.at(1), 99);
  TS_ASSERT_EQUALS(vChar.at(257), 97);
  TS_ASSERT_EQUALS(vChar.at(682), 53);
  TS_ASSERT_EQUALS(vChar.at(683), 0);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetAttFail ()
{
  VEC_INT_PAIR indices;
  Create(m_file2, "General Head/04. Map ID", indices);
  int maxStrLen;
  TS_ASSERT(!m_p->GetAtt("crap", maxStrLen));
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetAttInt ()
{
  VEC_INT_PAIR indices;
  Create(m_file2, "General Head/04. Map ID", indices);
  int maxStrLen;
  TS_ASSERT(m_p->GetAtt("Max. String Length", maxStrLen));
  TS_ASSERT_EQUALS(maxStrLen, 57);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGet_H5T_class_t ()
{
  char   c;
  int    i;
  float  f;
  double d;
  TS_ASSERT(H5DataReader::Get_H5T_class_t(&c) == H5T_INTEGER);
  TS_ASSERT(H5DataReader::Get_H5T_class_t(&i) == H5T_INTEGER);
  TS_ASSERT(H5DataReader::Get_H5T_class_t(&f) == H5T_FLOAT);
  TS_ASSERT(H5DataReader::Get_H5T_class_t(&d) == H5T_FLOAT);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGet_H5_nativetype ()
{
  char   c;
  int    i;
  float  f;
  double d;
  TS_ASSERT(H5DataReader::Get_H5_nativetype(&c) == H5T_NATIVE_CHAR);
  TS_ASSERT(H5DataReader::Get_H5_nativetype(&i) == H5T_NATIVE_INT);
  TS_ASSERT(H5DataReader::Get_H5_nativetype(&f) == H5T_NATIVE_FLOAT);
  TS_ASSERT(H5DataReader::Get_H5_nativetype(&d) == H5T_NATIVE_DOUBLE);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetDataVecDbl ()
{
  std::pair<int, int> myPair;
  myPair.first = 1;
  myPair.second = 1;
  VEC_INT_PAIR indices(3, myPair);
  indices.at(1).second = 9;
  Create1(m_file, "3D_Data_Sets/Double3X10X30", indices);
  std::vector<double> data;
  TS_ASSERT(m_p1->GetData(data));
  TS_ASSERT(data.size() == 9);
  TS_ASSERT_EQUALS(data.at(2), 40.0);
  TS_ASSERT_EQUALS(data.at(8), 100.0);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetIntFromDblDataSet ()
{
  std::pair<int, int> myPair;
  myPair.first = 1;
  myPair.second = 1;
  VEC_INT_PAIR indices(3, myPair);
  indices.at(1).second = 9;
  std::vector<int> data;
  Create1(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(!m_p1->GetData(data));
  m_p1->AllowTypeConversions(true);
  TS_ASSERT(m_p1->GetData(data));
  TS_ASSERT(data.size() == 9);
  TS_ASSERT_EQUALS(data.at(2), 40);
  TS_ASSERT_EQUALS(data.at(8), 100);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetDataSetTypeSize ()
{
  VEC_INT_PAIR indices;
  Create(m_file2, "General Head/04. Map ID", indices);
  TS_ASSERT(m_p->GetDataSetTypeSize() == sizeof(char));
  delete(m_p); m_p = NULL;
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->GetDataSetTypeSize() == sizeof(double));
  delete(m_p); m_p = NULL;
  Create(m_file, "1D_Data_Sets/FloatDataSet", indices);
  TS_ASSERT(m_p->GetDataSetTypeSize() == sizeof(float));
  delete(m_p); m_p = NULL;
  Create(m_file, "1D_Data_Sets/IntDataSet", indices);
  TS_ASSERT(m_p->GetDataSetTypeSize() == sizeof(int));
  delete(m_p); m_p = NULL;
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetDataSetDimensions ()
{
  VEC_INT_PAIR indices;
  CStr file("crap");
  std::vector<hsize_t> sizes;
  Create(file, "path", indices);
  TS_ASSERT(!m_p->GetDataSetDimensions(sizes));

  Create(m_file2, "River/07. Property", indices);
  TS_ASSERT(m_p->GetDataSetDimensions(sizes));
  TS_ASSERT(sizes.size() == 3);
  TS_ASSERT(sizes.at(0) == 3);
  TS_ASSERT(sizes.at(1) == 3);
  TS_ASSERT(sizes.at(2) == 3);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testDataSetExists ()
{
  VEC_INT_PAIR indices;
  CStr file("crap");
  std::vector<hsize_t> sizes;
  Create(file, "path", indices);
  TS_ASSERT(!m_p->DataSetExists());

  Create(m_file2, "River/07. Property", indices);
  TS_ASSERT(m_p->DataSetExists());
}
#endif // CXX_TEST


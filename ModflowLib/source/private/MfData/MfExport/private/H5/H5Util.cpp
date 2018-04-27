//------------------------------------------------------------------------------
// FILE      H5Util.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private\MfData\MfExport\private\H5\H5Util.h>

// 3. Standard library headers

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private\H5DataReader\H5DataSetWriter.h>
#include <private\H5DataReader\H5DataSetWriterSetup.h>
#include <private\MfData\MfExport\private\H5\H5Strings.h>
#include <private\MfData\MfGlobal.h>
#include <private\util\util.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
namespace MfData
{
namespace Export
{


//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------


//------------------------------------------------------------------------------
/// \brief For each H5 dataset (string) give chunks
//------------------------------------------------------------------------------
static std::map<CStr, std::vector<int> >& GetChunkMap ()
{
  static std::map<CStr, std::vector<int> > fg_chunkMap; // ok to leave
  if (fg_chunkMap.empty())
  {
    using std::vector;
    typedef std::vector<int> vec_int;
    vec_int v_1(1, 1);
    vec_int v_3(1, 3);
    vec_int v_5000(1, 5000);

    vec_int v_3_3(2, 3);
    vec_int v_3_5000;
    v_3_5000.push_back(3);
    v_3_5000.push_back(5000);
    vec_int v_5000_3;
    v_5000_3.push_back(5000);
    v_5000_3.push_back(3);
    
    vec_int v_3_5000_1;
    v_3_5000_1.push_back(3);
    v_3_5000_1.push_back(5000);
    v_3_5000_1.push_back(1);

    fg_chunkMap[FILE_VERSION] = v_1;
    fg_chunkMap[MFBC_VERSION] = v_1;
    fg_chunkMap["Arrays"] = v_5000;
    fg_chunkMap[MFBC_NUMBC] = v_1;
    fg_chunkMap[MFBC_USELAST] = v_3;
    fg_chunkMap["ET/01. Use Last"] = v_3_3;
    fg_chunkMap["ETS/01. Use Last"] = v_3_3;
    fg_chunkMap["Recharge/01. Use Last"] = v_3_3;
    fg_chunkMap["UZF/01. Use Last"] = v_3_3;
    fg_chunkMap[MFBC_CELLIDS] = v_5000;
    fg_chunkMap[MFBC_NAME] = v_5000;
    fg_chunkMap[MFBC_MAPIDSTR] = v_5000;
    fg_chunkMap[MFBC_FACTOR] = v_3;
    fg_chunkMap[MFBC_IFACE] = v_5000;
    fg_chunkMap[MFBC_DATA] = v_3_5000_1;
    fg_chunkMap[MFBC_DATAMULT] = v_3_3;
    fg_chunkMap[MFBC_LAY] = v_5000_3;
    fg_chunkMap[MFBC_LAYMULT] = v_3;
    // stream
    fg_chunkMap[MFBC_STRSEGID] = v_5000;
    fg_chunkMap[MFBC_SEGID] = v_5000;
    fg_chunkMap[MFBC_SEGFLW] = v_5000_3;
    fg_chunkMap[MFBC_ITRIB] = v_5000_3;
    fg_chunkMap[MFBC_UPID] = v_5000;
    fg_chunkMap[MFBC_NSEG] = v_1;
    fg_chunkMap[MFBC_SEGP] = v_3_5000_1;
    fg_chunkMap[MFBC_SEGFLWT] = v_3_5000;
    // ets
    fg_chunkMap[MFBC_PXDP] = v_3_5000_1;
    fg_chunkMap[MFBC_PXDPMULT] = v_3_3;
    fg_chunkMap[MFBC_PETM] = v_3_5000_1;
    fg_chunkMap[MFBC_PETMMULT] = v_3_3;
    fg_chunkMap[MFBC_NETSEG] = v_1;
    // mnw
    fg_chunkMap[MFBC_KSPREF] = v_1;
    fg_chunkMap[MFBC_LOSSTYPE] = v_1;
    fg_chunkMap[MFBC_IOWELL2] = v_3;
    // uzf
    fg_chunkMap[MFBC_IUZFBND] = v_5000;
    fg_chunkMap[MFBC_IUZFBNDMULT] = v_5000;
    fg_chunkMap[MFBC_IRUNBND] = v_5000;
    fg_chunkMap[MFBC_IRUNBNDMULT] = v_5000;
    fg_chunkMap[MFBC_VKS] = v_5000;
    fg_chunkMap[MFBC_VKSMULT] = v_3;
    fg_chunkMap[MFBC_EPS] = v_5000;
    fg_chunkMap[MFBC_EPSMULT] = v_3;
    fg_chunkMap[MFBC_THTS] = v_5000;
    fg_chunkMap[MFBC_THTSMULT] = v_3;
    fg_chunkMap[MFBC_THTI] = v_5000;
    fg_chunkMap[MFBC_THTIMULT] = v_3;
    // sub
    fg_chunkMap[MFBC_RNB] = v_5000;
    fg_chunkMap[MFBC_RNBMULT] = v_3;
    fg_chunkMap[MFBC_HC] = v_5000;
    fg_chunkMap[MFBC_HCMULT] = v_3;
    fg_chunkMap[MFBC_SFE] = v_5000;
    fg_chunkMap[MFBC_SFEMULT] = v_3;
    fg_chunkMap[MFBC_SFV] = v_5000;
    fg_chunkMap[MFBC_SFVMULT] = v_3;
    fg_chunkMap[MFBC_COM] = v_5000;
    fg_chunkMap[MFBC_COMMULT] = v_3;
    fg_chunkMap[MFBC_DSTART] = v_5000;
    fg_chunkMap[MFBC_DSTARTMULT] = v_3;
    fg_chunkMap[MFBC_DHC] = v_5000;
    fg_chunkMap[MFBC_DHCMULT] = v_3;
    fg_chunkMap[MFBC_DCOM] = v_5000;
    fg_chunkMap[MFBC_DCOMMULT] = v_3;
    fg_chunkMap[MFBC_DZ] = v_5000;
    fg_chunkMap[MFBC_DZMULT] = v_3;
    fg_chunkMap[MFBC_NZ] = v_5000;
    fg_chunkMap[MFBC_NZMULT] = v_3;
  }
  return fg_chunkMap;
} // GetChunkMap
//------------------------------------------------------------------------------
/// \brief Creates a dataset in an h5 file
//------------------------------------------------------------------------------
static void CreateH5Dataset (const CStr &a_file,
                             const CStr &a_group,
                             const CStr &a_path,
                             const hid_t a_type,
                             const int a_dim,
                             const bool a_compress)
{
  int    iData(0);
  double dData(0);
  char   cData(0);
  CStr   path;

  if (a_group != "")
    path = a_group + "/" + a_path;
  else
    path = a_path;
  H5DataSetWriterSetup s1(a_file, path, a_type, a_dim, a_compress);
  std::map<CStr,std::vector<int> >& chunkMap = GetChunkMap();
  std::vector<int> chunkSize;
  if (chunkMap.find(a_group) != chunkMap.end())
    chunkSize = chunkMap[a_group];
  else if (chunkMap.find(path) != chunkMap.end())
    chunkSize = chunkMap[path];
  else
    chunkSize = chunkMap[a_path];
  s1.SetChunkSize(chunkSize);
  
  std::vector<hsize_t> start, n2write;
  start.assign(a_dim, 0);
  n2write.assign(a_dim,0);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter t1(&s1);
  t1.SetDimInfoForWriting(&dim);
  if (a_type == H5T_NATIVE_INT)
    t1.WriteData(&iData, 0);
  else if (a_type == H5T_NATIVE_CHAR)
    t1.WriteData(&cData, 0);
  else
    t1.WriteData(&dData, 0);
} // CreateH5Dataset
//------------------------------------------------------------------------------
/// \brief Adds data sets to the BC group passed in
//------------------------------------------------------------------------------
static void CreateBcGroupDatasets (CStr& file,
                                   const char* a_group,
                                   bool a_compress)
{
  const int NUM_BC_PATHS = 7;
  CStr bcPaths[NUM_BC_PATHS] = { MFBC_NUMBC, MFBC_USELAST, MFBC_CELLIDS,
                                 MFBC_NAME, MFBC_MAPIDSTR, MFBC_IFACE,
                                 MFBC_DATA };
  hid_t bcType[NUM_BC_PATHS] = { H5T_NATIVE_INT, H5T_NATIVE_INT, H5T_NATIVE_INT,
                                 H5T_NATIVE_CHAR, H5T_NATIVE_CHAR,
                                 H5T_NATIVE_INT, H5T_NATIVE_DOUBLE };
  int   bcDim[NUM_BC_PATHS] = { 1, 1, 1, 1, 1, 1, 3 };

  for (int j=0; j<NUM_BC_PATHS; j++)
  {
    CreateH5Dataset(file, a_group, bcPaths[j], bcType[j], bcDim[j],
                    a_compress);
  }

} // CreateBcGroupDatasets
//------------------------------------------------------------------------------
/// \brief Goes through the bcs and writes the int attribute for the char
/// datasets i.e. Name, Map ID
//------------------------------------------------------------------------------
static void iWriteIntAttForDataSets (hid_t fid,
                                     CStr *bcGrp,
                                     int a_nBc,
                                     std::vector<CStr> &bcPaths,
                                     const char *attrName,
                                     int value)
{
  int i;
  hid_t dataId(-1);
  CStr path;
  for (i=0; i<a_nBc; i++)
  {
    std::vector<CStr>::iterator currPath = bcPaths.begin(); 
    for ( ; currPath != bcPaths.end(); ++currPath)
    {
      path.Format("%s/%s", bcGrp[i], *currPath);
      dataId = H5Dopen(fid, path);
      if (dataId > -1)
      {
        xfpWriteAttributeInt(dataId, attrName, 1, &value);
        H5Dclose(dataId);
        dataId = -1;
      }
    }
  }
} // iWriteIntAttForCharDataSets
//------------------------------------------------------------------------------
/// \brief Creates a default modflow h5 file
//------------------------------------------------------------------------------
bool H5Util_CreateDefaultMfH5File (const char *a_,
                                   int a_modelType/*=1*/,
                                   bool a_compress/*=false*/)
{
  CStr file(a_);
  file += ".h5";

  H5DataSetWriterSetup s(file);
  H5DataSetWriter t(&s);
  if (!t.CreateGroup("Arrays"))
  {
    return false;
  }

  int    i, j;
  const int GROUPS = 10;
  CStr bcGrp[GROUPS] = { "Drain", "General Head", "River", "Specified Head",
                         "Well", "Stream", "Stream (SFR2)", "Drain Return",
                         "Multi-Node Well", "MNW2" };

  const int SW_GROUPS = 2;
  CStr swBcGrp[SW_GROUPS] = { "VDF", "VSC" };

  const int NUM_ST_PATHS = 6;
  CStr stPaths[NUM_ST_PATHS] = { MFBC_STRSEGID, MFBC_SEGID, MFBC_SEGFLW,
                                 MFBC_ITRIB, MFBC_UPID, MFBC_NSEG };
  const int NUM_SF_PATHS = 4;
  CStr sfPaths[NUM_SF_PATHS] = { MFBC_STRSEGID, MFBC_NSEG, MFBC_SEGP,
                                 MFBC_SEGFLWT };
  hid_t stType[NUM_ST_PATHS] = { H5T_NATIVE_INT, H5T_NATIVE_INT,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_INT,
                                 H5T_NATIVE_INT, H5T_NATIVE_INT };
  hid_t sfType[NUM_SF_PATHS] = { H5T_NATIVE_INT, H5T_NATIVE_INT,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE};
  int   stDim[NUM_ST_PATHS] = { 1, 1, 2, 2, 1, 1 };
  int   sfDim[NUM_SF_PATHS] = { 1, 1, 3, 2 };

  for (i=0; i<GROUPS; i++)
  {
    t.CreateGroup(bcGrp[i]);
    CreateBcGroupDatasets(file, bcGrp[i], a_compress);
  }

  // do the extra stuff on streams (STR)
  for (j=0; j<NUM_ST_PATHS; j++)
  {
    CreateH5Dataset(file, bcGrp[5], stPaths[j], stType[j], stDim[j],
                    a_compress);
  }

  // do the extra stuff for streams (SFR2)
  for (j=0; j<NUM_SF_PATHS; j++)
  {
    CreateH5Dataset(file, bcGrp[6], sfPaths[j], sfType[j], sfDim[j],
                    a_compress);
  }

  // create SEAWAT groups
  if (a_modelType == MfData::SEAWAT)
  {
    for (i=0; i<SW_GROUPS; i++)
    {
      t.CreateGroup(swBcGrp[i]);
      CreateBcGroupDatasets(file, swBcGrp[i], a_compress);
    }
  }

  CStr arGrp[3] = {"ET", "Recharge", "ETS"};
  CStr arPaths[5] = { MFBC_USELAST, MFBC_DATA, MFBC_DATAMULT, MFBC_LAY,
                      MFBC_LAYMULT };
  hid_t arType[5] = { H5T_NATIVE_INT, H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                      H5T_NATIVE_INT, H5T_NATIVE_INT };
  int   arDim[5] = { 2, 3, 2, 2, 1 };

  for (i=0; i<3; i++)
  {
    t.CreateGroup(arGrp[i]);
    for (j=0; j<5; j++)
    {
      CreateH5Dataset(file, arGrp[i], arPaths[j], arType[j], arDim[j],
                      a_compress);
    }
  }
  
  int etStuffSize = 5;
  CStr etPaths[] = { MFBC_PXDP, MFBC_PXDPMULT, MFBC_PETM, MFBC_PETMMULT,
                     MFBC_NETSEG };
  hid_t etType[] = { H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                     H5T_NATIVE_DOUBLE, H5T_NATIVE_INT};
  int   etDim[] = { 3, 2, 3, 2, 1 };

  // do the extra stuff for evapotranspiration (ETS)
  for (j=0; j<etStuffSize; j++)
  {
    CreateH5Dataset(file, arGrp[2], etPaths[j], etType[j], etDim[j],
                    a_compress);
  }

  // UZF
  const int NUM_UZ_PATHS = 15;
  CStr uzPaths[NUM_UZ_PATHS] = { MFBC_USELAST, MFBC_DATA, MFBC_DATAMULT,
                                 MFBC_IUZFBND, MFBC_IUZFBNDMULT,
                                 MFBC_IRUNBND, MFBC_IRUNBNDMULT,
                                 MFBC_VKS, MFBC_VKSMULT,
                                 MFBC_EPS, MFBC_EPSMULT,
                                 MFBC_THTS, MFBC_THTSMULT,
                                 MFBC_THTI, MFBC_THTIMULT };
  hid_t uzType[NUM_UZ_PATHS] = { H5T_NATIVE_INT, H5T_NATIVE_DOUBLE, 
                                 H5T_NATIVE_DOUBLE,
                                 H5T_NATIVE_INT, H5T_NATIVE_INT,
                                 H5T_NATIVE_INT, H5T_NATIVE_INT,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE };
  int   uzDim[NUM_UZ_PATHS] = { 2, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };

  t.CreateGroup("UZF");
  for (j=0; j<NUM_UZ_PATHS; j++)
  {
    CreateH5Dataset(file, "UZF", uzPaths[j], uzType[j], uzDim[j],
                    a_compress);
  }

  int mnwStuffSize = 3;
  CStr mnwPaths[] = { MFBC_KSPREF, MFBC_LOSSTYPE, MFBC_IOWELL2 };
  hid_t mnwType[] = { H5T_NATIVE_INT, H5T_NATIVE_DOUBLE, H5T_NATIVE_INT };
  int   mnwDim[]  = { 1, 1, 1 };

  // do the extra stuff for multi-node well (MNW)
  for (j=0; j<mnwStuffSize; ++j)
  {
    CreateH5Dataset(file, bcGrp[8], mnwPaths[j], mnwType[j], mnwDim[j],
                    a_compress);
  }

  // sub
  const int NUM_SUB_PATHS = 23;
  CStr subPaths[NUM_SUB_PATHS] = { MFBC_USELAST, MFBC_DATA, MFBC_DATAMULT,
                                   MFBC_RNB, MFBC_RNBMULT,
                                   MFBC_HC, MFBC_HCMULT,
                                   MFBC_SFE, MFBC_SFEMULT,
                                   MFBC_SFV, MFBC_SFVMULT,
                                   MFBC_COM, MFBC_COMMULT,
                                   MFBC_DSTART, MFBC_DSTARTMULT,
                                   MFBC_DHC, MFBC_DHCMULT,
                                   MFBC_DCOM, MFBC_DCOMMULT,
                                   MFBC_DZ, MFBC_DZMULT,
                                   MFBC_NZ, MFBC_NZMULT };
  hid_t subType[NUM_SUB_PATHS] = { H5T_NATIVE_INT, H5T_NATIVE_DOUBLE, 
                                   H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE, 
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_INT, H5T_NATIVE_INT };
  int   subDim[NUM_SUB_PATHS] = { 2, 3, 2, 
                                  2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
                                  2, 1, 2, 1, 2, 1, 2, 1, 2, 1 };

  t.CreateGroup("SUB");
  for (j=0; j<NUM_SUB_PATHS; j++)
  {
    CreateH5Dataset(file, "SUB", subPaths[j], subType[j], subDim[j],
                    a_compress);
  }

  // bc version
  CreateH5Dataset(file, "", MFBC_VERSION, H5T_NATIVE_DOUBLE, 1, a_compress);
  {
    H5DataSetWriterSetup s1(file, MFBC_VERSION, H5T_NATIVE_DOUBLE, 1);
    H5DataSetWriter t1(&s1);
    double tmpd(3.0);
    t1.WriteData(&tmpd, 1);
  }

  // file version
  CreateH5Dataset(file, "", FILE_VERSION, H5T_NATIVE_DOUBLE, 1, a_compress);
  {
    H5DataSetWriterSetup s1(file, FILE_VERSION, H5T_NATIVE_DOUBLE, 1);
    H5DataSetWriter t1(&s1);
    double tmpd(1.0);
    t1.WriteData(&tmpd, 1);
  }

  std::vector<CStr> bcAttrPaths;
  bcAttrPaths.push_back(MFBC_NAME);
  bcAttrPaths.push_back(MFBC_MAPIDSTR);
  {
    hid_t fid(H5DataReader::GetFileId(file));
    xfpWriteDatasetString(fid, "File Type", "Xmdf");
    // write the int attribute for the mapid and name
    iWriteIntAttForDataSets(fid, bcGrp, GROUPS, bcAttrPaths,
                            MFBC_MAX_STR_LEN, 1);

    // write the int attribute for segment flow table
    bcAttrPaths.clear();
    bcAttrPaths.push_back(MFBC_SEGFLWT);
    iWriteIntAttForDataSets(fid, bcGrp+6, 1, bcAttrPaths, "NumRows", 0);
  }
  return true;
} // CreateDefaultMfH5File
//------------------------------------------------------------------------------
/// \brief Adds the Wel (CLN) group to the h5 file
//------------------------------------------------------------------------------
void H5Util_CreateWelClnGroup (const char* a_,
                               bool a_compress)
{
  CStr file(a_);
  file += ".h5";

  H5DataSetWriterSetup s(file);
  H5DataSetWriter t(&s);
  if (t.CreateGroup("WEL (CLN)"))
  {
    CreateBcGroupDatasets(file, "WEL (CLN)", a_compress);
  }

  {
    std::vector<CStr> bcAttrPaths;
    bcAttrPaths.push_back(MFBC_NAME);
    bcAttrPaths.push_back(MFBC_MAPIDSTR);
    hid_t fid(H5DataReader::GetFileId(file));
    CStr bcGrp[1] = {"WEL (CLN)"};
    // write the int attribute for the mapid and name
    iWriteIntAttForDataSets(fid, bcGrp, 1, bcAttrPaths,
      MFBC_MAX_STR_LEN, 1);
  }
} // H5Util_CreateWelClnGroup

} // namespace Export
} // namespace MfData

//------------------------------------------------------------------------------
/// \brief Writes a string dataset
//------------------------------------------------------------------------------
int xfpWriteDatasetString (hid_t a_Loc,
  const char *a_Name,
  const char *a_Str)
{
  hid_t   DsetId, SpaceId;
  hid_t   StringType;
  hsize_t Dims;
  herr_t  status;

  /* Create the string type */
  StringType = H5Tcopy(H5T_C_S1);
  if (StringType < 0) {
    return StringType;
  }
  H5Tset_strpad(StringType, H5T_STR_NULLTERM);

  /* Set the length of the string datatype */
  status = H5Tset_size(StringType, strlen(a_Str) + 1);

  /* Create the dataspace; */
  Dims = 1;
  SpaceId = H5Screate_simple(1, &Dims, &Dims);

  DsetId = H5Dcreate(a_Loc, a_Name, StringType, SpaceId, H5P_DEFAULT);
  if (DsetId < 0) {
    H5Tclose(StringType);
    H5Sclose(SpaceId);
    return DsetId;
  }

  status = H5Dwrite(DsetId, StringType, H5S_ALL, H5S_ALL, H5P_DEFAULT,
    a_Str);

  /* close resources */
  H5Sclose(SpaceId);
  H5Dclose(DsetId);
  H5Tclose(StringType);

  return status;
} // xfpWriteDatasetString
//------------------------------------------------------------------------------
/// \brief writes an int attribute to a dataset
//------------------------------------------------------------------------------
int xfpWriteAttributeInt (hid_t a_Loc,
  const char *a_Name,
  int a_Number,
  int *a_val)
{
  hid_t   AttId, SpaceId;
  hsize_t Dims;
  herr_t  status;

  /* Create the dataspace; */
  Dims = a_Number;
  SpaceId = H5Screate_simple(1, &Dims, &Dims);

  AttId = H5Aopen_name(a_Loc, a_Name);
  if (AttId < 0) {
    AttId = H5Acreate(a_Loc, a_Name, H5T_NATIVE_INT, SpaceId, H5P_DEFAULT);
    if (AttId < 0) {
      H5Sclose(SpaceId);
      return AttId;
    }
  }

  status = H5Awrite(AttId, H5T_NATIVE_INT, a_val);

  /* close resources */
  H5Sclose(SpaceId);
  H5Aclose(AttId);

  return status;
} // xfpWriteAttributeInt
//------------------------------------------------------------------------------
/// \brief writes a string attribute to a dataset
//------------------------------------------------------------------------------
int xfpWriteAttributeString (hid_t a_Loc,
  const char * a_Name, 
  const char * a_Str)
{
  hid_t   AttId, SpaceId;
  hid_t   StringType;
  hsize_t Dims;
  herr_t  status;
  int     length;

  // Create the string type
  StringType = H5Tcopy(H5T_C_S1);
  if (StringType < 0) {
    return StringType;
  }
  H5Tset_strpad(StringType, H5T_STR_NULLTERM);

  // Set the length of the string datatype
  length = (int)strlen(a_Str) + 1;
  status = H5Tset_size(StringType, length);

  // Create the dataspace
  Dims = 1;
  SpaceId = H5Screate_simple(1, &Dims, &Dims);

  AttId = H5Acreate1(a_Loc, a_Name, StringType, SpaceId, H5P_DEFAULT);
  if (AttId < 0) {
    H5Sclose(SpaceId);
    return AttId;
  }

  status = H5Awrite(AttId, StringType, a_Str);

  // close resources
  H5Sclose(SpaceId);
  H5Aclose(AttId);
  H5Tclose(StringType);

  return status;
} // xfpWriteAttributeString
//------------------------------------------------------------------------------
/// \brief writes a string attribute to a dataset
//------------------------------------------------------------------------------
int xfpWriteAttributeDouble (hid_t a_Loc,
  const char *a_Name, 
  int a_Number,
  double *a_val)
{
  hid_t   AttId, SpaceId;
  hsize_t Dims;
  herr_t  status;

  // Create the dataspace
  Dims = a_Number;
  SpaceId = H5Screate_simple(1, &Dims, &Dims);

  AttId = H5Acreate1(a_Loc, a_Name, H5T_IEEE_F64LE, SpaceId, H5P_DEFAULT);
  if (AttId < 0) {
    H5Sclose(SpaceId);
    return AttId;
  }

  status = H5Awrite(AttId, H5T_NATIVE_DOUBLE, a_val);

  // close resources
  H5Sclose(SpaceId);
  H5Aclose(AttId);

  return status;
} // xfpWriteAttributeDouble


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\H5\H5Util.t.h>

#include <private\H5DataReader\H5DataSetReader.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\MfLibAsserts.h>

#define TESTBASE "4814dfa0-51de-11dd-ae16-0800200c9a66"

#define _TS_ASSERT_H5_INT(f, l, file, path, expected)                          \
  Check1DH5IntValue(f, l, file, path, expected)
#define TS_ASSERT_H5_INT(file, path, expected)                                 \
  _TS_ASSERT_H5_INT(__FILE__, __LINE__, file, path, expected)

#define _TS_ASSERT_H5_DBL(f, l, file, path, expected)                          \
  Check1DH5DoubleValue(f, l, file, path, expected)
#define TS_ASSERT_H5_DBL(file, path, expected)                                 \
  _TS_ASSERT_H5_DBL(__FILE__, __LINE__, file, path, expected)

#define _TS_ASSERT_H5_ARRAY(f, l, file, path, expected, length)                \
  CheckH5ArrayValue(f, l, file, path, expected, length)
#define TS_ASSERT_H5_ARRAY(file, path, expected, length)                       \
  _TS_ASSERT_H5_ARRAY(__FILE__, __LINE__, file, path, expected, length)

#define _TS_ASSERT_H5_INT_ATT(f, l, file, path, att, expected)                 \
  CheckH5IntAtt(f, l, file, path, att, expected)
#define TS_ASSERT_H5_INT_ATT(file, path, att, expected)                        \
  _TS_ASSERT_H5_INT_ATT(__FILE__, __LINE__, file, path, att, expected)

#define EQ_TOL(A, B, tolerance) (fabs((A) - (B)) <= (tolerance))

//------------------------------------------------------------------------------
template <class T>
static void CheckH5ArrayValue (const char *a_file,
                               int a_line,
                               const CStr& a_filePath,
                               const CStr& a_h5path,
                               const T* a_expected,
                               size_t a_expectedLength)
{
  std::vector<T> actual;
  std::vector<T> expected(a_expected, a_expected+a_expectedLength);
  H5DataSetReader n(a_filePath, a_h5path);
  n.AllowTypeConversions(true); // allow conversion from double to float
  n.GetAllData(actual);
  _TS_ASSERT_EQUALS_VEC(a_file, a_line, expected, actual);
}
//------------------------------------------------------------------------------
void H5UtilT::testSupportedPackage ()
{
  using namespace MfData::Packages;
  MfData::Export::Mf2kNative e;
  TxtExporter* t(e.GetExp());
  TS_ASSERT(t->IsTypeSupported(BAS6));
  TS_ASSERT(t->IsTypeSupported(BCF));
  TS_ASSERT(t->IsTypeSupported(BCF6));
  TS_ASSERT(t->IsTypeSupported(BCT));
  TS_ASSERT(t->IsTypeSupported(CHD));
  TS_ASSERT(t->IsTypeSupported(CLN));
  TS_ASSERT(t->IsTypeSupported(DE4));
  TS_ASSERT(t->IsTypeSupported(DIS));
  TS_ASSERT(t->IsTypeSupported(DRN));
  TS_ASSERT(t->IsTypeSupported(DRT));
  TS_ASSERT(t->IsTypeSupported(EVT));
  TS_ASSERT(t->IsTypeSupported(ETS));
  TS_ASSERT(t->IsTypeSupported(GAGE));
  TS_ASSERT(t->IsTypeSupported(GHB));
  TS_ASSERT(t->IsTypeSupported(GLOBAL));
  TS_ASSERT(t->IsTypeSupported(GMG));
  TS_ASSERT(t->IsTypeSupported(GNC));
  TS_ASSERT(t->IsTypeSupported(HFB));
  TS_ASSERT(t->IsTypeSupported(HUF2));
  TS_ASSERT(t->IsTypeSupported(HUF));
  TS_ASSERT(t->IsTypeSupported(LAK));
  TS_ASSERT(t->IsTypeSupported(LIST));
  TS_ASSERT(t->IsTypeSupported(LMG));
  //TS_ASSERT(t->IsTypeSupported(LMT6));
  TS_ASSERT(t->IsTypeSupported(LPF));
  TS_ASSERT(t->IsTypeSupported(MLT));
  TS_ASSERT(t->IsTypeSupported(NWT));
  TS_ASSERT(t->IsTypeSupported(NAM));
  TS_ASSERT(t->IsTypeSupported(OC));
  TS_ASSERT(t->IsTypeSupported(PCG));
  TS_ASSERT(t->IsTypeSupported(PES));
  TS_ASSERT(t->IsTypeSupported(RCH));
  TS_ASSERT(t->IsTypeSupported(RIV));
  TS_ASSERT(t->IsTypeSupported(SIP));
  TS_ASSERT(t->IsTypeSupported(SEN));
  TS_ASSERT(t->IsTypeSupported(SFR));
  TS_ASSERT(t->IsTypeSupported(SOR));
  TS_ASSERT(t->IsTypeSupported(STRSP));
  TS_ASSERT(t->IsTypeSupported(SUB));
  TS_ASSERT(t->IsTypeSupported(SWI));
  TS_ASSERT(t->IsTypeSupported(WEL));
  TS_ASSERT(t->IsTypeSupported(MNW));
  TS_ASSERT(t->IsTypeSupported(MNW2));
  TS_ASSERT(t->IsTypeSupported(MNWI));
  TS_ASSERT(t->IsTypeSupported(UZF));
  TS_ASSERT(t->IsTypeSupported(UPW));
  TS_ASSERT(!t->IsTypeSupported(VDF));
  TS_ASSERT(!t->IsTypeSupported(VSC));
  TS_ASSERT(t->IsTypeSupported(ZON));

  TS_ASSERT_EQUALS(e.GetTypes().size(), 49);
}
//------------------------------------------------------------------------------
void H5UtilT::testGetArrayMap ()
{
  MfData::Export::Mf2kNative ex;
  std::map<CStr, CStr> &m(ex.GetMapArrays());
  TS_ASSERT_EQUALS(84, m.size());
  TS_ASSERT(m.find("crap") == m.end());

  TS_ASSERT(m[ARR_DIS_TOP] == "top");
  TS_ASSERT(m[ARR_DIS_BOT] == "bot");
  TS_ASSERT(m[ARR_DIS_VCB] == "vcb");
  TS_ASSERT(m[ARR_BAS_IBND] == "ibound");
  TS_ASSERT(m[ARR_BAS_SHEAD] == "StartHead");

  TS_ASSERT(m[ARR_RCH_RCH] == "Recharge/07. Property");
  TS_ASSERT(m[ARR_RCH_LAY] == "Recharge/09. Layer");

  TS_ASSERT(m[ARR_EVT_SURF] == "ET/07. Property");
  TS_ASSERT(m[ARR_EVT_RATE] == "ET/07. Property");
  TS_ASSERT(m[ARR_EVT_EXT] == "ET/07. Property");
  TS_ASSERT(m[ARR_EVT_LAY] == "ET/09. Layer");

  TS_ASSERT(m[ARR_ETS_SURF] == "ETS/07. Property");
  TS_ASSERT(m[ARR_ETS_RATE] == "ETS/07. Property");
  TS_ASSERT(m[ARR_ETS_EXT] == "ETS/07. Property");
  TS_ASSERT(m[ARR_ETS_LAY] == "ETS/09. Layer");
  TS_ASSERT(m[ARR_ETS_PXDP] == "ETS/16. Ext Depth");
  TS_ASSERT(m[ARR_ETS_PETM] == "ETS/18. Evap Rate");

  TS_ASSERT(m[ARR_BCF_HY] == "HY_");
  TS_ASSERT(m[ARR_BCF_HK_U] == "HY_");
  TS_ASSERT(m[ARR_BCF_TRAN] == "TRAN_");
  TS_ASSERT(m[ARR_BCF_VCONT] == "LEAK_");
  TS_ASSERT(m[ARR_BCF_SF1] == "SF1_");
  TS_ASSERT(m[ARR_BCF_SF2] == "SF2_");
  TS_ASSERT(m[ARR_BCF_WET] == "WET_");

  TS_ASSERT(m[ARR_LPF_HK] == "HK");
  TS_ASSERT(m[ARR_LPF_HANI] == "HANI");
  TS_ASSERT(m[ARR_LPF_VK] == "VK");
  TS_ASSERT(m[ARR_LPF_VANI] == "VANI");
  TS_ASSERT(m[ARR_LPF_SS] == "SS");
  TS_ASSERT(m[ARR_LPF_SC] == "SS");
  TS_ASSERT(m[ARR_LPF_SY] == "SY");
  TS_ASSERT(m[ARR_LPF_WET] == "WET");
  TS_ASSERT(m[ARR_LPF_VKCBD] == "QUASIVK");
  TS_ASSERT(m[ARR_LPF_ANGX] == "ANGLEX");

  TS_ASSERT(m[ARR_UZF_UBND] == "UZF/12. IUZFBND");
  TS_ASSERT(m[ARR_UZF_RBND] == "UZF/14. IRUNBND");
  TS_ASSERT(m[ARR_UZF_VKS] == "UZF/16. VKS");
  TS_ASSERT(m[ARR_UZF_EPS] == "UZF/18. EPS");
  TS_ASSERT(m[ARR_UZF_THTS] == "UZF/20. THTS");
  TS_ASSERT(m[ARR_UZF_THTI] == "UZF/22. THTI");
  TS_ASSERT(m[ARR_UZF_RCH] == "UZF/07. Property");
  TS_ASSERT(m[ARR_UZF_ET] == "UZF/07. Property");
  TS_ASSERT(m[ARR_UZF_EXT] == "UZF/07. Property");
  TS_ASSERT(m[ARR_UZF_EXTWC] == "UZF/07. Property");

  TS_ASSERT(m[ARR_VDF_DENS] == "VDF/07. Property");
  TS_ASSERT(m[ARR_VDF_CONC] == "VDF/07. Property");

  TS_ASSERT(m[ARR_VSC_VSC] == "VSC/07. Property");
  TS_ASSERT(m[ARR_VSC_CONC] == "VSC/07. Property");

  TS_ASSERT(m[ARR_SUB_RNB] == "SUB/12. RNB");
  TS_ASSERT(m[ARR_SUB_HC] == "SUB/14. HC");
  TS_ASSERT(m[ARR_SUB_SFE] == "SUB/16. Sfe");
  TS_ASSERT(m[ARR_SUB_SFV] == "SUB/18. Sfv");
  TS_ASSERT(m[ARR_SUB_COM] == "SUB/20. Com");
  TS_ASSERT(m[ARR_SUB_DSTRT] == "SUB/22. Dstart");
  TS_ASSERT(m[ARR_SUB_DHC] == "SUB/24. DHC");
  TS_ASSERT(m[ARR_SUB_DCOM] == "SUB/26. DCOM");
  TS_ASSERT(m[ARR_SUB_DZ] == "SUB/28. DZ");
  TS_ASSERT(m[ARR_SUB_NZ] == "SUB/30. NZ");

  TS_ASSERT(m[ARR_SWI_ZETA] == "ZETA");
  TS_ASSERT(m[ARR_SWI_SSZ] == "SSZ");
  TS_ASSERT(m[ARR_SWI_ISOURCE] == "ISOURCE");

  TS_ASSERT(m[ARR_LAK_ID] == "Lak_");
  TS_ASSERT(m[ARR_LAK_LEAK] == "LakLeak_");
}
//------------------------------------------------------------------------------
void H5UtilT::testIsDataArray ()
{
  using namespace MfData::Export;
  MfData::Export::Mf2kNative e;
  TS_ASSERT(!MfExportUtil::IsDataArray(CStr("crap"), e.GetMapArrays()));
  TS_ASSERT(MfExportUtil::IsDataArray(CStr(ARR_DIS_TOP),
                                      e.GetMapArrays()));
  TS_ASSERT(MfExportUtil::IsDataArray(CStr(ARR_DIS_BOT),
                                      e.GetMapArrays())); // good enough
  TS_ASSERT(MfExportUtil::IsDataArray(CStr("ZONE ARRAY: stuff"),
                                      e.GetMapArrays()));
  TS_ASSERT(MfExportUtil::IsDataArray(CStr("MULT. ARRAY: stuff"),
                                      e.GetMapArrays()));
}
//------------------------------------------------------------------------------
static bool testCheckGroupExists (hid_t fid,
                                  const char *a_)
{
  bool rval(true);
  hid_t g(H5Gopen(fid, a_));
  rval = g > -1;
  H5Gclose(g);
  return rval;
}
//------------------------------------------------------------------------------
static bool testCheckDatasetExists (hid_t fid,
                                    const char *a_)
{
  bool rval(true);
  hid_t d(H5Dopen(fid, a_));
  rval = d > -1;
  H5Dclose(d);
  return rval;
}
//------------------------------------------------------------------------------
void H5UtilT::testCreateDefaultMfH5File ()
{
  using namespace MfData::Export;
  CStr basePath;
  util::GetTempDirectory(basePath);
  basePath += "\\array";
  H5Util_CreateDefaultMfH5File(basePath, 1, false);
  H5DataReader::CloseAllH5FilesOpenForWriting();

  CStr fullPath(basePath + ".h5");
  hid_t fid;
  fid = H5Fopen(fullPath, H5F_ACC_RDONLY, H5P_DEFAULT);
  TS_ASSERT(fid > 0);
  if (fid < 0)
    return;

  TS_ASSERT(testCheckGroupExists(fid, "Arrays"));
  TS_ASSERT(testCheckGroupExists(fid, "Drain"));
  TS_ASSERT(testCheckGroupExists(fid, "Drain Return"));
  TS_ASSERT(testCheckGroupExists(fid, "ET"));
  TS_ASSERT(testCheckGroupExists(fid, "General Head"));
  TS_ASSERT(testCheckGroupExists(fid, "Recharge"));
  TS_ASSERT(testCheckGroupExists(fid, "River"));
  TS_ASSERT(testCheckGroupExists(fid, "Specified Head"));
  TS_ASSERT(testCheckGroupExists(fid, "Stream"));
  TS_ASSERT(testCheckGroupExists(fid, "Well"));
  TS_ASSERT(testCheckGroupExists(fid, "Multi-Node Well"));
  TS_ASSERT(testCheckGroupExists(fid, "UZF"));
  TS_ASSERT(!testCheckGroupExists(fid, "VDF"));
  TS_ASSERT(!testCheckGroupExists(fid, "VSC"));

  TS_ASSERT(testCheckDatasetExists(fid, "Well/00. Number of BCs"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/01. Use Last"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/02. Cell IDs"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/03. Name"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/04. Map ID"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/06. IFACE"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/07. Property"));

  TS_ASSERT(testCheckDatasetExists(fid, "Stream/08. Str reach segment ID"));
  TS_ASSERT(testCheckDatasetExists(fid, "Stream/09. Segment ID"));
  TS_ASSERT(testCheckDatasetExists(fid, "Stream/10. Segment Flow"));
  TS_ASSERT(testCheckDatasetExists(fid, "Stream/11. ITRIB"));
  TS_ASSERT(testCheckDatasetExists(fid, "Stream/12. Upstream ID"));
  TS_ASSERT(testCheckDatasetExists(fid, "Stream/13. Number of Segments"));
  
  TS_ASSERT(testCheckDatasetExists(fid, "ET/08. Property Multiplier"));

  TS_ASSERT(testCheckDatasetExists(fid, "Recharge/01. Use Last"));
  TS_ASSERT(testCheckDatasetExists(fid, "Recharge/07. Property"));
  TS_ASSERT(testCheckDatasetExists(fid, "Recharge/09. Layer"));
  TS_ASSERT(testCheckDatasetExists(fid, "Recharge/10. Layer Multiplier"));

  TS_ASSERT(testCheckDatasetExists(fid, "ETS/16. Ext Depth"));
  TS_ASSERT(testCheckDatasetExists(fid, "ETS/17. Ext Depth Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "ETS/18. Evap Rate"));
  TS_ASSERT(testCheckDatasetExists(fid, "ETS/19. Evap Rate Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "ETS/20. Number of Segments"));

  TS_ASSERT(testCheckDatasetExists(fid, "Multi-Node Well/21. Stress Period Ref"));
  TS_ASSERT(testCheckDatasetExists(fid, "Multi-Node Well/22. Loss Type"));
  TS_ASSERT(testCheckDatasetExists(fid, "Multi-Node Well/23. Well IO"));

  TS_ASSERT(testCheckDatasetExists(fid, "UZF/12. IUZFBND"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/13. IUZFBND Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/14. IRUNBND"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/15. IRUNBND Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/16. VKS"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/17. VKS Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/18. EPS"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/19. EPS Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/20. THTS"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/21. THTS Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/22. THTI"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/23. THTI Multiplier"));

  TS_ASSERT(testCheckDatasetExists(fid, MFBC_VERSION));
  H5DataSetReader r(fullPath, MFBC_VERSION);
  std::vector<double> vD;
  r.GetAllData(vD);
  TS_ASSERT(vD.size() == 1);
  TS_ASSERT_EQUALS(vD.at(0), 3.0);

  H5Fclose(fid);
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(fullPath));

  // test creation of SEAWAT specific items
  H5Util_CreateDefaultMfH5File(basePath, 3, false);
  H5DataReader::CloseAllH5FilesOpenForWriting();

  fid = H5Fopen(fullPath, H5F_ACC_RDONLY, H5P_DEFAULT);
  TS_ASSERT(fid > 0);
  if (fid < 0)
    return;

  TS_ASSERT(testCheckGroupExists(fid, "VDF"));
  TS_ASSERT(testCheckGroupExists(fid, "VSC"));

  TS_ASSERT(testCheckDatasetExists(fid, "VDF/07. Property"));
  TS_ASSERT(testCheckDatasetExists(fid, "VSC/07. Property"));

  H5Fclose(fid);
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(fullPath));
}

#endif

//------------------------------------------------------------------------------
// FILE      Export.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <Export.h>

#include <map>
#include <set>

#include <private\MfData.h>
#include <private\MfData\MfGlobal.h>
#include <private\util\util.h>

static std::map<int, CStr> &GetIunitMap();
static CStr &GetFileName();
static CStr &GetExporterStr();
static CStr &GetTablesStr();

//------------------------------------------------------------------------------
/// \brief file global for a map to look up packages based on unit numbers
//------------------------------------------------------------------------------
static std::map<int, CStr>& GetIunitMap ()
{
  static std::vector< std::map<int, CStr> > m_map; // ok to leave static
  size_t idx = MfData::Get().CurModIdx();
  while (m_map.size() < idx+1) m_map.push_back(std::map<int, CStr>());
  return m_map[idx];
  //static std::map<int, CStr> m_map;
  //return m_map;
} // GetIunitMap
//------------------------------------------------------------------------------
/// \brief This function tells if we are exporting
//------------------------------------------------------------------------------
bool &mfLibExp_Exporting ()
{
  static bool m_flag(false); // ok to leave static
  return m_flag;
} // mfLibExp_Exporting
//------------------------------------------------------------------------------
/// \brief Publishes the condfact data from Stream. This is called when the 
/// stream data is read by the list reader.
//------------------------------------------------------------------------------
void mfLibExp_StrCondFact (const Real* a_,
                           int a_nVal)
{
  if (!mfLibExp_Exporting()) return;

  MfData::SingleValFlt("STR_CONDFACT", "CONDFACT", a_);
  MfData::SingleValInt("STR_CONDFACT", "NVAL", &a_nVal);
  MfData::ExportPack("STR_CONDFACT");
} // mfLibExp_StrCondFact
//------------------------------------------------------------------------------
/// \brief Publishes the condfact data from SFR. This is called when the 
/// stream data is read by the sfr reader.
//------------------------------------------------------------------------------
void mfLibExp_SfrCondFact (const Real* a_,
                           int a_nVal,
                           const Real* a_2,
                           int a_nVal2)
{
  if (!mfLibExp_Exporting()) return;

  MfData::SingleValFlt("SFR_CONDFACT", "CONDFACT", a_);
  MfData::SingleValInt("SFR_CONDFACT", "NVAL", &a_nVal);
  MfData::SingleValFlt("SFR_CONDFACT", "CONDFACT2", a_2);
  MfData::SingleValInt("SFR_CONDFACT", "NVAL2", &a_nVal2);
  MfData::ExportPack("SFR_CONDFACT");
} // mfLibExp_SfrCondFact
//------------------------------------------------------------------------------
/// \brief This function is used to get and set the file name for an exporter
/// \param a_setFname If this string is not empty then it is used to set the 
/// name that is returned from this function.
//------------------------------------------------------------------------------
static CStr &GetFileName ()
{
  static CStr m_fname;
  return m_fname;
} // GetFileName
//------------------------------------------------------------------------------
/// \brief This function is used to get and set the file name for an exporter
/// \param a_setExporterStr If this string is not empty then it is used to set
/// the name that is returned from this function.
//------------------------------------------------------------------------------
static CStr &GetExporterStr ()
{
  static CStr m_exp;
  return m_exp;
} // GetExporterStr
//------------------------------------------------------------------------------
/// \brief This function is used to get and set packages to be exported
/// \param a_setPackStrs If this string is not empty then it is used to set
/// the name that is returned from this function.
//------------------------------------------------------------------------------
static CStr &GetTablesStr()
{
  static CStr m_str; // ok to leave static
  return m_str;
} // GetTablesStr
//------------------------------------------------------------------------------
/// \brief If the main prefix is already the first part of the namefile,
///        don't tack it on to the front again. Otherwise do.
//------------------------------------------------------------------------------
static void FormatNameFileNameForLgr (CStr t, CStr& nameFile)
{
  CStr prefix = nameFile;
  util::StripPathFromFilename(prefix, prefix);
  util::StripExtensionFromFilename(prefix, prefix);
  prefix += "_";
  if (t.find(prefix) == 0) // t starts with prefix
  {
    util::StripFileFromFilename(nameFile, nameFile);
  }
  else
  {
    util::StripFileFromFilename(nameFile, nameFile);
    nameFile += prefix;
  }
  nameFile += t;
} // FormatNameFileNameForLgr
//------------------------------------------------------------------------------
/// \brief This tells us that we are exporting a geodatabase and gives
/// the file name of the geodatabase.
/// \param a_fName a character string that is the path to the geodatabase
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_GEODB (const char *a_arg,
                               int a_dummy1,
                               const char * a_fName,
                               int a_dummy2)
{
  CStr tmp, fname(util::GetStr(a_fName, a_dummy1)),
       exp(util::GetStr(a_arg, a_dummy2));
  mfLibExp_Exporting() = true;
  GetFileName() = fname;
  GetExporterStr() = exp;
} // MFLIBEXP_GEODB
//------------------------------------------------------------------------------
/// \brief This tells us that we are exporting a GMS HDF5 modflow sim and gives
/// the file name of the simulation files.
/// \param a_fName a character string that is the path to the new simulation
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_GMS (const char *a_arg,
                             int a_dummy1,
                             const char * a_fName,
                             int a_dummy2)
{
  CStr tmp, fname(util::GetStr(a_fName, a_dummy1)),
       exp(util::GetStr(a_arg, a_dummy2));
  mfLibExp_Exporting() = true;
  GetFileName() = fname;
  GetExporterStr() = exp;
} // MFLIBEXP_GMS
//------------------------------------------------------------------------------
/// \brief This tells us that we are exporting only certain packages to a DB
/// \param a_arg a character string that has the packages we are exporting
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_TABLES (const char *a_arg,
                                int a_dummy)
{
  CStr tabs(util::GetStr(a_arg, a_dummy));

  GetTablesStr() = tabs;
} // MFLIBEXP_TABLES
//------------------------------------------------------------------------------
/// \brief This gives us the current stress period that we are reading
/// \param a_KPRE the current stress period
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_PUTCURRENTPERIOD (const int *a_KPER)
{
  MfData::PutCurrentPeriod(a_KPER);
} // MFLIBEXP_PUTCURRENTPERIOD
//------------------------------------------------------------------------------
/// \brief This gives us the current stress period that we are reading
/// \param a_KPRE the current stress period
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_PUTCURRENTGRID (const int *a_IGRID)
{
  MfData::PutCurrentGrid(a_IGRID);
} // MFLIBEXP_PUTCURRENTGRID
//------------------------------------------------------------------------------
/// \brief CLN package.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_CLNLINE1 (const int *a_NCLN,
                                  const int *a_ICLNNDS,
                                  const int *a_ICLNCB,
                                  const int *a_ICLNHD,
                                  const int *a_ICLNDD,
                                  const int *a_ICLNIB,
                                  const int *a_NCLNGWC,
                                  const int *a_NCONDUITYP)
{
  MfData::ClnLine1(a_NCLN,a_ICLNNDS,a_ICLNCB,a_ICLNHD,a_ICLNDD,a_ICLNIB,a_NCLNGWC,a_NCONDUITYP);
} // MFLIBEXP_CLNLINE1
//------------------------------------------------------------------------------
/// \brief CLN package.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_CLNLINE4 (const Real *a_ACLNNDSAQ,
                                  const int *a_NCLNNDS)
{
  MfData::ClnLine4(a_ACLNNDSAQ, a_NCLNNDS);
} // MFLIBEXP_CLNLINE4
//------------------------------------------------------------------------------
/// \brief CLN package.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_CLNLINE5 (const Real *a_ACLNGWCAQ)
{
  MfData::ClnLine5(a_ACLNGWCAQ);
} // MFLIBEXP_CLNLINE5
//------------------------------------------------------------------------------
/// \brief CLN package.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_CLNLINE6 (const Real *a_ACLNGWCAQ)
{
  MfData::ClnLine6(a_ACLNGWCAQ);
} // MFLIBEXP_CLNLINE6
//------------------------------------------------------------------------------
/// \brief CLN package.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_CLNLINE7 (const Real *a_ACLNCOND)
{
  MfData::ClnLine7(a_ACLNCOND);
} // MFLIBEXP_CLNLINE7
//------------------------------------------------------------------------------
/// \brief CLN package.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_CLNLINES8AND9 ()
{
  MfData::ClnLines8And9();
} // MFLIBEXP_CLNLINES8AND9
//------------------------------------------------------------------------------
/// \brief This is the data associated with Discretization package for MODFLOW.
/// \param a_NODES
/// \param a_NJAG
/// \param a_IVSD
/// \param a_IDSYMRD
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_DISU1 (const int *a_NODES,
                               const int *a_NJAG,
                               const int *a_IVSD,
                               const int *a_IDSYMRD)
{
  MfData::Disu1(a_NODES, a_NJAG, a_IVSD, a_IDSYMRD);
} // MFLIBEXP_DISU1
//------------------------------------------------------------------------------
/// \brief This is the data associated with Discretization package for MODFLOW.
/// \param a_NODLAY: Array of number of nodes per layer, size of num layers.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_DISU2 (const int *a_NODLAY)
{
  CStr exporter, filename;

  MfData::Disu2(a_NODLAY);
} // MFLIBEXP_DISU2
//------------------------------------------------------------------------------
/// \brief This is the data associated with Discretization package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_DISU3 (const Real* a_PERLEN,
                               const int* a_NSTP,
                               const Real* a_TSMULT,
                               const int* a_ISSFLG)
{
  MfData::Disu3(a_PERLEN, a_NSTP, a_TSMULT, a_ISSFLG);
} // MFLIBEXP_DISU3
//------------------------------------------------------------------------------
/// \brief This is the data associated with Discretization package for MODFLOW.
/// \param a_NLAY number of layers in the grid
/// \param a_NROW number of rows in the model grid
/// \param a_NCOL number of columns in the model grid
/// \param a_NPER number of stress periods in the model
/// \param a_ITMUNI the time unit for the model
/// \param a_LENUNI the length unit for the model
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_DISPACKAGE1 (const int *a_NLAY,
                                     const int *a_NROW,
                                     const int *a_NCOL,
                                     const int *a_NPER,
                                     const int *a_ITMUNI,
                                     const int *a_LENUNI,
                                     const int *a_LAYCBD,
                                     const int *a_IUNSTR)
{
  MfData::SetGlobal(a_NLAY, a_NROW, a_NCOL, a_NPER, a_ITMUNI, a_LENUNI,
                    a_LAYCBD, a_IUNSTR);
} // MFLIBEXP_DISPACKAGE1
//------------------------------------------------------------------------------
/// \brief This is the data associated with Discretization package for MODFLOW.
/// \param a_LAYCBD the array with a flag for each layer to indicate if it has
///  a confining layer below it.
/// \param a_DELR cell width along the rows
/// \param a_DELC cell width along the columns
/// \param a_NBTOM 
/// \param a_BOTM the elevation data for the model grid 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_DISPACKAGE2 (const Real *a_DELR,
                                     const Real *a_DELC,
                                     const int *a_NBTOM,
                                     const Real *a_BOTM,
                                     const Real *a_PERLEN,
                                     const int *a_NSTP,
                                     const Real *a_TSMULT,
                                     const int *a_ISSFLG)
{
  MfData::DisPackage2(a_DELR, a_DELC, a_NBTOM, a_BOTM, a_PERLEN,
                      a_NSTP, a_TSMULT, a_ISSFLG);
} // MFLIBEXP_DISPACKAGE2
//------------------------------------------------------------------------------
/// \brief This is the data associated with WEL/DRN/DRT/RIV/GHB/CHD package for
/// MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_LISTPACKAGE (const char *a_type,
                                     int dummy,
                                     const int *a_ITMP,
                                     const int *a_MXBC,
                                     const int *a_NBC,
                                     const int *a_NVL,
                                     const int *a_NAUX,
                                     const Real *a_BCDAT,
                                     const int *a_NP,
                                     const char *a_AUXNMS)
{
  CStr type(util::GetStr(a_type, dummy)), aType;

  if (type != "WEL" &&
      type != "DRN" &&
      type != "DRT" &&
      type != "GHB" &&
      type != "RIV" &&
      type != "CHD")
  {
    ErrorStack::Get().PutError("Invalid type passed to MFLIBEXP_LISTPACKAGE.");
    return;
  }

  MfData::ListPackage(type, a_ITMP, a_MXBC, a_NBC, a_NVL, a_NAUX, a_BCDAT, a_NP,
                      a_AUXNMS);
} // MFLIBEXP_LISTPACKAGE
//------------------------------------------------------------------------------
/// \brief This is the data associated with the SIP package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SIPPACKAGE (const char* a_type,
                                    int dummy,
                                    const int* a_MXITER,
                                    const int* a_NPARM,
                                    const Real* a_ACCL,
                                    const Real* a_HCLOSE,
                                    const int* a_IPCALC,
                                    const Real* a_WSEED,
                                    const int* a_IPRSIP)
{
  CStr type(util::GetStr(a_type, dummy)), aType;

  if (type != "SIP")
  {
    ErrorStack::Get().PutError("Invalid type passed to MFLIBEXP_SIPPACKAGE.");
    return;
  }

  MfData::SipPackage(type, a_MXITER, a_NPARM, a_ACCL, a_HCLOSE, a_IPCALC,
    a_WSEED, a_IPRSIP);
} // MFLIBEXP_SIPPACKAGE
//------------------------------------------------------------------------------
/// \brief This is the data associated with the DE4 package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_DE4LINE1 (const int* a_ITMX,
                                  const int* a_MXUP,
                                  const int* a_MXLOW,
                                  const int* a_MXBW)
{
  MfData::De4Line1(a_ITMX, a_MXUP, a_MXLOW, a_MXBW);
} // MFLIBEXP_DE4LINE1
//------------------------------------------------------------------------------
/// \brief This is the data associated with the DE4 package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_DE4LINE2 (const int* a_IFREQ,
                                  const int* a_MUTD4,
                                  const Real* a_ACCL,
                                  const Real* a_HCLOSE,
                                  const int* a_IPRD4)
{
  MfData::De4Line2(a_IFREQ, a_MUTD4, a_ACCL, a_HCLOSE, a_IPRD4);
} // MFLIBEXP_DE4LINE2
//------------------------------------------------------------------------------
/// \brief This is the data associated with the SOR package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SORPACKAGE (const char* a_type,
                                    int dummy,
                                    const int* a_MXITER,
                                    const Real* a_ACCL,
                                    const Real* a_HCLOSE,
                                    const int* a_IPRSOR)
{
  CStr type(util::GetStr(a_type, dummy)), aType;

  if (type != "SOR")
  {
    ErrorStack::Get().PutError("Invalid type passed to MFLIBEXP_SORPACKAGE.");
    return;
  }

  MfData::SorPackage(type, a_MXITER, a_ACCL, a_HCLOSE, a_IPRSOR);
} // MFLIBEXP_SORPACKAGE
//------------------------------------------------------------------------------
/// \brief This is the data associated with the PCG package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_PCGPACKAGE (const char* a_type,
                                    int dummy,
                                    const int* a_MXITER,
                                    const int* a_ITER1,
                                    const int* a_NPCOND,
                                    const Real* a_HCLOSE,
                                    const Real* a_RCLOSE,
                                    const Real* a_RELAX,
                                    const int* a_NBPOL,
                                    const int* a_IPRPCG,
                                    const int* a_MUTPCG,
                                    const Real* a_DAMP)
{
  CStr type(util::GetStr(a_type, dummy)), aType;

  if (type != "PCG")
  {
    ErrorStack::Get().PutError("Invalid type passed to MFLIBEXP_PCGPACKAGE.");
    return;
  }

  MfData::PcgPackage(type, a_MXITER, a_ITER1, a_NPCOND, a_HCLOSE, a_RCLOSE,
    a_RELAX, a_NBPOL, a_IPRPCG, a_MUTPCG, a_DAMP);
} // MFLIBEXP_PCGPACKAGE
//------------------------------------------------------------------------------
/// \brief This is the data associated with the SOR package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_LMGPACKAGE (const char* a_type,
                                    int dummy,
                                    const Real* a_STOR1,
                                    const Real* a_STOR2,
                                    const Real* a_STOR3,
                                    const int* a_ICG,
                                    const int* a_MXITER,
                                    const int* a_MXCYC,
                                    const Real* a_BCLOSE,
                                    const Real* a_DAMP,
                                    const int* a_IOUTAMG,
                                    const Real* a_DUP,
                                    const Real* a_DLOW,
                                    const Real* a_HCLOSE,
                                    const int* a_CONTROL)
{
  CStr type(util::GetStr(a_type, dummy)), aType;

  if (type != "LMG")
  {
    ErrorStack::Get().PutError("Invalid type passed to MFLIBEXP_LMGPACKAGE.");
    return;
  }

  MfData::LmgPackage(type, a_STOR1, a_STOR2, a_STOR3, a_ICG, a_MXITER, a_MXCYC,
                     a_BCLOSE, a_DAMP, a_IOUTAMG, a_DUP, a_DLOW,
                     a_HCLOSE, a_CONTROL);
} // MFLIBEXP_LMGPACKAGE
//------------------------------------------------------------------------------
/// \brief This is the data associated with the SOR package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_GMGPACKAGE (const char* a_type,
                                    int dummy,
                                    const Real* a_RCLOSE,
                                    const int* a_IITER,
                                    const Real* a_HCLOSE,
                                    const int* a_MXITER,
                                    const Real* a_DAMP,
                                    const int* a_IADAMP,
                                    const int* a_IOUTGMG,
                                    const int* a_ISM,
                                    const int* a_ISC,
                                    const double* a_RELAX)
{
  CStr type(util::GetStr(a_type, dummy)), aType;

  if (type != "GMG")
  {
    ErrorStack::Get().PutError("Invalid type passed to MFLIBEXP_GMGPACKAGE.");
    return;
  }

  MfData::GmgPackage(type, a_RCLOSE, a_IITER, a_HCLOSE, a_MXITER, a_DAMP,
                     a_IADAMP, a_IOUTGMG, a_ISM, a_ISC, a_RELAX);
} // MFLIBEXP_GMGPACKAGE
//------------------------------------------------------------------------------
/// \brief Data associated with the SMS solver package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SMSPACKAGE (const int* a_IFDPARAM,
                                    const double* a_HCLOSE,
                                    const double* a_HICLOSE,
                                    const int* a_MXITER,
                                    const int* a_ITER1,
                                    const int* a_IPRSMS,
                                    const int* a_NONMETH,
                                    const int* a_LINMETH,
                                    const double* a_THETA,
                                    const double* a_AKAPPA,
                                    const double* a_GAMMA,
                                    const double* a_AMOMENTUM,
                                    const int* a_NUMTRACK,
                                    const double* a_BTOL,
                                    const double* a_BREDUC,
                                    const double* a_RESLIM
                                    )
{
  MfData::SmsPackage(a_IFDPARAM, a_HCLOSE, a_HICLOSE, a_MXITER, a_ITER1,
                     a_IPRSMS, a_NONMETH, a_LINMETH, a_THETA, a_AKAPPA,
                     a_GAMMA, a_AMOMENTUM, a_NUMTRACK, a_BTOL, a_BREDUC,
                     a_RESLIM);
} // MFLIBEXP_SMSPACKAGE
//------------------------------------------------------------------------------
/// \brief Data associated with the SMS solver package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SMSXMDPACKAGE (const int* a_IACL,
                                       const int* a_NORDER,
                                       const int* a_LEVEL,
                                       const int* a_NORTH,
                                       const int* a_IREDSYS,
                                       const double* a_RRCTOL,
                                       const int* a_IDROPTOL,
                                       const double* a_EPSRN)
{
  MfData::SmsXmdPackage(a_IACL,a_NORDER,a_LEVEL,a_NORTH,a_IREDSYS,a_RRCTOL,a_IDROPTOL,a_EPSRN);
} // MFLIBEXP_SMSXMDPACKAGE
//------------------------------------------------------------------------------
/// \brief Data associated with the SMS (PCGU) solver package for MODFLOW.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SMSPCGUPACKAGE (const int* a_IPC,
                                       const int* a_ISCL,
                                       const int* a_IORD,
                                       const Real* a_RCLOSEPCGU)
{
  MfData::SmsPcguPackage(a_IPC, a_ISCL, a_IORD, a_RCLOSEPCGU);
} // MFLIBEXP_SMSPCGUPACKAGE
//------------------------------------------------------------------------------
/// \brief This is the data associated with an array
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_U2DREL (const char *a_name,
                                int a_dummy,
                                const Real *a_data,
                                const Real *a_multiplier,
                                const int *a_K,
                                const int *a_IPRN)
{
  CStr name(util::GetStr(a_name, a_dummy));
  name.Trim();
  MfData::U2DREL(name, a_data, a_multiplier, a_K, a_IPRN);
} // MFLIBEXP_U2DREL
//------------------------------------------------------------------------------
/// \brief This is the data associated with an array
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_U2DREL8 (const char *a_name,
                                 int a_dummy,
                                 const double *a_data,
                                 const Real *a_multiplier,
                                 const int *a_K,
                                 const int *a_IPRN)
{
  CStr name(util::GetStr(a_name, a_dummy));
  name.Trim();
  MfData::U2DREL8(name, a_data, a_multiplier, a_K, a_IPRN);
} // MFLIBEXP_U2DREL8
//------------------------------------------------------------------------------
/// \brief This is the data associated with an array
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_U2DINT (const char *a_name,
                                int a_dummy,
                                const int *a_data,
                                const int *a_multiplier,
                                const int *a_K,
                                const int *a_IPRN)
{
  CStr name(util::GetStr(a_name, a_dummy));
  name.Trim();
  MfData::U2DINT(name, a_data, a_multiplier, a_K, a_IPRN);
} // MFLIBEXP_U2DINT
//------------------------------------------------------------------------------
/// \brief This is the data associated with the EVT or ETS packages
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_ET (const char *a_PACK,
                            int a_dummy,
                            const int *a_NEVTOP,
                            const int *a_INSURF,
                            const int *a_INEVTR,
                            const int *a_INEXDP,
                            const int *a_INIEVT,
                            const int *a_NETSEG,
                            const int *a_INSGDF)
{
  CStr pack(util::GetStr(a_PACK, a_dummy));
  pack.Trim();
  MfData::ET(pack, a_NEVTOP, a_INSURF, a_INEVTR, a_INEXDP, a_INIEVT,
             a_NETSEG, a_INSGDF);
} // MFLIBEXP_ET
//------------------------------------------------------------------------------
/// \brief This is the data associated with the RCH package
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_RCH (const int *a_NRCHOP,
                             const int *a_INRECH,
                             const int *a_INIRCH)
{
  MfData::RCH(a_NRCHOP, a_INRECH, a_INIRCH);
} // MFLIBEXP_RCH
//------------------------------------------------------------------------------
/// \brief This is the data associated with the GNC package
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_GNC1 (const int *a_NPGNCn,
                             const int *a_MXGNn,
                             const int *a_NGNCNPn,
                             const int *a_MXADJn,
                             const int *a_I2Kn,
                             const int *a_ISYMGNCn,
                             const int *a_IFLALPHAn,
                             const int *a_IPRGNCn,
                             const int *a_N1,
                             const int *a_N2,
                             const Real *a_GNCn)
{
  MfData::GNC1(a_NPGNCn, a_MXGNn, a_NGNCNPn, a_MXADJn, a_I2Kn, a_ISYMGNCn,
               a_IFLALPHAn, a_IPRGNCn, a_N1, a_N2, a_GNCn);
} // MFLIBEXP_GNC1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_HEADINIT (const int* a_nper,
                                  const int* a_ncol,
                                  const int* a_nrow,
                                  const int* a_nlay)
{
  MfData::SetGlobal(a_nlay, a_nrow, a_ncol, a_nper);
} // MFLIBEXP_HEADINIT
//------------------------------------------------------------------------------
/// \brief This is the head solution data
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_HEAD (const int* a_iper,
                              const int* a_ncol,
                              const int* a_nrow,
                              const int* a_nlay,
                              const Real* a_head)
{
  MfData::Head(a_iper, a_ncol, a_nrow, a_nlay, a_head);
} // MFLIBEXP_HEAD
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SINGLEVALINT (const char *a_pckg,
                                      int a_dummy,
                                      const char *a_name,
                                      int a_dummy1,
                                      const int *a_flag)
{
  CStr pckg(util::GetStr(a_pckg, a_dummy));
  CStr name(util::GetStr(a_name, a_dummy1));
  MfData::SingleValInt(pckg, name, a_flag);
} // MFLIBEXP_SINGLEVALINT
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SINGLEVALFLT (const char *a_pckg,
                                      int a_dummy,
                                      const char *a_name,
                                      int a_dummy1,
                                      const Real *a_flag)
{
  CStr pckg(util::GetStr(a_pckg, a_dummy));
  CStr name(util::GetStr(a_name, a_dummy1));
  MfData::SingleValFlt(pckg, name, a_flag);
} // MFLIBEXP_SINGLEVALFLT
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SINGLEVALDBL (const char *a_pckg,
                                      int a_dummy,
                                      const char *a_name,
                                      int a_dummy1,
                                      const double *a_flag)
{
  CStr pckg(util::GetStr(a_pckg, a_dummy));
  CStr name(util::GetStr(a_name, a_dummy1));
  MfData::SingleValDbl(pckg, name, a_flag);
} // MFLIBEXP_SINGLEVALDBL
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_ARRAYVALINT (const char *a_pckg,
                                     int a_dummy,
                                    const char *a_name,
                                    int a_dummy1,
                                    const int *a_flag)
{
  CStr pckg(util::GetStr(a_pckg, a_dummy));
  CStr name(util::GetStr(a_name, a_dummy1));
  MfData::SingleValInt(pckg, name, a_flag);
} // MFLIBEXP_ARRAYVALINT
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_ARRAYVALFLT (const char *a_pckg,
                                     int a_dummy,
                                    const char *a_name,
                                    int a_dummy1,
                                    const Real *a_flag)
{
  CStr pckg(util::GetStr(a_pckg, a_dummy));
  CStr name(util::GetStr(a_name, a_dummy1));
  MfData::SingleValFlt(pckg, name, a_flag);
} // MFLIBEXP_ARRAYVALFLT
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_ARRAYVALDBL (const char *a_pckg,
                                     int a_dummy,
                                    const char *a_name,
                                    int a_dummy1,
                                    const double *a_flag)
{
  CStr pckg(util::GetStr(a_pckg, a_dummy));
  CStr name(util::GetStr(a_name, a_dummy1));
  MfData::SingleValDbl(pckg, name, a_flag);
} // MFLIBEXP_ARRAYVALDBL
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SINGLEVALSTR (const char *a_pckg,
                                      int a_dummy,
                                      const char *a_name,
                                      int a_dummy1,
                                      const char *a_str,
                                      int a_dummy2,
                                      const int *a_strLen)
{
  CStr pckg(util::GetStr(a_pckg, a_dummy));
  CStr name(util::GetStr(a_name, a_dummy1));
  CStr str(util::GetStr(a_str, a_strLen ? *a_strLen : a_dummy2));
  char* astr = util::NewCharArray(str.GetLength()+2);
  strcpy(astr, str.c_str());
  MfData::SingleValStr(pckg, name, astr);
} // MFLIBEXP_SINGLEVALSTR
//------------------------------------------------------------------------------
/// \brief Exports a particular package
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_EXPPACK (const char *a_pckg,
                                 int a_dummy)
{
  CStr pckg(util::GetStr(a_pckg, a_dummy));
  MfData::ExportPack(pckg);
} // MFLIBEXP_EXPPACK
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_LPF1 (const int *NLAY,
                              const int *ILPFCB,
                              const Real *HDRY,
                              const int *LAYTYP,
                              const int *LAYAVG,
                              const Real *CHANI,
                              const int *LAYVKA,
                              const int *LAYWET,
                              const int *VERTLEAKFLAG,
                              const int *MF2K5)
{
  MfData::LPF1(NLAY,ILPFCB,HDRY,LAYTYP,LAYAVG,CHANI,LAYVKA,LAYWET,VERTLEAKFLAG, MF2K5);
} // MFLIBEXP_LPF1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_FP_OPT (const char* PCK,
                                int a_dummy,
                                const int* ISFAC,
                                const int* ICONCV,
                                const int* ITHFLG,
                                const int* NOCVCO,
                                const int* NOVFC)
{
  CStr pckg(util::GetStr(PCK, a_dummy));
  MfData::LPF_OPT(pckg,ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC);
} // MFLIBEXP_LPF_OPT
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_BCF1 (const int *NLAY,
                              const int *IBCFCB,
                              const Real *HDRY,
                              const int *IWDFLG,
                              const Real *WETFCT,
                              const int *IWETIT,
                              const int *IHDWET,
                              const int *LAYCON,
                              const int *LAYAVG)
{
  MfData::BCF1(NLAY,IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET,LAYCON,LAYAVG);
} // MFLIBEXP_BCF1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_HFB (const int *NHFBNP,
                             const Real *HFB)
{
  MfData::HFB(NHFBNP, HFB);
} // MFLIBEXP_HFB
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SEN (const int *ISENALL,
                             const int *IUHEAD,
                             const int *IPRINTS,
                             const int *ISENSU,
                             const int *ISENPU,
                             const int *ISENFM)
{
  MfData::SEN(ISENALL,IUHEAD,IPRINTS,ISENSU,ISENPU,ISENFM);
} // MFLIBEXP_SEN
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_PVAL (const char *PARNAM,
                              int /*a_dummy*/,
                              const Real *B,
                              const int *NPVAL)
{
  MfData::PVAL(PARNAM,B,NPVAL);
} // MFLIBEXP_PVAL
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_PES (const int *ITMXP,
                             const Real *DMAX,
                             const Real *RTOL,
                             const Real *SOSC,
                             const int *IBEFLG,
                             const int *IYCFLG,
                             const int *IOSTAR,
                             const int *NOPT,
                             const int *NFIT,
                             const Real *SOSR,
                             const Real *RMAR,
                             const Real *RMARM,
                             const int *IAP,
                             const int *IPRC,
                             const int *IPRINT,
                             const int *LPRINT,
                             const Real *CSA,
                             const Real *FCONV,
                             const int *LASTX)
{
  MfData::PES(ITMXP,DMAX,RTOL,SOSC,IBEFLG,IYCFLG,IOSTAR,NOPT,
              NFIT,SOSR,RMAR,RMARM,IAP,IPRC,IPRINT,LPRINT,
              CSA,FCONV,LASTX);
} // MFLIBEXP_PES
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_OBSHD (const char *OBSNAME,
                               int a_dummy,
                               const int *LAYER,
                               const int *ROW,
                               const int *COL,
                               const int *IREFSP,
                               const Real *TOFFSET,
                               const Real *ROFF,
                               const Real *COFF,
                               const Real *HOBS,
                               const Real *STAT,
                               const int *STATFLG,
                               const int *PLOT)
{
  CStr str(util::GetStr(OBSNAME,a_dummy));
  MfData::ObsHd(str,LAYER,ROW,COL,IREFSP,TOFFSET,ROFF,COFF,HOBS,
                STAT,STATFLG,PLOT);
} // MFLIBEXP_OBSHD
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_OBSHD4 (const int *MLAY,
                                const Real *PR,
                                const int *ML)
{
  MfData::ObsHd4(MLAY,PR,ML);
} // MFLIBEXP_OBSHD4
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_OBSHD5 (const int *ITT)
{
  MfData::ObsHd5(ITT);
} // MFLIBEXP_OBSHD5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_OBSHD6 (const char *OBSNAME,
                                int a_dummy,
                                const int *IREFSP,
                                const Real *TOFFSET,
                                const Real *HOBS,
                                const Real *STATH,
                                const Real *STATDD,
                                const int *STATFLG,
                                const int *PLOT)
{
  CStr str(util::GetStr(OBSNAME,a_dummy));
  MfData::ObsHd6(str,IREFSP,TOFFSET,HOBS,STATH,STATDD,STATFLG,PLOT);
} // MFLIBEXP_OBSHD6
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_OBSFLO4 (const char *OBTYPE,
                                 int a_dummy,
                                 const char *OBSNAME,
                                 int a_dummy1,
                                 const int *IREFSP,
                                 const Real *TOFFSET,
                                 const Real *HOB,
                                 const Real *STAT,
                                 const int *STATFLG,
                                 const int *PLOT)
{
  CStr str(util::GetStr(OBTYPE,a_dummy)), str1(util::GetStr(OBSNAME, a_dummy1));
  MfData::FloObs4(str,str1,IREFSP,TOFFSET,HOB,STAT,STATFLG,PLOT);
} // MFLIBEXP_OBSFLO4
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_OBSFLO4_5 (const char *OBTYPE,
                                   int a_dummy,
                                   const char *OBSNAME,
                                   int a_dummy1,
                                   const int *IREFSP,
                                   const Real *TOFFSET,
                                   const Real *FLWOBS)
{
  CStr str(util::GetStr(OBTYPE,a_dummy)), str1(util::GetStr(OBSNAME, a_dummy1));
  MfData::FloObs4_5(str,str1,IREFSP,TOFFSET,FLWOBS);
} // MFLIBEXP_OBSFLO4_5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_OBSFLO5 (const int *NUM,
                                 const int *START,
                                 const Real *QCLS)
{
  MfData::FloObs5(NUM,START,QCLS);
} // MFLIBEXP_OBSFLO5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SENPARLIST (const int *NPLIST,
                                    const char *PARNAM,
                                    int ,
                                    const int *ISENS,
                                    const int *LN,
                                    const Real *B,
                                    const Real *BL,
                                    const Real *BU,
                                    const Real *BSCAL)
{
  MfData::SENParList(NPLIST,PARNAM,ISENS,LN,B,BL,BU,BSCAL);
} // MFLIBEXP_SENPARLIST
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_LAKSP(const int *NSOL,
                              const Real *STAGES,
                              const Real *SSMN,
                              const Real *SSMX,
                              const Real *CLAKE,
                              const int *ITMP,
                              const int *ITMP1,
                              const int *LWRT,
                              const int *LKARR,
                              const Real *BDLKNC,
                              const int *NSLMS,
                              const int *IC,
                              const int *ISUB,
                              const Real *SILLVT,
                              const Real *PRCPLK,
                              const Real *EVAPLK,
                              const Real *RNF,
                              const Real *WTHDRW,
                              const Real *CPPT,
                              const Real *CRNF,
                              const Real *CAUG,
                              const int *NLAKES)
{
#ifdef DBLPREC
  NLAKES;
  MfData::LAKSP(NSOL, STAGES, SSMN, SSMX, CLAKE, ITMP, ITMP1, LWRT, LKARR, 
                BDLKNC, NSLMS, IC, ISUB, SILLVT, PRCPLK, EVAPLK, RNF, WTHDRW, 
                CPPT, CRNF, CAUG);
#else
  std::vector<double> prcplk(PRCPLK, PRCPLK+*NLAKES);
  std::vector<double> evaplk(EVAPLK, EVAPLK+*NLAKES);
  MfData::LAKSP(NSOL, STAGES, SSMN, SSMX, CLAKE, ITMP, ITMP1, LWRT, LKARR, 
                BDLKNC, NSLMS, IC, ISUB, SILLVT, &prcplk[0], &evaplk[0], RNF,
                WTHDRW, CPPT, CRNF, CAUG);
#endif
} // MFLIBEXP_LAKSP
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_LAKSP_DBL(const int *NSOL,
                                  const Real *STAGES,
                                  const Real *SSMN,
                                  const Real *SSMX,
                                  const Real *CLAKE,
                                  const int *ITMP,
                                  const int *ITMP1,
                                  const int *LWRT,
                                  const int *LKARR,
                                  const Real *BDLKNC,
                                  const int *NSLMS,
                                  const int *IC,
                                  const int *ISUB,
                                  const Real *SILLVT,
                                  const double *PRCPLK,
                                  const double *EVAPLK,
                                  const Real *RNF,
                                  const Real *WTHDRW,
                                  const Real *CPPT,
                                  const Real *CRNF,
                                  const Real *CAUG)
{
  MfData::LAKSP(NSOL, STAGES, SSMN, SSMX, CLAKE, ITMP, ITMP1, LWRT, LKARR, 
                BDLKNC, NSLMS, IC, ISUB, SILLVT, PRCPLK, EVAPLK, RNF, WTHDRW, 
                CPPT, CRNF, CAUG);
} // MFLIBEXP_LAKSP
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_STR(int *ITMP,
                            int *IRDFLG,
                            int *IPTFLG,
                            Real *STRM,
                            int *ISTRM,
                            int *NSTREM,
                            int *MXSTRM,
                            int *ITRBAR,
                            int *IDIVAR)
{
  MfData::STRSP(ITMP,IRDFLG,IPTFLG,STRM,ISTRM,NSTREM,MXSTRM,ITRBAR,IDIVAR);
} // MFLIBEXP_STR
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SFRLINE2(int *ISTRM,
                                 int NISTRMD,
                                 Real *STRM,
                                 int NSTRMD)
{
  MfData::SFRLine2(ISTRM,NISTRMD,STRM,NSTRMD);
} // MFLIBEXP_SFRLINE2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_SFRLINE6(int *ISEG,
                                 int *IOTSG,
                                 int *IDIVAR,
                                 Real *SEG,
                                 Real *XSEC,
                                 Real *QSTAGE)
{
  MfData::SFRLine6(ISEG,IOTSG,IDIVAR,SEG,XSEC,QSTAGE);
} // MFLIBEXP_SFRLINE6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNWSETUP(const int* MXWEL2,
                                 const int* IWL2CB,
                                 const int* IWELPT,
                                 const int* KSPREF,
                                 const double* PLoss, // double precision in MODFLOW
                                 const int* IOWELL2,
                                 const int* NOMOITER,
                                 const char* FTAG,
                                 int /*dummy1*/,
                                 const char* PREFIX,
                                 int /*dummy2*/,
                                 const char* NAMES,
                                 int /*dummy3*/)
{
  MfData::MnwSetup(MXWEL2,IWL2CB,IWELPT,KSPREF,PLoss,IOWELL2,NOMOITER,FTAG,PREFIX,NAMES);
} // MFLIBEXP_MNWSETUP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNWSTRESSPERIOD(int *ITMP,
                                        int *NWELL2,
                                        double *WELL2, // double precision in MODFLOW
                                        char *MNWSITE,
                                        int /*a_dummy*/,
                                        double *MNWFLGS)
{
  MfData::MnwStressPeriod(ITMP,NWELL2,WELL2,MNWSITE,MNWFLGS);
} // MFLIBEXP_MNWSTRESSPERIOD
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_LISTPAR (const char *PNAME,
                                 int a_dummy,
                                 const char *PTYPE,
                                 int a_dummy1,
                                 Real *PVAL)
{
  CStr n(util::GetStr(PNAME,a_dummy)), t(util::GetStr(PTYPE,a_dummy1));
  MfData::ListPar(n,t,PVAL);
} // MFLIBEXP_LISTPAR
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_LISTPARDATA (const char *PNAME,
                                     int a_dummy,
                                     const char *PTYPE,
                                     int a_dummy1,
                                     const int *START,
                                     const int *LSTDIM,
                                     const int *NBC,
                                     const int *NVALS,
                                     const int *NAUX,
                                     const Real *BCDATA,
                                     const char *AUXNMS)
{
  CStr n(util::GetStr(PNAME,a_dummy)), t(util::GetStr(PTYPE,a_dummy1));
  MfData::ListParData(n,t,START,LSTDIM,NBC,NVALS,NAUX,BCDATA,AUXNMS);
} // MFLIBEXP_LISTPARDATA
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_STREAMPARINFO (const Real *PVAL,
                                       const int *START,
                                       const int *NUMBC)
{
  MfData::StreamParInfo(PVAL,START,NUMBC);
} // MFLIBEXP_STREAMPARINFO
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_ARRAYPAR (const char *PNAME,
                                  int a_dummy,
                                  const char *PTYPE,
                                  int a_dummy1,
                                  Real *PVAL,
                                  const int *NP,
                                  const int *IPLOC,
                                  const int *IPCLST,
                                  const char *MLTNAM,
                                  int ,
                                  const char *ZONNAM,
                                  int ,
                                  const char *INAME,
                                  int )
{
  CStr n(util::GetStr(PNAME,a_dummy)), t(util::GetStr(PTYPE,a_dummy1));
  MfData::ArrayPar(false,n,t,PVAL,NP,IPLOC,IPCLST,MLTNAM,ZONNAM,INAME);
} // MFLIBEXP_ARRAYPAR
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_ARRAYPARUSED (const char *PNAME,
                                      int a_dummy,
                                      const char *PTYPE,
                                      int a_dummy1,
                                      const char *INAME,
                                      int a_dummy2)
{
  CStr p(util::GetStr(PNAME,a_dummy)),
       t(util::GetStr(PTYPE,a_dummy1)),
       i(util::GetStr(INAME,a_dummy2));
  MfData::ArrayParUsed(p, t, i);
} // MFLIBEXP_ARRAYPARUSED
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_NAMEFILEITEM (const char *FTYPE,
                                      int a_dummy,
                                      const char *FNAME,
                                      int a_dummy1,
                                      const int *NIU)
{
  CStr t(util::GetStr(FTYPE,a_dummy)),
       n(util::GetStr(FNAME,a_dummy1));
  MfData::NameFileItem(t, n, NIU);
} // MFLIBEXP_NAMEFILEITEM
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_GAGE (const int * IGGLST, const int * NUMGAGE)
{
  MfData::Gage(IGGLST,NUMGAGE);
} // MFLIBEXP_GAGE
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_LGR_NAME (const char* /*FNAME*/,
                                  int /*a_dummy*/)
{
  //CStr str(util::GetStr(FNAME,a_dummy));
  //util::StripFileFromFilename(str, fname);
  //fname += GetFileName();
  //util::StripPathFromFilename(str, str);
  //fname += str;
  //MfData::Get().LgrName((LPCTSTR)fname);
    // I think we just want to do this.
  MfData::Get().LgrName(GetFileName());

} // MFLIBEXP_LGR_NAME
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_NAMEFILE_FILENAME (const int *MODTYPE,
                                           const int *IGRID,
                                           const char *FNAME,
                                           int a_dummy)
{
  static bool first(true); // ok to leave static
  if (first)
  {
    H5Initialize::Init();
    first = false;
  }
  static std::set<int> firstTime; // ok to leave static
  CStr t(util::GetStr(FNAME,a_dummy));
  CStr nameFile = GetFileName();
  if (*MODTYPE == 4) // this is mflgr
  {
    FormatNameFileNameForLgr(t, nameFile);
  }

  if (firstTime.find(*IGRID) == firstTime.end())
  {
    MfData::InitGlobal(*MODTYPE, *IGRID, GetExporterStr(),
                       nameFile, GetTablesStr());
    firstTime.insert(*IGRID);
  }

  char *t1 = util::NewCharArray(t.GetLength()+1);
  strcpy(t1, t.c_str());
  MfData::NameFileFilename(t1);
} // MFLIBEXP_NAMEFILE_FILENAME
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_HUF1 (const int *IHUFCB,
                              const Real *HDRY,
                              const int *NHUF,
                              const int *NPHUF,
                              const int *IOHUFHEADS,
                              const int *IOHUFFLOWS,
                              const int *LTHUF,
                              const int *LAYWT)
{
  MfData::HUF1(IHUFCB,HDRY,NHUF,NPHUF,IOHUFHEADS,IOHUFFLOWS,LTHUF,LAYWT);
} // MFLIBEXP_HUF1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_HUFFLAG (const int *IHGUFLG)
{
  MfData::HufFlag(IHGUFLG);
} // MFLIBEXP_HUFFLAG
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_HUFWET (const Real *WETFCT,
                                const int *IWETIT,
                                const int *IHDWET)
{
  MfData::HufWet(WETFCT,IWETIT,IHDWET);
} // MFLIBEXP_HUFWET
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_HUFHANI (const char *HGUNAM,
                                 int ,
                                 const Real *HGUHANI,
                                 const Real *HGUVANI)
{
  MfData::HufHani(HGUNAM,HGUHANI,HGUVANI);
} // MFLIBEXP_HUFHANI
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_HUFPAR (const char *PNAME,
                                int a_dummy,
                                const char *PTYPE,
                                int a_dummy1,
                                Real *PVAL,
                                const int *NP,
                                const int *IPLOC,
                                const int *IPCLST,
                                const char *MLTNAM,
                                int ,
                                const char *ZONNAM,
                                int ,
                                const char *HGUNAM,
                                int )
{
  CStr n(util::GetStr(PNAME,a_dummy)), t(util::GetStr(PTYPE,a_dummy1));
  MfData::ArrayPar(true,n,t,PVAL,NP,IPLOC,IPCLST,MLTNAM,ZONNAM,HGUNAM);
} // MFLIBEXP_HUFPAR
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_UZFLINE1 (int *NUZTOP,
                                  int *IUZFOPT,
                                  int *IRUNFLG,
                                  int *IETFLG,
                                  int *IUZFCB1,
                                  int *IUZFCB2,
                                  int *NTRAIL2,
                                  int *NSETS2,
                                  int *NUZGAG,
                                  Real *SURFDEP)
{
  MfData::UZFLine1(NUZTOP,IUZFOPT,IRUNFLG,IETFLG,IUZFCB1,IUZFCB2,NTRAIL2,NSETS2,
                   NUZGAG,SURFDEP);
} // MFLIBEXP_UZFLINE1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_UZFLINE8(int *IUZLIST)
{
  MfData::UZFLine8(IUZLIST);
} // MFLIBEXP_UZFLINE8
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_UZFSTRESSPERIOD(int *NUZF1,
                                        int *NUZF2,
                                        int *NUZF3,
                                        int *NUZF4)
{
  MfData::UZFStressPeriod(NUZF1,NUZF2,NUZF3,NUZF4);
} // MFLIBEXP_UZFSTRESSPERIOD
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_VDFLINE5(const int *MT3DRHOFLG,
                                 const int *MFNADVFD,
                                 const int *NSWTCPL,
                                 const int *IWTABLE,
                                 const Real *DENSEMIN,
                                 const Real *DENSEMAX,
                                 const Real *DNSCRIT,
                                 const Real *DENSEREF,
                                 const Real *DRHODC,
                                 const Real *DRHODPRHD,
                                 const Real *PRHDREF,
                                 const int *NSRHOEOS,
                                 const int *MTRHOSPEC,
                                 const Real *CRHOREF,
                                 const Real *FIRSTDT)
{
  MfData::VDFLine5(MT3DRHOFLG,MFNADVFD,NSWTCPL,IWTABLE,DENSEMIN,DENSEMAX,
                   DNSCRIT,DENSEREF,DRHODC,DRHODPRHD,PRHDREF,NSRHOEOS,MTRHOSPEC,
                   CRHOREF,FIRSTDT);
} // MFLIBEXP_VDFLINE5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_VDFSTRESSPERIOD(const int *INDENSE)
{
  MfData::VDFStressPeriod(INDENSE);
} // MFLIBEXP_VDFSTRESSPERIOD
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_VSCLINE3(const int *MT3DMUFLG,
                                 const Real *VISCMIN,
                                 const Real *VISCMAX,
                                 const Real *VISCREF,
                                 const Real *DMUDC,
                                 const Real *CMUREF,
                                 const int *NSMUEOS,
                                 const int *MUTEMPOPT,
                                 const int *MTMUSPEC,
                                 const int *MTMUTEMPSPEC,
                                 const Real *AMUCOEFF)
{
  MfData::VSCLine3(MT3DMUFLG,VISCMIN,VISCMAX,VISCREF,DMUDC,CMUREF,NSMUEOS,
                   MUTEMPOPT,MTMUSPEC,MTMUTEMPSPEC,AMUCOEFF);
} // MFLIBEXP_VSCLINE3
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_VSCSTRESSPERIOD(const int *INVISC)
{
  MfData::VSCStressPeriod(INVISC);
} // MFLIBEXP_VSCSTRESSPERIOD
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_NWTLN1 (const Real* toldum,
                                const Real* ftoldum,
                                const int* Mxiter,
                                const Real* Thickdum,
                                const int* Linmeth,
                                const int* IPRNWT,
                                const int* IBOTAV,
                                const int* IFDPARAM,
                                const Real* thetadum,
                                const Real* akappadum,
                                const Real* gammadum,
                                const Real* amomentdum,
                                const int* Btrack,
                                const int* Numtrack,
                                const Real* Btoldum,
                                const Real* Breducdum)
{
  MfData::NwtLn1(toldum,ftoldum,Mxiter,Thickdum,Linmeth,IPRNWT,IBOTAV,
                 IFDPARAM,thetadum,akappadum,gammadum,amomentdum,Btrack,
                 Numtrack,Btoldum,Breducdum);
} // MFLIBEXP_NWTLN1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_NWTLN2 (const int* IACL,
                                const int* NORDER,
                                const int* LEVEL,
                                const int* NORTH,
                                const int* IREDSYS,
                                const Real* RRCTOLS,
                                const int* IDROPTOL,
                                const Real* EPSRNS,
                                const Real* HCLOSEXMDDUM,
                                const int* MXITERXMD)
{
  MfData::NwtLn2(IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOLS,
                 IDROPTOL,EPSRNS,HCLOSEXMDDUM,MXITERXMD);
} // MFLIBEXP_NWTLN1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_NWTLN2A (const int* Maxitr_gmres,
                                 const int* Ilu_method,
                                 const int* Lev_fill,
                                 const Real* Stop_toldum,
                                 const int* Msdr)
{
  MfData::NwtLn2a(Maxitr_gmres,Ilu_method,Lev_fill,Stop_toldum,Msdr);
} // MFLIBEXP_NWTLN2A
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_UPWA (const int* NLAY,
                              const int* IUPWCB,
                              const Real* HDRY,
                              const int* IPHDRY,
                              const int* LAYTYPUPW,
                              const int* LAYAVG,
                              const Real* CHANI,
                              const int* LAYVKAUPW,
                              const int* LAYWET)
{
  MfData::UPW1(NLAY,IUPWCB,HDRY,IPHDRY,LAYTYPUPW,LAYAVG,CHANI,LAYVKAUPW,LAYWET);
} // MFLIBEXP_UPW1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_IUNIT (const int* IUNIT,
                               const char* CUNIT,
                               int a_dummy1)
{
  if (!IUNIT || !CUNIT)
  {
    return;
  }

  CStr cunit(util::GetStr(CUNIT, a_dummy1));
  if (*IUNIT != 0 && !cunit.empty())
  {
    GetIunitMap().insert(std::make_pair(*IUNIT, cunit));
  }
} // MFLIBEXP_IUNIT
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_COMMENT (const int* UNIT,
                                 const char* LINE,
                                 int a_dummy1)
{
  if (!UNIT || !LINE) return;

  CStr line(util::GetStr(LINE, a_dummy1));
  std::map<int, CStr>& aMap(GetIunitMap());
  std::map<int, CStr>::iterator it(aMap.find(*UNIT));
  if (it != aMap.end())
  {
    MfData::Comment(it->second.c_str(), line.c_str());
  }
} // MFLIBEXP_COMMENT
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNW2LN1 (const int* MNWMAX,
                                 const int* IWL2CB,
                                 const int* MNWPRNT,
                                 const int* NAUX,
                                 const char* MNWAUX)
{
  MfData::MNW2_Ln1(MNWMAX,IWL2CB,MNWPRNT,NAUX,MNWAUX);
} // MFLIBEXP_MNW2LN1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNW2LN2AB (const char* WELLID,
                                   int a_dummy1,
                                   const int* NNODES,
                                   const char* LOSSTYPE,
                                   int a_dummy2,
                                   const int* PUMPLOC,
                                   const int* Qlimit,
                                   const int* PPFLAG,
                                   const int* PUMPCAP)
{
  CStr wellid(util::GetStr(WELLID, a_dummy1));
  CStr losstype(util::GetStr(LOSSTYPE, a_dummy2));
  MfData::MNW2_Ln2ab(wellid,NNODES,losstype,PUMPLOC,Qlimit,PPFLAG,PUMPCAP);
} // MFLIBEXP_MNW2LN2AB
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNW2LN2C (const double* Rw,
                                  const double* Rskin,
                                  const double* Kskin,
                                  const double* B,
                                  const double* C,
                                  const double* P,
                                  const double* CWC,
                                  const char* LnDesc,
                                  int /*a_dummy1*/)
{
  MfData::MNW2_Ln2c(Rw,Rskin,Kskin,B,C,P,CWC,LnDesc);
} // MFLIBEXP_MNW2LN2C
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNW2LN2D1 (const int* IL,
                                   const int* IR,
                                   const int* IC,
                                   const double* RwNode,
                                   const double* RskinNode,
                                   const double* KskinNode,
                                   const double* BNode,
                                   const double* CNode,
                                   const double* PNode,
                                   const double* CWCNode,
                                   const double* PP,
                                   const char* LnDesc,
                                   int /*a_dummy1*/)
{
  MfData::MNW2_Ln2d(IL,IR,IC,RwNode,RskinNode,KskinNode,BNode,CNode,PNode,
                    CWCNode,PP,LnDesc,NULL,NULL);
} // MFLIBEXP_MNW2LN2D1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNW2LN2D2 (const double* Ztop,
                                   const double* Zbotm,
                                   const int* IR,
                                   const int* IC,
                                   const double* RwNode,
                                   const double* RskinNode,
                                   const double* KskinNode,
                                   const double* BNode,
                                   const double* CNode,
                                   const double* PNode,
                                   const double* CWCNode,
                                   const double* PP,
                                   const char* LnDesc,
                                   int /*a_dummy1*/)
{
  MfData::MNW2_Ln2d(NULL,IR,IC,RwNode,RskinNode,KskinNode,BNode,CNode,PNode,
                    CWCNode,PP,LnDesc,Ztop,Zbotm);
} // MFLIBEXP_MNW2LN2D2
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNW2LN2E (const int* PUMPLAY,
                                  const int* PUMPROW,
                                  const int* PUMPCOL,
                                  const double* Zpump,
                                  const char* LnDesc,
                                  int /*a_dummy1*/)
{
  MfData::MNW2_Ln2e(PUMPLAY,PUMPROW,PUMPCOL,Zpump,LnDesc);
} // MFLIBEXP_MNW2LN2E
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNW2LN2F (const double* Hlim,
                                  const int* QCUT,
                                  const double* Qfrcmn,
                                  const double* Qfrcmx,
                                  const char* LnDesc,
                                  int /*a_dummy1*/)
{
  MfData::MNW2_Ln2f(Hlim,QCUT,Qfrcmn,Qfrcmx,LnDesc);
} // MFLIBEXP_MNW2LN2F
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNW2LN2G (const double* Hlift,
                                  const double* LIFTq0,
                                  const double* LIFTqdes,
                                  const double* HWtol,
                                  const char* LnDesc,
                                  int /*a_dummy1*/)
{
  MfData::MNW2_Ln2g(Hlift,LIFTq0,LIFTqdes,HWtol,LnDesc);
} // MFLIBEXP_MNW2LN2G
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNW2LN2H (const double* Liftn,
                                  const double* Qn,
                                  const char* LnDesc,
                                  int /*a_dummy1*/)
{
  MfData::MNW2_Ln2h(Liftn,Qn,LnDesc);
} // MFLIBEXP_MNW2LN2H
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNW2LN34 (const int* ITMP,
                                  const double* MNW2,
                                  const int* NMNWVL,
                                  const int* MNWMAX,
                                  const int* NAUX)
{
  MfData::MNW2_Ln34(ITMP,MNW2,NMNWVL,MNWMAX,NAUX);
} // MFLIBEXP_MNW2LN34
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNWILN1 (const int* Wel1flag,
                                 const int* QSUMflag,
                                 const int* BYNDflag)
{
  MfData::MNWI_Ln1(Wel1flag,QSUMflag,BYNDflag);
} // MFLIBEXP_MNWILN1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNWILN2 (const int* MNWOBS)
{
  MfData::MNWI_Ln2(MNWOBS);
} // MFLIBEXP_MNWILN2
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNWILN3 (const char* WELLID,
                                 int a_dummy1,
                                 const int* UNIT,
                                 const int* QNDflag,
                                 const int* QBHflag,
                                 const int* CONCflag)
{
  CStr wellid(util::GetStr(WELLID, a_dummy1));
  MfData::MNWI_Ln3(wellid,UNIT,QNDflag,QBHflag,CONCflag);
} // MFLIBEXP_MNWILN3
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIBEXP_MNWIEND ()
{
  MfData::MNWI_End();
} // MFLIBEXP_MNWIEND

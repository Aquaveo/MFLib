//------------------------------------------------------------------------------
// FILE      MfData.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\MfData.h>

#include <private\MfData\MfExport\MfExporter.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfPackageUtil.h>
#include <private\util\util.h>

//------------------------------------------------------------------------------
/// \brief This gets the current stress period that we are on
//------------------------------------------------------------------------------
bool MfData::PutCurrentPeriod (const int *a_KPER)
{
  if (!a_KPER)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Get().PutCurrentPeriod(*a_KPER);
  return true;
} // MfData::PutCurrentPeriod
//------------------------------------------------------------------------------
/// \brief This gets the current lgr grid
//------------------------------------------------------------------------------
bool MfData::PutCurrentGrid (const int *a_IGRID)
{
  if (!a_IGRID)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Get().IGRID(*a_IGRID);
  return true;
} // MfData::PutCurrentGrid
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the DIS (discretization)
/// package.
//------------------------------------------------------------------------------
bool MfData::InitGlobal (int a_modelType,
                         int a_IGRID,
                         const char *a_exp,
                         const char *a_fileName,
                         const char *a_tables)
{
  MfGlobal::Init(a_modelType, a_IGRID, a_exp, a_fileName, a_tables);
  return true;
} // MfData::Global
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the DIS (discretization)
/// package.
//------------------------------------------------------------------------------
bool MfData::SetGlobal (const int *a_NLAY,
                        const int *a_NROW,
                        const int *a_NCOL,
                        const int *a_NPER)
{
  if (!a_NLAY ||
      !a_NROW ||
      !a_NCOL ||
      !a_NPER)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Set(*a_NROW, *a_NCOL, *a_NLAY, *a_NPER);
  return true;
} // MfData::Global
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the DIS (discretization)
/// package.
//------------------------------------------------------------------------------
bool MfData::SetGlobal (const int *a_NLAY,
                        const int *a_NROW,
                        const int *a_NCOL,
                        const int *a_NPER,
                        const int *a_ITMUNI,
                        const int *a_LENUNI,
                        const int *a_LAYCBD)
{
  if (!a_NLAY ||
      !a_NROW ||
      !a_NCOL ||
      !a_NPER ||
      !a_ITMUNI ||
      !a_LENUNI ||
      !a_LAYCBD)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Set(*a_NROW, *a_NCOL, *a_NLAY, *a_NPER, *a_ITMUNI, *a_LENUNI,
              a_LAYCBD);
  return true;
} // MfData::Global
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the DIS (discretization)
/// package.
//------------------------------------------------------------------------------
bool MfData::DisPackage2 (const Real *a_DELR,
                          const Real *a_DELC,
                          const int *a_NBTOM,
                          const Real *a_BOTM,
                          const Real *a_PERLEN,
                          const int *a_NSTP,
                          const Real *a_TSMULT,
                          const int *a_ISSFLG)
{
  if (!a_DELR ||
      !a_DELC ||
      !a_NBTOM ||
      !a_BOTM ||
      !a_PERLEN ||
      !a_NSTP ||
      !a_TSMULT ||
      !a_ISSFLG)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::DisPackage(a_DELR,
                               a_DELC,
                               a_NBTOM,
                               a_BOTM,
                               a_PERLEN,
                               a_NSTP,
                               a_TSMULT,
                               a_ISSFLG);
  return true;
} // MfData::DisPack
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the List based packages
//------------------------------------------------------------------------------
bool MfData::ListPackage (const char * const a_type,
                          const int *a_ITMP,
                          const int *a_MAXBC,
                          const int *a_NUMBC,
                          const int *a_NUMFIELDS,
                          const int *a_NAUX,
                          const Real *a_DATA,
                          const int *a_NP,
                          const char *a_AUX)
{
  if (!a_MAXBC ||
      !a_NUMBC ||
      !a_NUMFIELDS ||
      !a_NAUX ||
      !a_DATA ||
      !a_NP ||
      !a_AUX)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::ListPackage(a_type,
                                a_ITMP,
                                a_MAXBC,
                                a_NUMBC,
                                a_NUMFIELDS,
                                a_NAUX,
                                a_DATA,
                                a_NP,
                                a_AUX);
  return true;
} // MfData::ListPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the SIP package
//------------------------------------------------------------------------------
bool MfData::SipPackage (const char* const a_type,
                         const int* a_MXITER,
                         const int* a_NPARM,
                         const Real* a_ACCL,
                         const Real* a_HCLOSE,
                         const int* a_IPCALC,
                         const Real* a_WSEED,
                         const int* a_IPRSIP)
{
  if (!a_MXITER ||
      !a_NPARM ||
      !a_ACCL ||
      !a_HCLOSE ||
      !a_IPCALC ||
      !a_WSEED ||
      !a_IPRSIP)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::SipPackage(a_type,
                               a_MXITER,
                               a_NPARM,
                               a_ACCL,
                               a_HCLOSE,
                               a_IPCALC,
                               a_WSEED,
                               a_IPRSIP);
  return true;
} // MfData::SipPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the DE4 package
//------------------------------------------------------------------------------
bool MfData::De4Line1 (const int* a_ITMX,
                       const int* a_MXUP,
                       const int* a_MXLOW,
                       const int* a_MXBW)
{
  if (!a_ITMX ||
      !a_MXUP ||
      !a_MXLOW ||
      !a_MXBW)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::De4Line1(a_ITMX,a_MXUP,a_MXLOW,a_MXBW);
  return true;
} // MfData::De4Line1
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the DE4 package
//------------------------------------------------------------------------------
bool MfData::De4Line2 (const int* a_IFREQ,
                       const int* a_MUTD4,
                       const Real* a_ACCL,
                       const Real* a_HCLOSE,
                       const int* a_IPRD4)
{
  if (!a_IFREQ ||
      !a_MUTD4 ||
      !a_ACCL ||
      !a_HCLOSE ||
      !a_IPRD4)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::De4Line2(a_IFREQ,a_MUTD4,a_ACCL,a_HCLOSE,a_IPRD4);
  return true;
} // MfData::De4Line2
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the SOR package
//------------------------------------------------------------------------------
bool MfData::SorPackage (const char*  const a_type,
                         const int* a_MXITER,
                         const Real* a_ACCL,
                         const Real* a_HCLOSE,
                         const int* a_IPRSIP)
{
  if (!a_MXITER ||
      !a_ACCL ||
      !a_HCLOSE ||
      !a_IPRSIP)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::SorPackage(a_type,
                               a_MXITER,
                               a_ACCL,
                               a_HCLOSE,
                               a_IPRSIP);
  return true;
} // MfData::SorPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the PCG package
//------------------------------------------------------------------------------
bool MfData::PcgPackage (const char*  const a_type,
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
  if (!a_MXITER ||
      !a_ITER1 ||
      !a_NPCOND ||
      !a_HCLOSE ||
      !a_RCLOSE ||
      !a_RELAX ||
      !a_NBPOL ||
      !a_IPRPCG ||
      !a_MUTPCG ||
      !a_DAMP)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::PcgPackage(a_type,
                               a_MXITER,
                               a_ITER1,
                               a_NPCOND,
                               a_HCLOSE,
                               a_RCLOSE,
                               a_RELAX,
                               a_NBPOL,
                               a_IPRPCG,
                               a_MUTPCG,
                               a_DAMP);
  return true;
} // MfData::PcgPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the SOR package
//------------------------------------------------------------------------------
bool MfData::LmgPackage (const char* const a_type,
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
  if (!a_STOR1 ||
      !a_STOR2 ||
      !a_STOR3 ||
      !a_ICG ||
      !a_MXITER ||
      !a_MXCYC ||
      !a_BCLOSE ||
      !a_DAMP ||
      !a_IOUTAMG ||
      !a_DUP ||
      !a_DLOW ||
      !a_HCLOSE ||
      !a_CONTROL)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::LmgPackage(a_type,
                               a_STOR1,
                               a_STOR2,
                               a_STOR3,
                               a_ICG,
                               a_MXITER,
                               a_MXCYC,
                               a_BCLOSE,
                               a_DAMP,
                               a_IOUTAMG,
                               a_DUP,
                               a_DLOW,
                               a_HCLOSE,
                               a_CONTROL);
  return true;
} // MfData::LmgPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the SOR package
//------------------------------------------------------------------------------
bool MfData::GmgPackage (const char* const a_type,
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
  if (!a_RCLOSE ||
      !a_IITER ||
      !a_HCLOSE ||
      !a_MXITER ||
      !a_DAMP ||
      !a_IADAMP ||
      !a_IOUTGMG ||
      !a_ISM ||
      !a_ISC ||
      !a_RELAX)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::GmgPackage(a_type,
                               a_RCLOSE,
                               a_IITER,
                               a_HCLOSE,
                               a_MXITER,
                               a_DAMP,
                               a_IADAMP,
                               a_IOUTGMG,
                               a_ISM,
                               a_ISC,
                               a_RELAX);
  return true;
} // MfData::GmgPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the List based packages
//------------------------------------------------------------------------------
bool MfData::U2DREL (const char * const a_name,
                     const Real *a_data,
                     const Real *a_multiplier,
                     const int *a_LAY,
                     const int *a_IPRN)
{
  if (!a_data ||
      !a_multiplier ||
      !a_LAY ||
      !a_IPRN ||
      strlen(a_name) < 1)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::Array2D(a_name,
                            a_data,
                            a_multiplier,
                            a_LAY,
                            a_IPRN);
  return true;
} // MfData::U2DREL
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the List based packages
//------------------------------------------------------------------------------
bool MfData::U2DREL8 (const char * const a_name,
                      const double *a_data,
                      const Real *a_multiplier,
                      const int *a_LAY,
                      const int *a_IPRN)
{
  if (!a_data ||
      !a_multiplier ||
      !a_LAY ||
      !a_IPRN ||
      strlen(a_name) < 1)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::Array2D8(a_name,
                             a_data,
                             a_multiplier,
                             a_LAY,
                             a_IPRN);
  return true;
} // MfData::U2DREL8
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the List based packages
//------------------------------------------------------------------------------
bool MfData::U2DINT (const char * const a_name,
                     const int *a_data,
                     const int *a_multiplier,
                     const int *a_LAY,
                     const int *a_IPRN)
{
  if (!a_data ||
      !a_multiplier ||
      !a_LAY ||
      !a_IPRN ||
      strlen(a_name) < 1)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::Array2D(a_name,
                            a_data,
                            a_multiplier,
                            a_LAY,
                            a_IPRN);
  return true;
} // MfData::U2DREL
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the EVT and ETS packages
//------------------------------------------------------------------------------
bool MfData::ET (const char *a_PACK,
                  const int *a_NEVTOP,
                  const int *a_INSURF,
                  const int *a_INEVTR,
                  const int *a_INEXDP,
                  const int *a_INIEVT,
                  const int *a_NETSEG,
                  const int *a_INSGDF)
{
  if (!a_PACK ||
      !a_NEVTOP ||
      !a_INSURF ||
      !a_INEVTR ||
      !a_INEXDP ||
      !a_INIEVT ||
      !a_NETSEG ||
      !a_INSGDF)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::ETPackage(a_PACK, a_NEVTOP, a_INSURF, a_INEVTR, a_INEXDP,
                               a_INIEVT, a_NETSEG, a_INSGDF);
  return true;
} // MfData::ET
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the RCH package
//------------------------------------------------------------------------------
bool MfData::RCH (const int *a_NRCHOP,
                  const int *a_INRECH,
                  const int *a_INIRCH)
{
  if (!a_NRCHOP ||
      !a_INRECH ||
      !a_INIRCH)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::RCHPackage(a_NRCHOP, a_INRECH, a_INIRCH);
  return true;
} // MfData::RCH
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Head (const int* a_iper,
                   const int* a_ncol,
                   const int* a_nrow,
                   const int* a_nlay,
                   const Real* a_head)
{
  if (!a_iper ||
      !a_ncol ||
      !a_nrow ||
      !a_nlay ||
      !a_head)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::Head(a_iper, a_ncol, a_nrow, a_nlay, a_head);
  return true;
} // MfData::Head
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::SingleValInt (const char *a_pckg,
                           const char *a_name,
                           const int *a_flag)
{
  if (!a_pckg ||
      !a_name ||
      !a_flag)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::SingleValIntToPack(a_pckg, a_name, a_flag);
  return true;
} // MfData::SingleValInt
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::SingleValFlt (const char *a_pckg,
                           const char *a_name,
                           const Real *a_flag)
{
  if (!a_pckg ||
      !a_name ||
      !a_flag)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::SingleValFltToPack(a_pckg, a_name, a_flag);
  return true;
} // MfData::SingleValFlt
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::SingleValDbl (const char *a_pckg,
                           const char *a_name,
                           const double *a_flag)
{
  if (!a_pckg ||
      !a_name ||
      !a_flag)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::SingleValDblToPack(a_pckg, a_name, a_flag);
  return true;
} // MfData::SingleValDbl
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::SingleValStr (const char *a_pckg,
                           const char *a_name,
                           const char *a_flag)
{
  if (!a_pckg ||
      !a_name ||
      !a_flag)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::SingleValStrToPack(a_pckg, a_name, a_flag);
  return true;
} // MfData::SingleValFlt
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::ExportPack (const char *a_pckg)
{
  MfData::Get().Export(a_pckg);
  return true;
} // MfData::ExportPack
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::LPF1 (const int *NLAY,
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
  if (!NLAY ||
      !ILPFCB ||
      !HDRY ||
      !LAYTYP ||
      !LAYAVG ||
      !CHANI ||
      !LAYVKA ||
      !LAYWET ||
      !VERTLEAKFLAG ||
      !MF2K5)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::LPF1Package(NLAY,ILPFCB,HDRY,LAYTYP,LAYAVG,CHANI,LAYVKA,
                                LAYWET,VERTLEAKFLAG, MF2K5);
  return true;
} // MfData::LPF1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::LPF_OPT (const char* PCK,
                      const int* ISFAC,
                      const int* ICONCV,
                      const int* ITHFLG,
                      const int* NOCVCO,
                      const int* NOVFC)
{
  if (!PCK ||
      !ISFAC ||
      !ICONCV ||
      !ITHFLG ||
      !NOCVCO ||
      !NOVFC)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::LPF_OPT(PCK,ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC);
  return true;
} // MfData::LPF_OPT
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::BCF1 (const int *NLAY,
                   const int *IBCFCB,
                   const Real *HDRY,
                   const int *IWDFLG,
                   const Real *WETFCT,
                   const int *IWETIT,
                   const int *IHDWET,
                   const int *LAYCON,
                   const int *LAYAVG)
{
  if (!NLAY ||
      !IBCFCB ||
      !HDRY ||
      !IWDFLG ||
      !WETFCT ||
      !IWETIT ||
      !IHDWET ||
      !LAYCON ||
      !LAYAVG)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::BCF1Package(NLAY,IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET,
                                LAYCON,LAYAVG);
  return true;
} // MfData::LPF1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::HFB (const int *NHFBNP,
                  const Real *HFBf)
{
  if (!NHFBNP ||
      !HFBf)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::HFBPackage(NHFBNP, HFBf);
  return true;
} // MfData::HFB
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::SEN (const int *ISENALL,
                  const int *IUHEAD,
                  const int *IPRINTS,
                  const int *ISENSU,
                  const int *ISENPU,
                  const int *ISENFM)
{
  if (!ISENALL ||
      !IUHEAD ||
      !IPRINTS ||
      !ISENSU ||
      !ISENPU ||
      !ISENFM)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::SENPackage(ISENALL,IUHEAD,IPRINTS,ISENSU,ISENPU,ISENFM);
  return true;
} // MfData::SEN
bool MfData::PVAL (const char *PARNAM,
									 const Real *B,
									 const int *NPVAL)
{
  if (!PARNAM ||
      !B ||
      !NPVAL)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::PVALPackage(PARNAM,B,NPVAL);
  return true;
} // MfData::PVAL
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::PES (const int *ITMXP,
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
       
  if (!ITMXP ||
      !DMAX ||
      !RTOL ||
      !SOSC ||
      !IBEFLG ||
      !IYCFLG ||
      !IOSTAR ||
      !NOPT ||
      !NFIT ||
      !SOSR ||
      !RMAR ||
      !RMARM ||
      !IAP ||
      !IPRC ||
      !IPRINT ||
      !LPRINT ||
      !CSA ||
      !FCONV ||
      !LASTX)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::PESPackage(ITMXP,DMAX,RTOL,SOSC,IBEFLG,IYCFLG,IOSTAR,NOPT,
                               NFIT,SOSR,RMAR,RMARM,IAP,IPRC,IPRINT,LPRINT,
                               CSA,FCONV,LASTX);
  return true;
} // MfData::PES
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::ObsHd (const char *OBSNAME,
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
  if (!OBSNAME ||
      !LAYER ||
      !ROW ||
      !COL ||
      !IREFSP ||
      !TOFFSET ||
      !ROFF ||
      !COFF ||
      !HOBS ||
      !STAT ||
      !STATFLG ||
      !PLOT)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::ObsHdPackage(OBSNAME,LAYER,ROW,COL,IREFSP,TOFFSET,ROFF,COFF,
                                 HOBS,STAT,STATFLG,PLOT);
  return true;
} // MfData::ObsHd
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::ObsHd4 (const int *MLAY,
                     const Real *PR,
                     const int *ML)
{
  if (!MLAY ||
      !PR ||
      !ML)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::ObsHd4(MLAY,PR,ML);
  return true;
} // MfData::ObsHd4
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::ObsHd5 (const int *ITT)
{
  if (!ITT)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::ObsHd5(ITT);
  return true;
} // MfData::ObsHd5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::ObsHd6 (const char *OBSNAME,
                     const int *IREFSP,
                     const Real *TOFFSET,
                     const Real *HOBS,
                     const Real *STATH,
                     const Real *STATDD,
                     const int *STATFLG,
                     const int *PLOT)
{
  if (!OBSNAME ||
      !IREFSP ||
      !TOFFSET ||
      !HOBS ||
      !STATH ||
      !STATDD ||
      !STATFLG ||
      !PLOT)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::ObsHd6(OBSNAME,IREFSP,TOFFSET,HOBS,STATH,STATDD,
                           STATFLG,PLOT);
  return true;
} // MfData::ObsHd6
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::FloObs4 (const char *OBTYPE,
                      const char *OBSNAME,
                      const int *IREFSP,
                      const Real *TOFFSET,
                      const Real *HOB,
                      const Real *STAT,
                      const int *STATFLG,
                      const int *PLOT)
{
  if (!OBTYPE ||
      !OBSNAME ||
      !IREFSP ||
      !TOFFSET ||
      !HOB ||
      !STAT ||
      !STATFLG ||
      !PLOT)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::FloObs4(OBTYPE,OBSNAME,IREFSP,TOFFSET,HOB,STAT,
                            STATFLG,PLOT);
  return true;
} // MfData::FloObs4
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::FloObs4_5 (const char *OBTYPE,
                        const char *OBSNAME,
                        const int *IREFSP,
                        const Real *TOFFSET,
                        const Real *FLWOBS)
{
  if (!OBTYPE ||
      !OBSNAME ||
      !IREFSP ||
      !TOFFSET ||
      !FLWOBS)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::FloObs4_5(OBTYPE,OBSNAME,IREFSP,TOFFSET,FLWOBS);
  return true;
} // MfData::FloObs4_5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::FloObs5 (const int *NUM,
                      const int *START,
                      const Real *QCLS)
{
  if (!NUM ||
      !START ||
      !QCLS)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::FloObs5(NUM,START,QCLS);
  return true;
} // MfData::FloObs5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::SENParList (const int *NPLIST,
                         const char *PARNAM,
                         const int *ISENS,
                         const int *LN,
                         const Real *B,
                         const Real *BL,
                         const Real *BU,
                         const Real *BSCAL)
{
  if (!NPLIST ||
      !PARNAM ||
      !ISENS ||
      !LN ||
      !B ||
      !BL ||
      !BU ||
      !BSCAL)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::SENParList(NPLIST,PARNAM,ISENS,LN,B,BL,BU,BSCAL);
  return true;
} // MfData::SENParList
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::LAKSP(const int *NSOL,
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
  if (!NSOL ||
      !STAGES ||
      !SSMN ||
      !SSMX ||
      !CLAKE ||
      !ITMP ||
      !ITMP1 ||
      !LWRT ||
      !LKARR ||
      !BDLKNC ||
      !NSLMS ||
      !IC ||
      !ISUB ||
      !SILLVT ||
      !PRCPLK ||
      !EVAPLK ||
      !RNF ||
      !WTHDRW ||
      !CPPT ||
      !CRNF ||
      !CAUG)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::LAKSPPackage(NSOL, STAGES, SSMN, SSMX, CLAKE, ITMP, ITMP1,
                                 LWRT, LKARR, BDLKNC, NSLMS, IC, ISUB, SILLVT,
                                 PRCPLK, EVAPLK, RNF, WTHDRW, CPPT, CRNF,
                                 CAUG);
  return true;
} // mfLibExp_LAKSP
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::STRSP(const int *ITMP,
                   const int *IRDFLG,
                   const int *IPTFLG,
                   const Real *STRM,
                   const int *ISTRM,
                   const int *NSTREM,
                   const int *MXSTRM,
                   const int *ITRBAR,
                   const int *IDIVAR)
{
  if (!ITMP ||
      !IRDFLG ||
      !IPTFLG ||
      !STRM ||
      !ISTRM ||
      !NSTREM ||
      !MXSTRM ||
      !ITRBAR ||
      !IDIVAR)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::STRPackage(ITMP,IRDFLG,IPTFLG,STRM,ISTRM,NSTREM,MXSTRM,
                               ITRBAR,IDIVAR);
  return true;
} // STRSP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool MfData::SFRLine2(const int *ISTRM,
                      int NISTRMD,
                      const Real *STRM,
                      int NSTRMD)
{
  if (!ISTRM ||
      !STRM)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::SFRPackageLine2(ISTRM,NISTRMD,STRM,NSTRMD);
  return true;
} // SFRLine2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool MfData::SFRLine6(const int *ISEG,
                      const int *IOTSG,
                      const int *IDIVAR,
                      const Real *SEG,
                      const Real *XSEC,
                      const Real *QSTAGE)
{
  if (!ISEG ||
      !IOTSG ||
      !IDIVAR ||
      !SEG ||
      !XSEC ||
      !QSTAGE)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::SFRPackageLine6(ISEG,IOTSG,IDIVAR,SEG,XSEC,QSTAGE);
  return true;
} // SFRLine6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool MfData::MnwSetup(const int *MXWEL2,
                      const int *IWL2CB,
                      const int *IWELPT,
                      const int *KSPREF,
                      const double *PLoss, // double precision in MODFLOW
                      const int *IOWELL2,
                      const int* NOMOITER,
                      const char* FTAG,
                      const char* PREFIX,
                      const char* NAMES)
{
  if (!MXWEL2 ||
      !IWL2CB ||
      !IWELPT ||
      !KSPREF ||
      !PLoss ||
      !IOWELL2 ||
      !NOMOITER ||
      !PREFIX ||
      !FTAG ||
      !NAMES ||
      !IOWELL2)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::MnwPackageSetup(MXWEL2,IWL2CB,IWELPT,KSPREF,PLoss,IOWELL2,
                                    NOMOITER,FTAG,PREFIX,NAMES);
  return true;
} // MnwSetup
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool MfData::MnwStressPeriod(const int *ITMP,
                             const int *NWELL2,
                             const double *WELL2, // double precision in MODFLOW
                             const char *MNWSITE,
                             const double *MNWFLGS)
{
  if (!ITMP ||
      !NWELL2 ||
      !WELL2 ||
      !MNWSITE ||
      !MNWFLGS)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::MnwPackageStressPeriod(ITMP,NWELL2,WELL2,MNWSITE,MNWFLGS);
  return true;
} // MnwStressPeriod
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::ListPar (const char *PNAME,
                      const char *PTYPE,
                      Real *PVAL)
{
  if (!PNAME ||
      !PTYPE ||
      !PVAL)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::ListPar(PNAME,PTYPE,PVAL);
  return true;
} // MfData::ListPar
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::ListParData (const char *PNAME,
                          const char *PTYPE,
                          const int *START,
                          const int *LSTDIM,
                          const int *NBC,
                          const int *NVALS,
                          const int *NAUX,
                          const Real *BCDATA,
                          const char *AUXNMS)
{
  if (!PNAME ||
      !PTYPE ||
      !START ||
      !LSTDIM ||
      !NBC ||
      !NVALS ||
      !NAUX ||
      !BCDATA ||
      !AUXNMS)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::ListParData(PNAME,PTYPE,START,LSTDIM,NBC,NVALS,NAUX,
                                BCDATA,AUXNMS);
  return true;
} // MfData::ListParData
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::StreamParInfo (const Real *PVAL,
                            const int *START,
                            const int *NUMBC)
{
  if (!PVAL ||
      !START ||
      !NUMBC)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::StreamParInfo(PVAL,START,NUMBC);
  return true;
} // MfData::ListParData
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::ArrayPar (bool a_hufPar,
                       const char *PNAME,
                       const char *PTYPE,
                       Real *PVAL,
                       const int *NP,
                       const int *IPLOC,
                       const int *IPCLST,
                       const char *MLTNAM,
                       const char *ZONNAM,
                       const char *INAME)
{
  if (!PNAME ||
      !PTYPE ||
      !PVAL ||
      !NP ||
      !IPLOC ||
      !IPCLST ||
      !MLTNAM ||
      !ZONNAM ||
      !INAME)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::ArrayPar(a_hufPar,PNAME,PTYPE,PVAL,NP,IPLOC,IPCLST,
                             MLTNAM,ZONNAM,INAME);
  return true;
} // MfData::ArrayPar
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::ArrayParUsed (const char *PNAME,
                           const char *PTYPE,
                           const char *INAME)
{
  if (!PNAME ||
      !PTYPE ||
      !INAME)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::ArrayParUsed(PNAME,PTYPE,INAME);
  return true;
} // MfData::ArrayParUsed
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::NameFileItem (const char *FTYPE,
                           const char *FNAME,
                           const int *NIU)
{
  if (!FTYPE ||
      !FNAME ||
      !NIU)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::NameFileItem(FTYPE,FNAME,NIU);
  return true;
} // MfData::ArrayParUsed
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Gage (const int* IGGLST, const int* NUMGAGE)
{
  if (!IGGLST)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::Gage(IGGLST, NUMGAGE);
  return true;
} // MfData::Gage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::NameFileFilename (const char *FNAME)
{
  if (!FNAME)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::NameFileFilename(FNAME);
  return true;
} // MfData::NameFileFilename
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::HUF1 (const int *IHUFCB,
                   const Real *HDRY,
                   const int *NHUF,
                   const int *NPHUF,
                   const int *IOHUFHEADS,
                   const int *IOHUFFLOWS,
                   const int *LTHUF,
                   const int *LAYWT)
{
  if (!IHUFCB ||
      !HDRY ||
      !NHUF ||
      !NPHUF ||
      !IOHUFHEADS ||
      !IOHUFFLOWS ||
      !LTHUF ||
      !LAYWT)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::HUF1(IHUFCB,HDRY,NHUF,NPHUF,IOHUFHEADS,IOHUFFLOWS,
                         LTHUF,LAYWT);
  return true;
} // MfData::HUF1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::HufFlag (const int *IHGUFLG)
{
  if (!IHGUFLG)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::HufFlag(IHGUFLG);
  return true;
} // MfData::HufFlag
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::HufWet (const Real *WETFCT,
                     const int *IWETIT,
                     const int *IHDWET)
{
  if (!WETFCT ||
      !IWETIT ||
      !IHDWET)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::HufWet(WETFCT,IWETIT,IHDWET);
  return true;
} // MfData::HufWet
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::HufHani (const char *HGUNAM,
                      const Real *HGUHANI,
                      const Real *HGUVANI)
{
  if (!HGUNAM ||
      !HGUHANI ||
      !HGUVANI)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::HufHani(HGUNAM,HGUHANI,HGUVANI);
  return true;
} // MfData::HufHani
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::UZFLine1(int *NUZTOP,
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
  if (!NUZTOP ||
      !IUZFOPT ||
      !IRUNFLG ||
      !IETFLG ||
      !IUZFCB1 ||
      !IUZFCB2 ||
      !NTRAIL2 ||
      !NSETS2 ||
      !NUZGAG ||
      !SURFDEP)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::UZFPackageLine1(NUZTOP,IUZFOPT,IRUNFLG,IETFLG,IUZFCB1,
                                    IUZFCB2,NTRAIL2,NSETS2,NUZGAG,SURFDEP);
  return true;
} // MfData::UZFLine1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::UZFLine8(int *IUZLIST)
{
  if (!IUZLIST)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::UZFPackageLine8(IUZLIST);
  return true;
} // UZFLine8
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::UZFStressPeriod(int *NUZF1,
                             int *NUZF2,
                             int *NUZF3,
                             int *NUZF4)
{
  if (!NUZF1 ||
      !NUZF2 ||
      !NUZF3 ||
      !NUZF4)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::UZFPackageSP(NUZF1,NUZF2,NUZF3,NUZF4);
  return true;
} // UZFStressPeriod
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::VDFLine5(const int *MT3DRHOFLG,
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
  if (!MT3DRHOFLG ||
      !MFNADVFD ||
      !NSWTCPL ||
      !IWTABLE ||
      !DENSEMIN ||
      !DENSEMAX ||
      !DNSCRIT ||
      !DENSEREF ||
      !DRHODC ||
      !DRHODPRHD ||
      !PRHDREF ||
      !NSRHOEOS ||
      !MTRHOSPEC ||
      !CRHOREF ||
      !FIRSTDT)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::VDFPackageLine5(MT3DRHOFLG,MFNADVFD,NSWTCPL,IWTABLE,
                                    DENSEMIN,DENSEMAX,DNSCRIT,DENSEREF,DRHODC,
                                    DRHODPRHD,PRHDREF,NSRHOEOS,MTRHOSPEC,
                                    CRHOREF,FIRSTDT);
  return true;
} // MfData::VDFLine5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::VDFStressPeriod(const int *INDENSE)
{
  if (!INDENSE)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::VDFPackageStressPeriod(INDENSE);
  return true;
} // MfData::VDFStressPeriod
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::VSCLine3(const int *MT3DMUFLG,
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
  if (!MT3DMUFLG ||
      !VISCMIN ||
      !VISCMAX ||
      !VISCREF ||
      !DMUDC ||
      !CMUREF ||
      !NSMUEOS ||
      !MUTEMPOPT ||
      !MTMUSPEC ||
      !MTMUTEMPSPEC ||
      !AMUCOEFF)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::VSCPackageLine3(MT3DMUFLG,VISCMIN,VISCMAX,VISCREF,DMUDC,
                                    CMUREF,NSMUEOS,MUTEMPOPT,MTMUSPEC,
                                    MTMUTEMPSPEC,AMUCOEFF);
  return true;
} // MfData::VSCLine3
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::VSCStressPeriod(const int *INVISC)
{
  if (!INVISC)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::VSCPackageStressPeriod(INVISC);
  return true;
} // MfData::VSCStressPeriod
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::NwtLn1 (const Real* toldum,
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
  if (!toldum ||
      !ftoldum ||
      !Mxiter ||
      !Thickdum ||
      !Linmeth ||
      !IPRNWT ||
      !IBOTAV ||
      !IFDPARAM ||
      !thetadum ||
      !akappadum ||
      !gammadum ||
      !amomentdum ||
      !Btrack ||
      !Numtrack ||
      !Btoldum ||
      !Breducdum
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::NwtLn1(toldum,ftoldum,Mxiter,Thickdum,Linmeth,IPRNWT,IBOTAV,
                 IFDPARAM,thetadum,akappadum,gammadum,amomentdum,Btrack,
                 Numtrack,Btoldum,Breducdum);
  return true;
} // MfData::NwtLn1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::NwtLn2 (const int* IACL,
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
  if (!IACL ||
      !NORDER ||
      !LEVEL ||
      !NORTH ||
      !IREDSYS ||
      !RRCTOLS ||
      !IDROPTOL ||
      !EPSRNS ||
      !HCLOSEXMDDUM ||
      !MXITERXMD
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::NwtLn2(IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOLS,
                           IDROPTOL,EPSRNS,HCLOSEXMDDUM,MXITERXMD);
  return true;
} // MfData::NwtLn2
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::NwtLn2a (const int* Maxitr_gmres,
                      const int* Ilu_method,
                      const int* Lev_fill,
                      const Real* Stop_toldum,
                      const int* Msdr)
{
  if (!Maxitr_gmres ||
      !Ilu_method ||
      !Lev_fill ||
      !Stop_toldum ||
      !Msdr
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::NwtLn2a(Maxitr_gmres,Ilu_method,Lev_fill,Stop_toldum,Msdr);
  return true;
} // MfData::NwtLn2a
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::UPW1 (const int* NLAY,
                   const int* IUPWCB,
                   const Real* HDRY,
                   const int* IPHDRY,
                   const int* LAYTYPUPW,
                   const int* LAYAVG,
                   const Real* CHANI,
                   const int* LAYVKAUPW,
                   const int* LAYWET)
{
  if (!NLAY ||
      !IUPWCB ||
      !HDRY ||
      !IPHDRY ||
      !LAYTYPUPW ||
      !LAYAVG ||
      !CHANI ||
      !LAYVKAUPW ||
      !LAYWET
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::UPW1(NLAY,IUPWCB,HDRY,IPHDRY,LAYTYPUPW,LAYAVG,CHANI,
                         LAYVKAUPW,LAYWET);
  return true;
} // MfData::NwtLn2a
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Comment (const char* a_pack,
                      const char* a_line)
{
  MfData::Packages::Comment(a_pack, a_line);
  return true;
} // MfData::Comment
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNW2_Ln1 (const int* MNWMAX,
                       const int* IWL2CB,
                       const int* MNWPRNT,
                       const int* NAUX,
                       const char* MNWAUX)
{
  if (
      !MNWMAX ||
      !IWL2CB ||
      !MNWPRNT ||
      !NAUX ||
      !MNWAUX
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::MNW2_Ln1(MNWMAX,IWL2CB,MNWPRNT,NAUX,MNWAUX);
  return true;
} // MfData::MNW2_Ln1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNW2_Ln2ab (const char* WELLID,
                         const int* NNODES,
                         const char* LOSSTYPE,
                         const int* PUMPLOC,
                         const int* Qlimit,
                         const int* PPFLAG,
                         const int* PUMPCAP)
{
  if (
      !WELLID ||
      !NNODES ||
      !LOSSTYPE ||
      !PUMPLOC ||
      !Qlimit ||
      !PPFLAG ||
      !PUMPCAP
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::MNW2_Ln2ab(WELLID,NNODES,LOSSTYPE,PUMPLOC,Qlimit,
                               PPFLAG,PUMPCAP);
  return true;
} // MfData::MNW2_Ln2ab
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNW2_Ln2c (const double* Rw,
                        const double* Rskin,
                        const double* Kskin,
                        const double* B,
                        const double* C,
                        const double* P,
                        const double* CWC,
                        const char* LnDesc)
{
  if (
      !Rw ||
      !Rskin ||
      !Kskin ||
      !B ||
      !P ||
      !C ||
      !CWC ||
      !LnDesc
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::MNW2_Ln2c(Rw,Rskin,Kskin,B,C,P,CWC,LnDesc);
  return true;
} // MfData::MNW2_Ln2c
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNW2_Ln2d (const int* IL,
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
                        const double* Ztop,
                        const double* Zbotm)
{
  if (!IL && (!Ztop || !Zbotm))
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  if (
      !IR ||
      !IC ||
      !RwNode ||
      !RskinNode ||
      !KskinNode ||
      !BNode ||
      !PNode ||
      !CNode ||
      !CWCNode ||
      !PP ||
      !LnDesc
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::MNW2_Ln2d(IL,IR,IC,RwNode,RskinNode,KskinNode,BNode,
                              CNode,PNode,CWCNode,PP,LnDesc,Ztop,Zbotm);
  return true;
} // MfData::MNW2_Ln2c
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNW2_Ln2e (const int* PUMPLAY,
                        const int* PUMPROW,
                        const int* PUMPCOL,
                        const double* Zpump,
                        const char* LnDesc)
{
  if (
      !PUMPLAY ||
      !PUMPROW ||
      !PUMPCOL ||
      !Zpump ||
      !LnDesc
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::MNW2_Ln2e(PUMPLAY,PUMPROW,PUMPCOL,Zpump,LnDesc);
  return true;
} // MfData::MNW2_Ln2e
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNW2_Ln2f (const double* Hlim,
                        const int* QCUT,
                        const double* Qfrcmn,
                        const double* Qfrcmx,
                        const char* LnDesc)
{
  if (
      !Hlim ||
      !QCUT ||
      !Qfrcmn ||
      !Qfrcmx ||
      !LnDesc
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::MNW2_Ln2f(Hlim,QCUT,Qfrcmn,Qfrcmx,LnDesc);
  return true;
} // MfData::MNW2_Ln2f
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNW2_Ln2g (const double* Hlift,
                        const double* LIFTq0,
                        const double* LIFTqdes,
                        const double* HWtol,
                        const char* LnDesc)
{
  if (
      !Hlift ||
      !LIFTq0 ||
      !LIFTqdes ||
      !HWtol ||
      !LnDesc
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::MNW2_Ln2g(Hlift,LIFTq0,LIFTqdes,HWtol,LnDesc);
  return true;
} // MfData::MNW2_Ln2g
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNW2_Ln2h (const double* Liftn,
                        const double* Qn,
                        const char* LnDesc)
{
  if (
      !Liftn ||
      !Qn ||
      !LnDesc
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::MNW2_Ln2h(Liftn,Qn,LnDesc);
  return true;
} // MfData::MNW2_Ln2h
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNW2_Ln34 (const int* ITMP,
                        const double* MNW2,
                        const int* NMNWVL,
                        const int* MNWMAX,
                        const int* NAUX)
{
  if (
      !ITMP ||
      !MNW2 ||
      !NMNWVL ||
      !MNWMAX ||
      !NAUX
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  MfData::Packages::MNW2_Ln34(ITMP,MNW2,NMNWVL,MNWMAX,NAUX);
  return true;
} // MfData::MNW2_Ln2h
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNWI_Ln1 (const int* Wel1flag,
                       const int* QSUMflag,
                       const int* BYNDflag)
{
  if (
      !Wel1flag ||
      !QSUMflag ||
      !BYNDflag
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::MNWI_Ln1(Wel1flag,QSUMflag,BYNDflag);
  return true;
} // MfData::MNWI_Ln1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNWI_Ln2 (const int* MNWOBS)
{
  if (
      !MNWOBS
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::MNWI_Ln2(MNWOBS);
  return true;
} // MfData::MNWI_Ln2
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNWI_Ln3 (const char* WELLID,
                       const int* UNIT,
                       const int* QNDflag,
                       const int* QBHflag,
                       const int* CONCflag)
{
  if (
      !WELLID ||
      !UNIT ||
      !QNDflag ||
      !QBHflag ||
      !CONCflag
      )
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  MfData::Packages::MNWI_Ln3(WELLID,UNIT,QNDflag,QBHflag,CONCflag);
  return true;
} // MfData::MNWI_Ln3
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::MNWI_End ()
{
  MfData::Packages::MNWI_End();
  return true;
} // MfData::MNWI_End


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData.t.h>
#include <sstream>

//------------------------------------------------------------------------------
void MfDataT::testGlobal ()
{
  ErrorStack::Get().ClearErrors();

  CStr str, e, f;
  int *iptr(NULL);
  int nlay(2), nrow(4), ncol(5), nper(3), itmuni(7), lenuni(8), laycbd[2]={0,0};
  TS_ASSERT(!MfData::SetGlobal(iptr, &nrow, &ncol, &nper));
  TS_ASSERT(!MfData::SetGlobal(&nlay, iptr, &ncol, &nper));
  TS_ASSERT(!MfData::SetGlobal(&nlay, &nrow, iptr, &nper));
  TS_ASSERT(!MfData::SetGlobal(&nlay, &nrow, &ncol, iptr));

  TS_ASSERT(!MfData::SetGlobal(&nlay, iptr, &ncol, &nper, &itmuni, &lenuni,laycbd));
  TS_ASSERT(!MfData::SetGlobal(&nlay, &nrow, iptr, &nper, &itmuni, &lenuni,laycbd));
  TS_ASSERT(!MfData::SetGlobal(&nlay, &nrow, &ncol, iptr, &itmuni, &lenuni,laycbd));
  TS_ASSERT(!MfData::SetGlobal(&nlay, &nrow, &ncol, &nper, iptr, &lenuni,laycbd));
  TS_ASSERT(!MfData::SetGlobal(&nlay, &nrow, &ncol, &nper, &itmuni, iptr,laycbd));
  std::stringstream stream;
  ErrorStack::Get().PrintErrors(stream);
  char myChar[100];
  stream.getline(myChar, 100);
  stream.getline(myChar, 100);
  str = myChar;
  TS_ASSERT(!str.IsEmpty());

  ErrorStack::Get().ClearErrors();
  TS_ASSERT(MfData::InitGlobal(MfData::MF2K, -1, e, f, ""));
  TS_ASSERT(MfData::SetGlobal(&nlay, &nrow, &ncol, &nper));
  TS_ASSERT(MfData::SetGlobal(&nlay, &nrow, &ncol, &nper, &itmuni, &lenuni,laycbd));
  std::stringstream stream1;
  ErrorStack::Get().PrintErrors(stream1);
  stream1.getline(myChar, 100);
  str = myChar;
  TS_ASSERT(str.IsEmpty());
  MfData::SetGlobal(&nlay, &nrow, &ncol, &nper);
  TS_ASSERT_EQUALS(MfData::Get().NumLay(), 2);
  TS_ASSERT_EQUALS(MfData::Get().NumRow(), 4);
  TS_ASSERT_EQUALS(MfData::Get().NumCol(), 5);
  
  nlay = 3;
  nrow = 5;
  ncol = 6;
  nper = 2;
  MfData::InitGlobal(MfData::MF2K5, -1, e, f, "");
  MfData::SetGlobal(&nlay, &nrow, &ncol, &nper, &itmuni, &lenuni,laycbd);
  TS_ASSERT_EQUALS(MfData::Get().ModelType(), 1);
  TS_ASSERT_EQUALS(MfData::Get().NumLay(), 3);
  TS_ASSERT_EQUALS(MfData::Get().NumRow(), 5);
  TS_ASSERT_EQUALS(MfData::Get().NumCol(), 6);
  TS_ASSERT_EQUALS(MfData::Get().NumPeriods(), 2);
  TS_ASSERT_EQUALS(MfData::Get().TimeUnit(), 7);
  TS_ASSERT_EQUALS(MfData::Get().LengthUnit(), 8);

  MfData::InitGlobal(MfData::MF2K, -1, e, f, "");
}
//------------------------------------------------------------------------------
void MfDataT::testDispPackage2 ()
{
  int nlay(2), nrow(4), ncol(5), nper(3), itmuni(7), lenuni(8);
  CStr e, f;
  MfData::InitGlobal(MfData::MF2K, -1, e, f, "");

  int nbotm(3);
  int *iptr(NULL);
  Real *fptr(NULL);
  std::vector<int> laycbd(nlay), nstp(nper, 2), issflg(nper, 0);
  std::vector<Real> delr(ncol), delc(nrow), botm(nrow*ncol*nbotm),
                     perlen(nper, 5), tsmult(nper, 1);
  MfData::SetGlobal(&nlay, &nrow, &ncol, &nper, &itmuni, &lenuni,&laycbd[0]);

  TS_ASSERT(!MfData::DisPackage2(fptr, &delc[0], &nbotm, &botm[0],
                                 &perlen[0], &nstp[0], &tsmult[0], &issflg[0]))
  TS_ASSERT(!MfData::DisPackage2(&delr[0], fptr, &nbotm, &botm[0],
                                 &perlen[0], &nstp[0], &tsmult[0], &issflg[0]))
  TS_ASSERT(!MfData::DisPackage2(&delr[0], &delc[0], iptr, &botm[0],
                                 &perlen[0], &nstp[0], &tsmult[0], &issflg[0]))
  TS_ASSERT(!MfData::DisPackage2(&delr[0], &delc[0], &nbotm, fptr,
                                 &perlen[0], &nstp[0], &tsmult[0], &issflg[0]))
  TS_ASSERT(!MfData::DisPackage2(&delr[0], &delc[0],&nbotm,&botm[0],
                                 fptr, &nstp[0], &tsmult[0], &issflg[0]))
  TS_ASSERT(!MfData::DisPackage2(&delr[0], &delc[0],&nbotm,&botm[0],
                                 &perlen[0], iptr, &tsmult[0], &issflg[0]))
  TS_ASSERT(!MfData::DisPackage2(&delr[0], &delc[0],&nbotm,&botm[0],
                                 &perlen[0], &nstp[0], fptr, &issflg[0]));
  TS_ASSERT(!MfData::DisPackage2(&delr[0], &delc[0],&nbotm,&botm[0],
                                 &perlen[0], &nstp[0], &tsmult[0], iptr));

  //TS_ASSERT(MfData::DisPackage2(&delr[0], &delc[0], &nbotm,&botm[0],
  //                              &perlen[0], &nstp[0], &tsmult[0], &issflg[0]));
}

#endif

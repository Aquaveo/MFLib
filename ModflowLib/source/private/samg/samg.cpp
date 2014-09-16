//------------------------------------------------------------------------------
// FILE      samg.cpp
// PURPOSE   a class that wraps calls to a dll that we late load so that
//           we don't have compiler dependencies
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/samg/samg.h>

// this is a class that wraps calls to a dll
LATELOAD_BEGIN_CLASS(BaseDll,SAMG,false,FALSE)
  LATELOAD_FUNC_0_VOID(STDMETHODVCALLTYPE,SAMG_REL_LIC)
  LATELOAD_FUNC_16_VOID(STDMETHODVCALLTYPE,LMG_MFUSG,double*,double*,double*,int*,int*,int*,int*,int*,int*,int*,int*,double*,int*,int*,int*,int*)
  LATELOAD_FUNC_21_VOID(STDMETHODVCALLTYPE,LMG1ALSAMG,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,Real*,Real*,Real*,char*)
  LATELOAD_FUNC_14_VOID(STDMETHODVCALLTYPE,LMG1RPSAMG,int*,int*,Real*,Real*,Real*,int*, int*, int*,Real*,Real*,Real*,int*,int*,char*)
  LATELOAD_FUNC_43_VOID(STDMETHODVCALLTYPE,LMG1APSAMG,double*,int*,Real*,Real*,Real*,Real*,Real*,double*,int*,int*,double*,double*,int*,int*,int*,int*,int*,int*,Real*,Real*,int*,int*,int*,int*,int*,int*,int*,int*,int*,Real*,int*,int*,int*,Real*,Real*,int*,int*,Real*,Real*,int*,int*,int*,char*)
LATELOAD_END_CLASS()

//------------------------------------------------------------------------------
/// \brief internal functions
//------------------------------------------------------------------------------
namespace
{
//------------------------------------------------------------------------------
static bool& SamgCalled ()
{
  static bool m_(false);
  return m_;
} // SamgCalled
//------------------------------------------------------------------------------
static BaseDll& dll ()
{
  static BaseDll m_;
  char name[5001];
  GetModuleFileName(NULL, name, 5000);
  CStr fname, dllName;
  util::StripPathFromFilename(name, fname);

  std::string nm = "SAMG";
  if (!m_.dll_IsLoaded())
  {
    bool win64 = sizeof(int*) == 8 ? 1 : 0;
    bool openMp = fname.Find("_par") != -1 ? 1 : 0;
    bool dblprec = sizeof(Real) == sizeof(double) ? 1 : 0;
    if (!dblprec)
    {
      if      ( win64 &&  openMp) dllName = "samg_par_64";
      else if (!win64 &&  openMp) dllName = "samg_par";
      else if ( win64 && !openMp) dllName = "samg_64";
      else                        dllName = "samg";
    }
    else
    {
      if      ( win64 &&  openMp) dllName = "samg_dbl_par_64";
      else if (!win64 &&  openMp) dllName = "samg_dbl_par";
      else if ( win64 && !openMp) dllName = "samg_dbl_64";
      else                        dllName = "samg_dbl";
    }
    m_.dll_LoadLibraryFromName(dllName.c_str());
    if (!m_.dll_IsLoaded())
    {
      printf("Unable to load dll %s.\n", dllName.c_str());
    }
  }
  return m_;
} // dll
//------------------------------------------------------------------------------
static bool DoChecksFirst ()
{
  if (!dll().dll_IsLoaded())
  {
    return false;
  }
  SamgCalled() = true;
  return true;
} // DoChecksFirst
} // unnamed namespace
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void SamgReleaseLicense ()
{
  if (SamgCalled()) dll().SAMG_REL_LIC();
} // SamgReleaseLicense
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void samgUsg (double* A, double* RHS, double* HNEW, int* IA, int* JA, int* NNA,
              int* NNU, int* KPER, int* KSTP, int* ncyc, int* NCYC_DONE,
              double* EPSSAMG, int* IBOUND, int* SAMGLOG, int* IERR,
              int* aqLicense)
{
  if (!DoChecksFirst())
  {
    *IERR = -1;
    return;
  }
  dll().LMG_MFUSG(A,RHS,HNEW,IA,JA,NNA,NNU,KPER,KSTP,ncyc,NCYC_DONE,EPSSAMG,
                   IBOUND,SAMGLOG,IERR,aqLicense);
} // samgUsg
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void samgLMG1ALSAMG (int* ISUM, int* ISUMI, int* LCA, int* LCIA, int* LCJA,
                     int* LCU1, int* LCFRHS, int* LCIG, int* ISIZ1, int* ISIZ2,
                     int* ISIZ3, int* ISIZ4, int* ICG, int* NCOL, int* NROW,
                     int* NLAY, int* samg_logio, Real* stor1, Real* stor2,
                     Real* stor3, char* samg_logfile)
{
  if (!DoChecksFirst()) return;
  dll().LMG1ALSAMG(ISUM,ISUMI,LCA,LCIA,LCJA,LCU1,LCFRHS,LCIG,ISIZ1,ISIZ2,ISIZ3,
                    ISIZ4,ICG,NCOL,NROW,NLAY,samg_logio,stor1,stor2,stor3,
                    samg_logfile);
} // samgLMG1ALSAMG
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void samgLMG1RPsamg (
  int* MXITER, int* MXCYC, Real* rcloselmg, Real* damplmg, Real* damplmgt,
  int* ioutamg, int* ICG, int* IADAMPlmg, Real* DUPlmg, Real* DLOWlmg,
  Real* HCLOSE, int* CONTROLlmg, int* samg_logio, char* SAMG_LOGFILE)
{
  if (!DoChecksFirst()) return;
  dll().LMG1RPSAMG(MXITER,MXCYC,rcloselmg,damplmg,damplmgt,ioutamg,
                    ICG,IADAMPlmg,DUPlmg,DLOWlmg,HCLOSE,CONTROLlmg,
                    samg_logio,SAMG_LOGFILE);
} // samgLMG1RPsamg
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void samgLMG1APsamg (
  double* HNEW,int* IBOUND,Real* CR,Real* CC,Real* CV,Real* HCOF,Real* RHS,     //7
  double* A,int* IA,int* JA,double* U,double* FRHS,int* IG,int* ISIZ1,          //7
  int* ISIZ2,int* ISIZ3,int* ISIZ4,int* KITER,Real* BCLOSE,Real* DAMP,          //6
  int* ICNVG,int* KSTP,int* KPER,int* MXITER,int* MXCYC,int* NCOL,int* NROW,    //7
  int * NLAY,int* NODES,Real* HNOFLO,int* IOUTAMG,int* ICG,int* IADAMP,         //6
  Real* DUP,Real* DLOW,int* samg_logio,int* IHCOFADD,Real* start_res,           //5
  Real* end_res,int* iter_done,int* setup_done,int* iLicense,char* samg_logfile)//5
{
  if (!DoChecksFirst()) return;
  dll().LMG1APSAMG(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,A,IA,JA,U,FRHS,IG,ISIZ1,ISIZ2,
                    ISIZ3,ISIZ4,KITER,BCLOSE,DAMP,ICNVG,KSTP,KPER,MXITER,MXCYC,
                    NCOL,NROW,NLAY,NODES,HNOFLO,IOUTAMG,ICG,IADAMP,DUP,DLOW,
                    samg_logio,IHCOFADD,start_res,end_res,iter_done,setup_done,
                    iLicense, samg_logfile);
} // samgLMG1APsamg



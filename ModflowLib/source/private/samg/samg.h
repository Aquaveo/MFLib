//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SAMG_H
#define SAMG_H
#include <private/util/util.h>

void SamgReleaseLicense();
void samgUsg(double* A,
             double* RHS,
             double* HNEW,
             int*    IA,
             int*    JA,
             int*    NNA,
             int*    NNU,
             int*    KPER,
             int*    KSTP,
             int*    ncyc,
             int*    NCYC_DONE,
             double* EPSSAMG,
             int*    IBOUND,
             int*    SAMGLOG,
             int*    IERR,
             int*    aqLicense);
void samgLMG1ALSAMG(int* ISUM,
                    int* ISUMI,
                    int* LCA,
                    int* LCIA,
                    int* LCJA,
                    int* LCU1,
                    int* LCFRHS,
                    int* LCIG,
                    int* ISIZ1,
                    int* ISIZ2,
                    int* ISIZ3,
                    int* ISIZ4,
                    int* ICG,
                    int* NCOL,
                    int* NROW,
                    int* NLAY,
                    int* samg_logio,
                    Real* stor1,
                    Real* stor2,
                    Real* stor3,
                    char* samg_logfile);
void samgLMG1RPsamg(int* MXITER,
                    int* MXCYC,
                    Real* rcloselmg,
                    Real* damplmg,
                    Real* damplmgt,
                    int* ioutamg,
                    int* ICG,
                    int* IADAMPlmg,
                    Real* DUPlmg,
                    Real* DLOWlmg,
                    Real* HCLOSE,
                    int* CONTROLlmg,
                    int* samg_logio,
                    char* SAMG_LOGFILE);
void samgLMG1APsamg(double* HNEW,
                    int* IBOUND,
                    Real* CR,
                    Real* CC,
                    Real* CV,
                    Real* HCOF,
                    Real* RHS,
                    double* A,
                    int* IA,
                    int* JA,
                    double* U,
                    double* FRHS,
                    int* IG,
                    int* ISIZ1,
                    int* ISIZ2,
                    int* ISIZ3,
                    int* ISIZ4,
                    int* KITER,
                    Real* BCLOSE,
                    Real* DAMP,
                    int* ICNVG,
                    int* KSTP,
                    int* KPER,
                    int* MXITER,
                    int* MXCYC,
                    int* NCOL,
                    int* NROW,
                    int * NLAY,
                    int* NODES,
                    Real* HNOFLO,
                    int* IOUTAMG,
                    int* ICG,
                    int* IADAMP,
                    Real* DUP,
                    Real* DLOW,
                    int* samg_logio,
                    int* IHCOFADD,
                    Real* start_res,
                    Real* end_res,
                    int* iter_done,
                    int* setup_done,
                    int* iLicense,
                    char* samg_logfile);


#endif

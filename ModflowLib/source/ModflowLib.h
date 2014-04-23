//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MODFLOWLIB_H
#define MODFLOWLIB_H

// use "Real" instead of "float" or "double" so switching is easy
#ifndef Real
  #ifdef DBLPREC // DBLPREC would be defined in the project settings
    #define Real double
  #else
    #define Real float
  #endif
#endif

#if MFLIB_DLL
#define DLLEXPORT __declspec( dllexport )
#else
#define DLLEXPORT 
#endif

extern "C"
{
DLLEXPORT void MFLIB_TESTNOARG();
DLLEXPORT void MFLIB_TESTARG(int *a_i);
DLLEXPORT void __stdcall MFLIB_TESTCNOARG();
DLLEXPORT void __stdcall MFLIB_TESTCARG(int *a_i);
DLLEXPORT void MFLIB_CDECSTRING(char *a_c,
                                int a_len);
DLLEXPORT void __stdcall MFLIB_STDDECSTRING(char *a_c,
                                            int a_len);

DLLEXPORT void MFLIB_CHECKMEMORY();
DLLEXPORT void MFLIB_CLOSEALLH5FILES();
DLLEXPORT void MFLIB_SETMFFILEPATH(const char* a_path,
                                   int a_len);
DLLEXPORT void MFLIB_GETMFFILEPATH (char* a_path,
                                    int a_len);
DLLEXPORT void MFLIB_SETMT3DPATH(const char* a_path,
                                 int a_len);
DLLEXPORT void MFLIB_GETMT3DPATH(char* a_path,
                                 int a_len);
DLLEXPORT void MFLIB_SETPARFNAME(const char* a_fName,
                                 int a_fNameLen);
DLLEXPORT void MFLIB_SETSENFNAME(const char* a_fName,
                                 int a_fNameLen);
DLLEXPORT void MFLIB_U2DDBL(int *a_SUCCESS,
                            int *a_IPRN,
                            const int *a_I,
                            const int *a_J,
                            double *a_arr,
                            const char *a_line,
                            int a_lineLen);
DLLEXPORT void MFLIB_AREALOPT_EXDATA(const int* a_OPT,
                                     const char* a_NAME,
                                     int a_NAMElen);
DLLEXPORT void MFLIB_U2DINT_EXDATA(const char* a_NAME,
                                   int a_NAMElen,
                                   const int* a_ARR,
                                   const int* a_MULT,
                                   const int* a_K,
                                   const int* a_JJ,
                                   const int* a_II);
DLLEXPORT void MFLIB_U2DREL_EXDATA(const char* a_NAME,
                                   int a_NAMElen,
                                   const Real* a_ARR,
                                   const Real* a_MULT,
                                   const int* a_K,
                                   const int* a_JJ,
                                   const int* a_II);
DLLEXPORT void MFLIB_U2DREL8_EXDATA(const char* a_NAME,
                                    int a_NAMElen,
                                    const double* a_ARR,
                                    const Real* a_MULT,
                                    const int* a_K,
                                    const int* a_JJ,
                                    const int* a_II);
DLLEXPORT void MFLIB_U2DREL(int *a_SUCCESS,
                            int *a_IPRN,
                            const int *a_I,
                            const int *a_J,
                            Real *a_arr,
                            const char *a_line,
                            int a_lineLen,
                            const char* a_name,
                            int a_nameLen);
DLLEXPORT void MFLIB_U2DREL8(int *a_SUCCESS,
                             int *a_IPRN,
                             const int *a_I,
                             const int *a_J,
                             double *a_arr,
                             const char *a_line,
                             int a_lineLen,
                             const char* a_name,
                             int a_nameLen);
DLLEXPORT void MFLIB_U2DREL2(int *a_SUCCESS,
                            int *a_IPRN,
                            const int *a_I,
                            const int *a_J,
                            Real *a_arr,
                            const char *a_line,
                            int a_lineLen,
                            const char* a_path,
                            int a_pathLen);
DLLEXPORT void MFLIB_U2DINT(int *a_SUCCESS,
                            int *a_IPRN,
                            const int *a_I,
                            const int *a_J,
                            int *a_arr,
                            const char *a_line,
                            int a_lineLen);
DLLEXPORT void MFLIB_SQLITE_U2DINT(int *a_SUCCESS,
                                   int *a_IPRN,
                                   const int *a_I,
                                   const int *a_J,
                                   int *a_arr,
                                   const char *a_line,
                                   int a_lineLen);
DLLEXPORT void MFLIB_ULSTRD(int *a_SUCCESS,
                            const int* a_NLIST,
                            const int *a_LDIM,
                            const int *a_IAL,
                            const int *a_NAUX,
                            const char *a_CAUX,
                            int a_dummy,
                            const int *a_NCOL,
                            const int *a_NROW,
                            Real *a_RLIST,
                            const char *a_LINE,
                            int a_LINElen);
DLLEXPORT void MFLIB_ULSTRD_DBL(int *a_SUCCESS,
                                const int* a_NLIST,
                                const int *a_LDIM,
                                const int *a_IAL,
                                const int *a_NAUX,
                                const char *a_CAUX,
                                int a_dummy,
                                const int *a_NCOL,
                                const int *a_NROW,
                                double *a_RLIST,
                                const char *a_LINE,
                                int a_LINElen);
DLLEXPORT void MFLIB_READSTR(int *a_SUCCESS,
                             const int *a_NSTREM,
                             const int *a_NSS,
                             const int *a_NTRIB,
                             const int *a_NCOL,
                             const int *a_NROW,
                             Real *a_STRM,
                             int *a_ISTRM,
                             int *a_ITRBAR,
                             int *a_IDVIAR,
                             const char *a_LINE,
                             int a_LINElen);
DLLEXPORT void MFLIB_READMNW(int *a_SUCCESS,
                             const int *a_ITMP,
                             double *a_WELL2,
                             char *a_MNWSITE,
                             int /*a_MNWSITElen*/,
                             double *a_MNWFLGS,
                             const char *a_LINE,
                             int a_LINElen);
DLLEXPORT void MFLIB_SFR2REACH(const int* ii,
                               const int* NROW,
                               const int* NCOL,
                               int* krch,
                               int* irch,
                               int* jrch,
                               int* jseg,
                               int* ireach,
                               Real* Strm,
                               int NStrmD,
                               const char *line,
                               int a_dummy);
DLLEXPORT void MFLIB_SFR(const int* Nlst,
                         const int* Kper,
                         int* Iseg,
                         int* lotsg,
                         int* Idivar,
                         Real* Seg,
                         Real* Xsec,
                         Real* Qstage,
                         const char *line,
                         int a_dummy);
DLLEXPORT void MFLIB_FILLINPARTYPE(const int* NPVAL,
                                   const char *PARNAM,
                                   const int a_dummy,
                                   char *PARTYP,
                                   const int a_dummy1);
DLLEXPORT void MFLIB_READMNW2SP(int *a_SUCCESS,
                                double* MNW2,
                                const int* MNWMAX,
                                const int* NMNWVL,
                                const int* NAUX,
                                const char* LnDesc,
                                int a_dummy1);
DLLEXPORT void MFLIB_SETSFRSEGSIZE(int *a_SZ);

 
}

#endif

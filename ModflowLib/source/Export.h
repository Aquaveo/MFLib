//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef EXPORT_H
#define EXPORT_H

#include <private\util\dblprec.h>

#if MFLIB_DLL
#define DLLEXPORT __declspec( dllexport )
#else
#define DLLEXPORT 
#endif

extern "C"
{
void mfLibExp_NameFile(const char * a_fName);
DLLEXPORT void MFLIBEXP_GEODB(const char *a_arg,
                              int a_dummy,
                              const char * a_fName,
                              int a_dummy1);
DLLEXPORT void MFLIBEXP_GMS(const char *a_arg,
                            int a_dummy,
                            const char * a_fName,
                            int a_dummy1);
DLLEXPORT void MFLIBEXP_TABLES(const char *a_arg,
                               int dummy);
DLLEXPORT void MFLIBEXP_PUTCURRENTPERIOD(const int *a_KPER);
DLLEXPORT void MFLIBEXP_PUTCURRENTGRID(const int *a_IGRID);
DLLEXPORT void MFLIBEXP_CLNLINES0AND1 (const int *a_NCLN,
                                       const int *a_ICLNNDS,
                                       const int *a_ICLNCB,
                                       const int *a_ICLNHD,
                                       const int *a_ICLNDD,
                                       const int *a_ICLNIB,
                                       const int *a_NCLNGWC,
                                       const int *a_NCONDUITYP,
                                       const int *a_ICLNTIB);
DLLEXPORT void MFLIBEXP_CLNLINE7 (const Real *a_ACLNNDSAQ,
                                  const int *a_NCLNNDS);
DLLEXPORT void MFLIBEXP_CLNLINE8 (const Real *a_ACLNGWCAQ);
DLLEXPORT void MFLIBEXP_CLNLINE9 (const Real *a_ACLNGWCAQ);
DLLEXPORT void MFLIBEXP_CLNLINE10 (const Real *a_ACLNCOND);
DLLEXPORT void MFLIBEXP_CLNLINES11AND12 ();
DLLEXPORT void MFLIBEXP_DISU1(const int *a_NODES,
                              const int *a_NJAG,
                              const int *a_IVSD,
                              const int *a_IDSYMRD);
DLLEXPORT void MFLIBEXP_DISU2(const int *a_NODLAY);
DLLEXPORT void MFLIBEXP_DISU3 (const Real* a_PERLEN,
                               const int* a_NSTP,
                               const Real* a_TSMULT,
                               const int* a_ISSFLG);
DLLEXPORT void MFLIBEXP_DISPACKAGE1(const int *a_NLAY,
                                    const int *a_NROW,
                                    const int *a_NCOL,
                                    const int *a_NPER,
                                    const int *a_ITMUNI,
                                    const int *a_LENUNI,
                                    const int *a_LAYCBD,
                                    const int *a_IUNSTR);
DLLEXPORT void MFLIBEXP_DISPACKAGE2(const Real *a_DELR,
                                    const Real *a_DELC,
                                    const int *a_NBTOM,
                                    const Real *a_BOTM,
                                    const Real *a_PERLEN,
                                    const int *a_NSTP,
                                    const Real *a_TSMULT,
                                    const int *a_ISSFLG);
DLLEXPORT void MFLIBEXP_LISTPACKAGE(const char *a_type,
                                    int a_dummy,
                                    const int *a_ITMP,
                                    const int *a_MXBC,
                                    const int *a_NBC,
                                    const int *a_NVL,
                                    const int *a_NAUX,
                                    const Real *a_BCDAT,
                                    const int *a_NP,
                                    const char *a_AUXNMS);
DLLEXPORT void MFLIBEXP_SIPPACKAGE(const char* a_type,
                                   int a_dummy,
                                   const int* a_MXITER,
                                   const int* a_NPARM,
                                   const Real* a_ACCL,
                                   const Real* a_HCLOSE,
                                   const int* a_IPCALC,
                                   const Real* a_WSEED,
                                   const int* a_IPRSIP);
DLLEXPORT void MFLIBEXP_DE4LINE1(const int* a_ITMX,
                                 const int* a_MXUP,
                                 const int* a_MXLOW,
                                 const int* a_MXBW);
DLLEXPORT void MFLIBEXP_DE4LINE2(const int* a_IFREQ,
                                 const int* a_MUTD4,
                                 const Real* a_ACCL,
                                 const Real* a_HCLOSE,
                                 const int* a_IPRD4);
DLLEXPORT void MFLIBEXP_SORPACKAGE(const char* a_type,
                                   const int a_dummy,
                                   const int* a_MXITER,
                                   const Real* a_ACCL,
                                   const Real* a_HCLOSE,
                                   const int* a_IPRSOR);
DLLEXPORT void MFLIBEXP_PCGPACKAGE(const char* a_type,
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
                                   const Real* a_DAMP);
DLLEXPORT void MFLIBEXP_LMGPACKAGE(const char* a_type,
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
                                   const int* a_CONTROL);
DLLEXPORT void MFLIBEXP_GMGPACKAGE(const char* a_type,
                                   int a_dummy,
                                   const Real* a_RCLOSE,
                                   const int* a_IITER,
                                   const Real* a_HCLOSE,
                                   const int* a_MXITER,
                                   const Real* a_DAMP,
                                   const int* a_IADAMP,
                                   const int* a_IOUTGMG,
                                   const int* a_ISM,
                                   const int* a_ISC,
                                   const double* a_RELAX);
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
                                    const double* a_RESLIM);
DLLEXPORT void MFLIBEXP_SMSXMDPACKAGE(const int* a_IACL,
                                      const int* a_NORDER,
                                      const int* a_LEVEL,
                                      const int* a_NORTH,
                                      const int* a_IREDSYS,
                                      const double* a_RRCTOL,
                                      const int* a_IDROPTOL,
                                      const double* a_EPSRN);
DLLEXPORT void MFLIBEXP_SMSPCGUPACKAGE(const int* a_IPC,
                                      const int* a_ISCL,
                                      const int* a_IORD,
                                      const Real* a_RCLOSEPCGU);
DLLEXPORT void MFLIBEXP_U2DREL(const char *a_name,
                               int a_dummy,
                               const Real *a_data,
                               const Real *a_multiplier,
                               const int *a_K,
                               const int *a_IPRN);
DLLEXPORT void MFLIBEXP_U2DREL8(const char *a_name,
                                int a_dummy,
                                const double *a_data,
                                const Real *a_multiplier,
                                const int *a_K,
                                const int *a_IPRN);
DLLEXPORT void MFLIBEXP_U2DINT(const char *a_name,
                               int a_dummy,
                               const int *a_data,
                               const int *a_multiplier,
                               const int *a_K,
                               const int *a_IPRN);
DLLEXPORT void MFLIBEXP_ET(const char *a_PACK,
                           int a_dummy,
                           const int *a_NEVTOP,
                           const int *a_INSURF,
                           const int *a_INEVTR,
                           const int *a_INEXDP,
                           const int *a_INIEVT,
                           const int *a_NETSEG,
                           const int *a_INSGDF);
DLLEXPORT void MFLIBEXP_RCH(const int *a_NRCHOP,
                            const int *a_INRECH,
                            const int *a_INIRCH);
DLLEXPORT void MFLIBEXP_GNC1(const int *a_NPGNCn,
                            const int *a_MXGNn,
                            const int *a_NGNCNPn,
                            const int *a_MXADJn,
                            const int *a_I2Kn,
                            const int *a_ISYMGNCn,
                            const int *a_IFLALPHAn,
                            const int *a_IPRGNCn,
                            const int *a_N1,
                            const int *a_N2,
                            const Real *a_GNCn);
DLLEXPORT void MFLIBEXP_SWI(const int *a_NSRF,
                            const int *a_ISTRAT,
                            const int *a_NOBS,
                            const int *a_ISWIZT,
                            const int *a_ISWIBD,
                            const int *a_ISWIOBS,
                            const int *a_iadptflg,
                            const int *a_NSOLVER,
                            const int *a_IPRSOL,
                            const int *a_MUTSOL,
                            const int *a_MXITER,
                            const int *a_ITER1,
                            const int *a_NPCOND,
                            const Real *a_ZCLOSE,
                            const Real *a_RCLOSE,
                            const Real *a_RELAX,
                            const int *a_NBPOL,
                            const Real *a_DAMP,
                            const Real *a_DAMPT,
                            const Real *a_TOESLOPE,
                            const Real *a_TIPSLOPE,
                            const Real *a_ALPHA,
                            const Real *a_BETA,
                            const int *a_NADPTMX,
                            const int *a_NADPTMN,
                            const Real *a_ADPTFCT,
                            const char* a_obsname,
                            int /*dummy1*/,
                            const int *a_obsk,
                            const int *a_obsi,
                            const int *a_obsj);
DLLEXPORT void MFLIBEXP_HEADINIT(const int* a_nper,
                                 const int* a_ncol,
                                 const int* a_nrow,
                                 const int* a_nlay);
DLLEXPORT void MFLIBEXP_HEAD(const int* a_iper,
                             const int* a_ncol,
                             const int* a_nrow,
                             const int* a_nlay,
                             const Real* a_head);
DLLEXPORT void MFLIBEXP_SINGLEVALINT(const char *a_pckg,
                                     int a_dummy,
                                     const char *a_name,
                                     int a_dummy1,
                                     const int *a_flag);
DLLEXPORT void MFLIBEXP_SINGLEVALFLT(const char *a_pckg,
                                     int a_dummy,
                                     const char *a_name,
                                     int a_dummy1,
                                     const Real *a_flag);
DLLEXPORT void MFLIBEXP_SINGLEVALDBL(const char *a_pckg,
                                     int a_dummy,
                                     const char *a_name,
                                     int a_dummy1,
                                     const double *a_flag);
DLLEXPORT void MFLIBEXP_ARRAYVALINT(const char *a_pckg,
                                    int a_dummy,
                                    const char *a_name,
                                    int a_dummy1,
                                    const int *a_flag);
DLLEXPORT void MFLIBEXP_ARRAYVALFLT(const char *a_pckg,
                                    int a_dummy,
                                    const char *a_name,
                                    int a_dummy1,
                                    const Real *a_flag);
DLLEXPORT void MFLIBEXP_ARRAYVALDBL(const char *a_pckg,
                                    int a_dummy,
                                    const char *a_name,
                                    int a_dummy1,
                                    const double *a_flag);
DLLEXPORT void MFLIBEXP_SINGLEVALSTR(const char *a_pckg,
                                     int a_dummy,
                                     const char *a_name,
                                     int a_dummy1,
                                     const char *a_str,
                                     int a_dummy2,
                                     const int *a_strLen);
DLLEXPORT void MFLIBEXP_EXPPACK(const char *a_pckg,
                                int a_dummy);
DLLEXPORT void MFLIBEXP_LPF1(const int *NLAY,
                             const int *ILPFCB,
                             const Real *HDRY,
                             const int *LAYTYP,
                             const int *LAYAVG,
                             const Real *CHANI,
                             const int *LAYVKA,
                             const int *LAYWET,
                             const int *VERTLEAKFLAG,
                             const int *MF2K5);
DLLEXPORT void MFLIBEXP_FP_OPT(const char *PCK,
                               int a_dummy,
                               const int* ISFAC,
                               const int* ICONCV,
                               const int* ITHFLG,
                               const int* NOCVCO,
                               const int* NOVFC);
DLLEXPORT void MFLIBEXP_BCF1(const int *NLAY,
                             const int *IBCFCB,
                             const Real *HDRY,
                             const int *IWDFLG,
                             const Real *WETFCT,
                             const int *IWETIT,
                             const int *IHDWET,
                             const int *LAYCON,
                             const int *LAYAVG);
DLLEXPORT void MFLIBEXP_HFB(const int *NHFBNP,
                            const Real *HFB);
DLLEXPORT void MFLIBEXP_SEN(const int *ISENALL,
                            const int *IUHEAD,
                            const int *IPRINTS,
                            const int *ISENSU,
                            const int *ISENPU,
                            const int *ISENFM);
DLLEXPORT void MFLIBEXP_PVAL(const char *PARNAM,
                             const int a_dummy,
                             const Real *B,
                             const int *NPVAL);
DLLEXPORT void MFLIBEXP_PES(const int *ITMXP,
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
                            const int *LASTX);
DLLEXPORT void MFLIBEXP_OBSHD(const char *OBSNAME,
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
                              const int *PLOT);
DLLEXPORT void MFLIBEXP_OBSHD4(const int *MLAY,
                               const Real *PR,
                               const int *ML);
DLLEXPORT void MFLIBEXP_OBSHD5(const int *ITT);
DLLEXPORT void MFLIBEXP_OBSHD6(const char *OBSNAME,
                               int a_dummy,
                               const int *IREFSP,
                               const Real *TOFFSET,
                               const Real *HOBS,
                               const Real *STATH,
                               const Real *STATDD,
                               const int *STATFLG,
                               const int *PLOT);
DLLEXPORT void MFLIBEXP_OBSFLO4(const char *OBTYPE,
                                int a_dummy,
                                const char *OBSNAME,
                                int a_dummy1,
                                const int *IREFSP,
                                const Real *TOFFSET,
                                const Real *HOB,
                                const Real *STAT,
                                const int *STATFLG,
                                const int *PLOT);
DLLEXPORT void MFLIBEXP_OBSFLO4_5(const char *OBTYPE,
                                  int a_dummy,
                                  const char *OBSNAME,
                                  int a_dummy1,
                                  const int *IREFSP,
                                  const Real *TOFFSET,
                                  const Real *FLWOBS);
DLLEXPORT void MFLIBEXP_OBSFLO5(const int *NUM,
                                const int *START,
                                const Real *QCLS);
DLLEXPORT void MFLIBEXP_SENPARLIST(const int *NPLIST,
                                   const char *PARNAM,
                                   int ,
                                   const int *ISENS,
                                   const int *LN,
                                   const Real *B,
                                   const Real *BL,
                                   const Real *BU,
                                   const Real *BSCAL);
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
                              const int *NLAKES);
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
                                  const Real *CAUG);
DLLEXPORT void MFLIBEXP_STR(int *ITMP,
                            int *IRDFLG,
                            int *IPTFLG,
                            Real *STRM,
                            int *ISTRM,
                            int *NSTREM,
                            int *MXSTRM,
                            int *ITRBAR,
                            int *IDIVAR);
DLLEXPORT void MFLIBEXP_SFRLINE2(int *ISTRM,
                                 int NISTRMD,
                                 Real *STRM,
                                 int NSTRMD);
DLLEXPORT void MFLIBEXP_SFRLINE6(int *ISEG,
                                 int *IOTSG,
                                 int *IDIVAR,
                                 Real *SEG,
                                 Real *XSEC,
                                 Real *QSTAGE);
DLLEXPORT void MFLIBEXP_MNWSETUP(const int* MXWEL2,
                                 const int* IWL2CB,
                                 const int* IWELPT,
                                 const int* KSPREF,
                                 const double* PLoss, // double precision in MODFLOW
                                 const int* IOWELL2,
                                 const int* NOMOITER,
                                 const char* FTAG,
                                 int dummy1,
                                 const char* PREFIX,
                                 int dummy2,
                                 const char* NAMES,
                                 int dummy3);
DLLEXPORT void MFLIBEXP_MNWSTRESSPERIOD(int *ITMP,
                                        int *NWELL2,
                                        double *WELL2, // double precision in MODFLOW
                                        char *MNWSITE,
                                        int a_dummy,
                                        double *MNWFLGS);
DLLEXPORT void MFLIBEXP_LISTPAR(const char *PNAME,
                                int a_dummy,
                                const char *PTYPE,
                                int a_dummy1,
                                Real *PVAL); // this is NOT const intentionally
DLLEXPORT void MFLIBEXP_LISTPARDATA(const char *PNAME,
                                    int a_dummy,
                                    const char *PTYPE,
                                    int a_dummy1,
                                    const int *START,
                                    const int *LSTDIM,
                                    const int *NBC,
                                    const int *NVALS,
                                    const int *NAUX,
                                    const Real *BCDATA,
                                    const char *AUXNMS);
DLLEXPORT void MFLIBEXP_STREAMPARINFO(const Real *PVAL,
                                      const int *START,
                                      const int *NUMBC);
DLLEXPORT void MFLIBEXP_ARRAYPAR(const char *PNAME,
                                 int a_dummy,
                                 const char *PTYPE,
                                 int a_dummy1,
                                 Real *PVAL, // this is NOT const intentionally
                                 const int *NP,
                                 const int *IPLOC,
                                 const int *IPCLST,
                                 const char *MLTNAM,
                                 int ,
                                 const char *ZONNAM,
                                 int ,
                                 const char *INAME,
                                 int );
DLLEXPORT void MFLIBEXP_ARRAYPARUSED(const char *PNAME,
                                     int a_dummy,
                                     const char *PTYPE,
                                     int a_dummy1,
                                     const char *INAME,
                                     int a_dummy2);
DLLEXPORT void MFLIBEXP_NAMEFILEITEM(const char *FTYPE,
                                     int a_dummy,
                                     const char *FNAME,
                                     int a_dummy1,
                                     const int *NIU);
DLLEXPORT void MFLIBEXP_GAGE(const int *IGGLST, const int *NUMGAGE);
DLLEXPORT void MFLIBEXP_LGR_NAME(const char *FNAME,
                                 int a_dummy);
DLLEXPORT void MFLIBEXP_NAMEFILE_FILENAME(const int* MODTYPE,
                                          const int* IGRID,
                                          const char *FNAME,
                                          int a_dummy);
DLLEXPORT void MFLIBEXP_HUF1(const int *IHUFCB,
                             const Real *HDRY,
                             const int *NHUF,
                             const int *NPHUF,
                             const int *IOHUFHEADS,
                             const int *IOHUFFLOWS,
                             const int *LTHUF,
                             const int *LAYWT);
DLLEXPORT void MFLIBEXP_HUFFLAG(const int *IHGUFLG);
DLLEXPORT void MFLIBEXP_HUFWET(const Real *WETFCT,
                               const int *IWETIT,
                               const int *IHDWET);
DLLEXPORT void MFLIBEXP_HUFHANI(const char *HGUNAM,
                                int ,
                                const Real *HGUHANI,
                                const Real *HGUVANI);
DLLEXPORT void MFLIBEXP_HUFPAR(const char *PNAME,
                               int a_dummy,
                               const char *PTYPE,
                               int a_dummy1,
                               Real *PVAL, // this is NOT const intentionally
                               const int *NP,
                               const int *IPLOC,
                               const int *IPCLST,
                               const char *MLTNAM,
                               int ,
                               const char *ZONNAM,
                               int ,
                               const char *HGUNAM,
                               int );
DLLEXPORT void MFLIBEXP_UZFLINE1(int *NUZTOP,
                                 int *IUZFOPT,
                                 int *IRUNFLG,
                                 int *IETFLG,
                                 int *IUZFCB1,
                                 int *IUZFCB2,
                                 int *NTRAIL2,
                                 int *NSETS2,
                                 int *NUZGAG,
                                 Real *SURFDEP);
DLLEXPORT void MFLIBEXP_UZFLINE8(int *IUZLIST);
DLLEXPORT void MFLIBEXP_UZFSTRESSPERIOD(int *NUZF1,
                                        int *NUZF2,
                                        int *NUZF3,
                                        int *NUZF4);
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
                                 const Real *FIRSTDT);
DLLEXPORT void MFLIBEXP_VDFSTRESSPERIOD(const int *INDENSE);
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
                                 const Real *AMUCOEFF);
DLLEXPORT void MFLIBEXP_VSCSTRESSPERIOD(const int *INVISC);
DLLEXPORT void MFLIBEXP_NWTLN1(const Real* toldum,
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
                               const Real* Breducdum,
                               const int* ICNVGFLG);
DLLEXPORT void MFLIBEXP_NWTLN2(const int* IACL,
                               const int* NORDER,
                               const int* LEVEL,
                               const int* NORTH,
                               const int* IREDSYS,
                               const Real* RRCTOLS,
                               const int* IDROPTOL,
                               const Real* EPSRNS,
                               const Real* HCLOSEXMDDUM,
                               const int* MXITERXMD);
DLLEXPORT void MFLIBEXP_NWTLN2A(const int* Maxitr_gmres,
                                const int* Ilu_method,
                                const int* Lev_fill,
                                const Real* Stop_toldum,
                                const int* Msdr);
DLLEXPORT void MFLIBEXP_UPWA(const int* NLAY,
                             const int* IUPWCB,
                             const Real* HDRY,
                             const int* IPHDRY,
                             const int* LAYTYPUPW,
                             const int* LAYAVG,
                             const Real* CHANI,
                             const int* LAYVKAUPW,
                             const int* LAYWET);
DLLEXPORT void MFLIBEXP_IUNIT(const int* IUNIT,
                              const char* CUNIT,
                              int a_dummy1);
DLLEXPORT void MFLIBEXP_SETSAVECOMMENTS(const int* UNIT,
                                        const int* ISAVE);
DLLEXPORT void MFLIBEXP_COMMENT(const int* UNIT,
                                const char* LINE,
                                int a_dummy1);
DLLEXPORT void MFLIBEXP_MNW2LN1(const int* MNWMAX,
                                const int* IWL2CB,
                                const int* MNWPRNT,
                                const int* NAUX,
                                const char* MNWAUX);
DLLEXPORT void MFLIBEXP_MNW2LN2AB(const char* WELLID,
                                  int a_dummy1,
                                  const int* NNODES,
                                  const char* LOSSTYPE,
                                  int a_dummy2,
                                  const int* PUMPLOC,
                                  const int* Qlimit,
                                  const int* PPFLAG,
                                  const int* PUMPCAP);
DLLEXPORT void MFLIBEXP_MNW2LN2C(const double* Rw,
                                 const double* Rskin,
                                 const double* Kskin,
                                 const double* B,
                                 const double* C,
                                 const double* P,
                                 const double* CWC,
                                 const char* LnDesc,
                                 int a_dummy1);
DLLEXPORT void MFLIBEXP_MNW2LN2D1(const int* IL,
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
                                  int a_dummy1);
DLLEXPORT void MFLIBEXP_MNW2LN2D2(const double* Ztop,
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
                                  int a_dummy1);
DLLEXPORT void MFLIBEXP_MNW2LN2E(const int* PUMPLAY,
                                 const int* PUMPROW,
                                 const int* PUMPCOL,
                                 const double* Zpump,
                                 const char* LnDesc,
                                 int a_dummy1);
DLLEXPORT void MFLIBEXP_MNW2LN2F(const double* Hlim,
                                 const int* QCUT,
                                 const double* Qfrcmn,
                                 const double* Qfrcmx,
                                 const char* LnDesc1,
                                 int a_dummy1);
DLLEXPORT void MFLIBEXP_MNW2LN2G(const double* Hlift,
                                 const double* LIFTq0,
                                 const double* LIFTqdes,
                                 const double* HWtol,
                                 const char* LnDesc1,
                                 int a_dummy1);
DLLEXPORT void MFLIBEXP_MNW2LN2H(const double* Liftn,
                                 const double* Qn,
                                 const char* LnDesc1,
                                 int a_dummy1);
DLLEXPORT void MFLIBEXP_MNW2LN34(const int* ITMP,
                                 const double* MNW2,
                                 const int* NMNWVL,
                                 const int* MNWMAX,
                                 const int* NAUX);
DLLEXPORT void MFLIBEXP_MNWILN1(const int* Wel1flag,
                                const int* QSUMflag,
                                const int* BYNDflag);
DLLEXPORT void MFLIBEXP_MNWILN2(const int* MNWOBS);
DLLEXPORT void MFLIBEXP_MNWILN3(const char* WELLID,
                                int a_dummy1,
                                const int* UNIT,
                                const int* QNDflag,
                                const int* QBHflag,
                                const int* CONCflag);
DLLEXPORT void MFLIBEXP_MNWIEND();


}
bool &mfLibExp_Exporting();
void  mfLibExp_StrCondFact(const Real* a_,
                           const int a_nVal);
void  mfLibExp_SfrCondFact(const Real* a_,
                           int a_nVal,
                           const Real* a_2,
                           int a_nVal2);
#endif

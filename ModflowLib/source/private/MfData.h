//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MFDATA_H
#define MFDATA_H

#include <private\util\dblprec.h>

namespace MfData
{
  bool PutCurrentPeriod(const int *a_KPER);
  bool PutCurrentGrid(const int *a_IGRID);
  bool InitGlobal(int a_modelType,
                  int a_IGRID,
                  const char *a_exp,
                  const char *a_fileName,
                  const char *a_tables);
  bool SetGlobal(const int *a_NLAY,
                 const int *a_NROW,
                 const int *a_NCOL,
                 const int *a_NPER);
  bool SetGlobal(const int *a_NLAY,
                 const int *a_NROW,
                 const int *a_NCOL,
                 const int *a_NPER,
                 const int *a_ITMUNI,
                 const int *a_LENUNI,
                 const int *a_LAYCBD,
                 const int *a_IUNSTR);
  bool exp_ClnLines0And1(const int *a_NCLN,
                         const int *a_ICLNNDS,
                         const int *a_ICLNCB,
                         const int *a_ICLNHD,
                         const int *a_ICLNDD,
                         const int *a_ICLNIB,
                         const int *a_NCLNGWC,
                         const int *a_NCONDUITYP,
                         const int *a_ICLNTIB);
  bool ClnLine7 (const Real *a_ACLNNDSAQ,
                 const int *a_NCLNNDS);
  bool ClnLine8 (const Real *a_ACLNGWCAQ);
  bool ClnLine9 (const Real *a_ACLNGWCAQ);
  bool ClnLine10 (const Real *a_ACLNCOND);
  bool ClnLines11And12 ();
  bool Disu1 (const int *a_NODES,
              const int *a_NJAG,
              const int *a_IVSD,
              const int *a_IDSYMRD);
  bool Disu2 (const int *a_NODLAY);
  bool Disu3 (const Real* a_PERLEN,
              const int* a_NSTP,
              const Real* a_TSMULT,
              const int* a_ISSFLG);
  bool DisPackage2(const Real *a_DELR,
                   const Real *a_DELC,
                   const int *a_NBTOM,
                   const Real *a_BOTM,
                   const Real *a_PERLEN,
                   const int *a_NSTP,
                   const Real *a_TSMULT,
                   const int *a_ISSFLG);
  bool ListPackage(const char * const a_type,
                   const int *a_ITMP,
                   const int *a_MAXBC,
                   const int *a_NUMBC,
                   const int *a_NUMFIELDS,
                   const int *a_NAUX,
                   const Real *a_DATA,
                   const int *a_NP,
                   const char *a_AUX);
  bool SipPackage(const char* const a_type,
                  const int* a_MXITER,
                  const int* a_NPARM,
                  const Real* a_ACCL,
                  const Real* a_HCLOSE,
                  const int* a_IPCALC,
                  const Real* a_WSEED,
                  const int* a_IPRSIP);
  bool De4Line1(const int* a_ITMX,
                const int* a_MXUP,
                const int* a_MXLOW,
                const int* a_MXBW);
  bool De4Line2(const int* a_IFREQ,
                const int* a_MUTD4,
                const Real* a_ACCL,
                const Real* a_HCLOSE,
                const int* a_IPRD4);
  bool SorPackage(const char* const a_type,
                  const int* a_MXITER,
                  const Real* a_ACCL,
                  const Real* a_HCLOSE,
                  const int* a_IPRSOR);
  bool PcgPackage(const char*  const a_type,
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
  bool LmgPackage(const char* const a_type,
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
  bool GmgPackage(const char* const a_type,
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
  bool SmsPackage(const int* a_IFDPARAM,
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
  bool SmsXmdPackage(const int* a_IACL,
                     const int* a_NORDER,
                     const int* a_LEVEL,
                     const int* a_NORTH,
                     const int* a_IREDSYS,
                     const double* a_RRCTOL,
                     const int* a_IDROPTOL,
                     const double* a_EPSRN);
  bool SmsPcguPackage(const int* a_IPC,
                      const int* a_ISCL,
                      const int* a_IORD,
                      const Real* a_RCLOSEPCGU);
  bool U2DREL(const char * const a_name,
              const Real *a_data,
              const Real *a_multiplier,
              const int *a_LAY,
              const int *a_IPRN);
  bool U2DREL8(const char * const a_name,
               const double *a_data,
               const Real *a_multiplier,
               const int *a_LAY,
               const int *a_IPRN);
  bool U2DINT(const char * const a_name,
              const int *a_data,
              const int *a_multiplier,
              const int *a_LAY,
              const int *a_IPRN);
  bool ET(const char *a_PACK,
          const int *a_NEVTOP,
          const int *a_INSURF,
          const int *a_INEVTR,
          const int *a_INEXDP,
          const int *a_INIEVT,
          const int *a_NETSEG,
          const int *a_INSGDF);
  bool RCH(const int *a_NRCHOP,
           const int *a_INRECH,
           const int *a_INIRCH);
  bool GNC1(const int *a_NPGNCn,
           const int *a_MXGNn,
           const int *a_NGNCNPn,
           const int *a_MXADJn,
           const int *a_I2Kn,
           const int *a_ISYMGNCn,
           const int *a_IFLALPHAn,
           const int *a_IPRGNCn,
           const int* a_N1,
           const int* a_N2,
           const Real* a_GNCn);
  bool SWI (const int *a_NSRF,
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
            const int *a_obsk,
            const int *a_obsi,
            const int *a_obsj);
  bool Head(const int* a_iper,
            const int* a_ncol,
            const int* a_nrow,
            const int* a_nlay,
            const Real* a_head);
  bool SingleValInt(const char *a_pckg,
                    const char *a_name,
                    const int *a_flag);
  bool SingleValFlt(const char *a_pckg,
                    const char *a_name,
                    const Real *a_flag);
  bool SingleValDbl(const char *a_pckg,
                    const char *a_name,
                    const double *a_flag);
  bool SingleValStr(const char *a_pckg,
                    const char *a_name,
                    const char *a_flag);
  bool ExportPack(const char *a_pckg);
  bool LPF1(const int *NLAY,
            const int *ILPFCB,
            const Real *HDRY,
            const int *LAYTYP,
            const int *LAYAVG,
            const Real *CHANI,
            const int *LAYVKA,
            const int *LAYWET,
            const int *VERTLEAKFLAG,
            const int *MF2K5);
  bool LPF_OPT(const char *PCK,
               const int* ISFAC,
               const int* ICONCV,
               const int* ITHFLG,
               const int* NOCVCO,
               const int* NOVFC);
  bool BCF1(const int *NLAY,
            const int *IBCFCB,
            const Real *HDRY,
            const int *IWDFLG,
            const Real *WETFCT,
            const int *IWETIT,
            const int *IHDWET,
            const int *LAYCON,
            const int *LAYAVG);
  bool HFB(const int *NHFBNP,
           const Real *HFB);
  bool SEN(const int *ISENALL,
           const int *IUHEAD,
           const int *IPRINTS,
           const int *ISENSU,
           const int *ISENPU,
           const int *ISENFM);
  bool PVAL(const char *PARNAM,
            const Real *B,
            const int *NPVAL);
  bool PES(const int *ITMXP,
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
  bool ObsHd(const char *OBSNAME,
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
  bool ObsHd4(const int *MLAY,
              const Real *PR,
              const int *ML);
  bool ObsHd5(const int *ITT);
  bool ObsHd6(const char *OBSNAME,
              const int *IREFSP,
              const Real *TOFFSET,
              const Real *HOBS,
              const Real *STATH,
              const Real *STATDD,
              const int *STATFLG,
              const int *PLOT);
  bool FloObs4(const char *OBTYPE,
               const char *OBSNAME,
               const int *IREFSP,
               const Real *TOFFSET,
               const Real *HOB,
               const Real *STAT,
               const int *STATFLG,
               const int *PLOT);
  bool FloObs4_5(const char *OBTYPE,
                 const char *OBSNAME,
                 const int *IREFSP,
                 const Real *TOFFSET,
                 const Real *FLWOBS);
  bool FloObs5(const int *NUM,
               const int *START,
               const Real *QCLS);
  bool SENParList(const int *NPLIST,
                  const char *PARNAM,
                  const int *ISENS,
                  const int *LN,
                  const Real *B,
                  const Real *BL,
                  const Real *BU,
                  const Real *BSCAL);
  bool LAKSP(const int *NSOL,
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
  bool STRSP(const int *ITMP,
             const int *IRDFLG,
             const int *IPTFLG,
             const Real *STRM,
             const int *ISTRM,
             const int *NSTREM,
             const int *MXSTRM,
             const int *ITRBAR,
             const int *IDIVAR);
  bool SFRLine2(const int *ISTRM,
                int NISTRMD,
                const Real *STRM,
                int NSTRMD);
  bool SFRLine6(const int *ISEG,
                const int *IOTSG,
                const int *IDIVAR,
                const Real *SEG,
                const Real *XSEC,
                const Real *QSTAGE);
  bool MnwSetup(const int *MXWEL2,
                const int *IWL2CB,
                const int *IWELPT,
                const int *KSPREF,
                const double *PLoss, // double precision in MODFLOW
                const int *IOWELL2,
                const int* NOMOITER,
                const char* FTAG,
                const char* PREFIX,
                const char* NAMES);
  bool MnwStressPeriod(const int *ITMP,
                       const int *NWELL2,
                       const double *WELL2, // double precision in MODFLOW
                       const char *MNWSITE,
                       const double *MNWFLGS);
  bool ListPar(const char *PNAME,
               const char *PTYPE,
               Real *PVAL);
  bool ListParData(const char *PNAME,
                   const char *PTYPE,
                   const int *START,
                   const int *LSTDIM,
                   const int *NBC,
                   const int *NVALS,
                   const int *NAUX,
                   const Real *BCDATA,
                   const char *AUXNMS);
  bool StreamParInfo(const Real *PVAL,
                     const int *START,
                     const int *NUMBC);
  bool ArrayPar(bool a_hufPar,
                const char *PNAME,
                const char *PTYPE,
                Real *PVAL,
                const int *NP,
                const int *IPLOC,
                const int *IPCLST,
                const char *MLTNAM,
                const char *ZONNAM,
                const char *INAME);
  bool ArrayParUsed(const char *PNAME,
                    const char *PTYPE,
                    const char *INAME);
  bool NameFileItem(const char *FTYPE,
                    const char *FNAME,
                    const int *NIU);
  bool Gage(const int* IGGLST, const int* NUMGAGE);
  bool NameFileFilename(const char *FNAME);
  bool HUF1(const int *IHUFCB,
            const Real *HDRY,
            const int *NHUF,
            const int *NPHUF,
            const int *IOHUFHEADS,
            const int *IOHUFFLOWS,
            const int *LTHUF,
            const int *LAYWT);
  bool HufFlag(const int *IHGUFLG);
  bool HufWet(const Real *WETFCT,
              const int *IWETIT,
              const int *IHDWET);
  bool HufHani(const char *HGUNAM,
               const Real *HGUHANI,
               const Real *HGUVANI);
  bool UZFLine1(int *NUZTOP,
                int *IUZFOPT,
                int *IRUNFLG,
                int *IETFLG,
                int *IUZFCB1,
                int *IUZFCB2,
                int *NTRAIL2,
                int *NSETS2,
                int *NUZGAG,
                Real *SURFDEP);
  bool UZFLine8(int *IUZLIST);
  bool UZFStressPeriod(int *NUZF1,
                       int *NUZF2,
                       int *NUZF3,
                       int *NUZF4);
  bool VDFLine5(const int *MT3DRHOFLG,
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
  bool VDFStressPeriod(const int *INDENSE);
  bool VSCLine3(const int *MT3DMUFLG,
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
  bool VSCStressPeriod(const int *INVISC);
  bool NwtLn1(const Real* toldum,
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
  bool NwtLn2(const int* IACL,
              const int* NORDER,
              const int* LEVEL,
              const int* NORTH,
              const int* IREDSYS,
              const Real* RRCTOLS,
              const int* IDROPTOL,
              const Real* EPSRNS,
              const Real* HCLOSEXMDDUM,
              const int* MXITERXMD);
  bool NwtLn2a(const int* Maxitr_gmres,
               const int* Ilu_method,
               const int* Lev_fill,
               const Real* Stop_toldum,
               const int* Msdr);
  bool UPW1(const int* NLAY,
            const int* IUPWCB,
            const Real* HDRY,
            const int* IPHDRY,
            const int* LAYTYPUPW,
            const int* LAYAVG,
            const Real* CHANI,
            const int* LAYVKAUPW,
            const int* LAYWET);
  bool GetSaveComments (int UNIT);
  void SetSaveComments(int UNIT,
                       int ISAVE);
  bool Comment(const char* a_pack,
               const char* a_line);
  bool MNW2_Ln1(const int* MNWMAX,
                const int* IWL2CB,
                const int* MNWPRNT,
                const int* NAUX,
                const char* MNWAUX);
  bool MNW2_Ln2ab(const char* WELLID,
                  const int* NNODES,
                  const char* LOSSTYPE,
                  const int* PUMPLOC,
                  const int* Qlimit,
                  const int* PPFLAG,
                  const int* PUMPCAP);
  bool MNW2_Ln2c(const double* Rw,
                 const double* Rskin,
                 const double* Kskin,
                 const double* B,
                 const double* C,
                 const double* P,
                 const double* CWC,
                 const char* LnDesc);
  bool MNW2_Ln2d(const int* IL,
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
                 const double* Zbotm);
  bool MNW2_Ln2e(const int* PUMPLAY,
                 const int* PUMPROW,
                 const int* PUMPCOL,
                 const double* Zpump,
                 const char* LnDesc);
  bool MNW2_Ln2f(const double* Hlim,
                 const int* QCUT,
                 const double* Qfrcmn,
                 const double* Qfrcmx,
                 const char* LnDesc1);
  bool MNW2_Ln2g(const double* Hlift,
                 const double* LIFTq0,
                 const double* LIFTqdes,
                 const double* HWtol,
                 const char* LnDesc);
  bool MNW2_Ln2h(const double* Liftn,
                 const double* Qn,
                 const char* LnDesc1);
  bool MNW2_Ln34(const int* ITMP,
                 const double* MNW2,
                 const int* NMNWVL,
                 const int* MNWMAX,
                 const int* NAUX);
  bool MNWI_Ln1(const int* Wel1flag,
                const int* QSUMflag,
                const int* BYNDflag);
  bool MNWI_Ln2(const int* MNWOBS);
  bool MNWI_Ln3(const char* WELLID,
                const int* UNIT,
                const int* QNDflag,
                const int* QBHflag,
                const int* CONCflag);
  bool MNWI_End();
}

#endif

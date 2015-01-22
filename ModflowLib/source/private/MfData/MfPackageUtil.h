//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MFPACKAGEUTIL_H
#define MFPACKAGEUTIL_H

#include <private/util/util.h>
#include <private/util/dblprec.h>

namespace MfData
{
  class MfPackage;
  namespace Packages
  {
    void GetComments(CStr a_pack,
                     CStr& a_comment);
    void ClnLine1 (const int *a_NCLN,
                   const int *a_ICLNNDS,
                   const int *a_ICLNCB,
                   const int *a_ICLNHD,
                   const int *a_ICLNDD,
                   const int *a_ICLNIB,
                   const int *a_NCLNGWC,
                   const int *a_NCONDUITYP);
    void ClnLine4 (const Real *a_ACLNNDSAQ,
                   const int *a_NCLNNDS);
    void ClnLine5Or6 (const Real *a_ACLNGWCAQ, int a_size2);
    void ClnLine5 (const Real *a_ACLNGWCAQ);
    void ClnLine6 (const Real *a_ACLNGWCAQ);
    void ClnLine7 (const Real *a_ACLNCOND);
    void ClnLines8And9 ();
    void Disu1 (const int *a_NODES,
                const int *a_NJAG,
                const int *a_IVSD,
                const int *a_IDSYMRD);
    void Disu2 (const int *a_NODLAY);
    void Disu3 (const Real* a_PERLEN,
                const int* a_NSTP,
                const Real* a_TSMULT,
                const int* a_ISSFLG);
    void DisPackage(const Real *a_DELR,
                    const Real *a_DELC,
                    const int *a_NBTOM,
                    const Real *a_BOTM,
                    const Real *a_PERLEN,
                    const int *a_NSTP,
                    const Real *a_TSMULT,
                    const int *a_ISSFLG);
    void ListPackage(const char * const a_type,
                     const int *a_ITMP,
                     const int *a_MAXBC,
                     const int *a_NNPRIV,
                     const int *a_NRIVVL,
                     const int *a_NAUX,
                     const Real *a_RIVR,
                     const int *a_NP,
                     const char *a_RIVAUX);
    void SipPackage(const char* const a_type,
                    const int* a_MXITER,
                    const int* a_NPARM,
                    const Real* a_ACCL,
                    const Real* a_HCLOSE,
                    const int* a_IPCALC,
                    const Real* a_WSEED,
                    const int* a_IPRSIP);
    void De4Line1(const int* a_ITMX,
                  const int* a_MXUP,
                  const int* a_MXLOW,
                  const int* a_MXBW);
    void De4Line2(const int* a_IFREQ,
                  const int* a_MUTD4,
                  const Real* a_ACCL,
                  const Real* a_HCLOSE,
                  const int* a_IPRD4);
    void SorPackage(const char* const a_type,
                    const int* a_MXITER,
                    const Real* a_ACCL,
                    const Real* a_HCLOSE,
                    const int* a_IPRSOR);
    void PcgPackage(const char*  const a_type,
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
    void LmgPackage(const char* const a_type,
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
    void GmgPackage(const char* const a_type,
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
    void SmsPackage(const int* IFDPARAM,
                    const double* HCLOSE,
                    const double* HICLOSE,
                    const int* MXITER,
                    const int* ITER1,
                    const int* IPRSMS,
                    const int* NONMETH,
                    const int* LINMETH,
                    const double* THETA,
                    const double* AKAPPA,
                    const double* GAMMA,
                    const double* AMOMENTUM,
                    const int* NUMTRACK,
                    const double* BTOL,
                    const double* BREDUC,
                    const double* RESLIM);
    void SmsXmdPackage(const int* IACL,
                       const int* NORDER,
                       const int* LEVEL,
                       const int* NORTH,
                       const int* IREDSYS,
                       const double* RRCTOL,
                       const int* IDROPTOL,
                       const double* EPSRN);
    void SmsPcguPackage(const int* IPC,
                        const int* ISCL,
                        const int* IORD,
                        const Real* RCLOSEPCGU);
    void Array2D(const char * const a_name,
                 const Real *a_data,
                 const Real *a_multiplier,
                 const int *a_LAY,
                 const int *a_IPRN);
    void Array2D8(const char * const a_name,
                  const double *a_data,
                  const Real *a_multiplier,
                  const int *a_LAY,
                  const int *a_IPRN);
    void Array2D(const char * const a_name,
                 const int *a_data,
                 const int *a_multiplier,
                 const int *a_LAY,
                 const int *a_IPRN);
    void ETPackage(const char *a_PACK,
                   const int *a_NEVTOP,
                   const int *a_INSURF,
                   const int *a_INEVTR,
                   const int *a_INEXDP,
                   const int *a_INIEVT,
                   const int *a_NETSEG,
                   const int *a_INSGDF);
    void RCHPackage(const int *a_NRCHOP,
                    const int *a_INRECH,
                    const int *a_INIRCH);
    void GNCPackage1(const int *a_NPGNCn,
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
    void Head(const int* a_iper,
              const int* a_ncol,
              const int* a_nrow,
              const int* a_nlay,
              const Real* a_head);
    void SingleValIntToPack(const char *a_pckg,
                            const char *a_name,
                            const int *a_flag);
    void SingleValFltToPack(const char *a_pckg,
                            const char *a_name,
                            const Real *a_flag);
    void SingleValDblToPack(const char *a_pckg,
                            const char *a_name,
                            const double *a_flag);
    void SingleValStrToPack(const char *a_pckg,
                            const char *a_name,
                            const char *a_flag);
    void LPF1Package(const int *NLAY,
                     const int *ILPFCB,
                     const Real *HDRY,
                     const int *LAYTYP,
                     const int *LAYAVG,
                     const Real *CHANI,
                     const int *LAYVKA,
                     const int *LAYWET,
                     const int *VERTLEAKFLAG,
                     const int *MF2K5);
    void LPF_OPT(const char* PCK,
                 const int* ISFAC,
                 const int* ICONCV,
                 const int* ITHFLG,
                 const int* NOCVCO,
                 const int* NOVFC);
    void BCF1Package(const int *NLAY,
                     const int *IBCFCB,
                     const Real *HDRY,
                     const int *IWDFLG,
                     const Real *WETFCT,
                     const int *IWETIT,
                     const int *IHDWET,
                     const int *LAYCON,
                     const int *LAYAVG);
    void HFBPackage(const int *NHFBNP,
                    const Real *HFBf);
    void SENPackage(const int *ISENALL,
                    const int *IUHEAD,
                    const int *IPRINTS,
                    const int *ISENSU,
                    const int *ISENPU,
                    const int *ISENFM);
    void PVALPackage(const char *PARNAM,
                     const Real *B,
                     const int *NPVAL);
    void PESPackage(const int *ITMXP,
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
    void ObsHdPackage(const char *OBSNAME,
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
    void ObsHd4(const int *MLAY,
                const Real *PR,
                const int *ML);
    void ObsHd5(const int *ITT);
    void ObsHd6(const char *OBSNAME,
                const int *IREFSP,
                const Real *TOFFSET,
                const Real *HOBS,
                const Real *STATH,
                const Real *STATDD,
                const int *STATFLG,
                const int *PLOT);
    void FloObs4(const char *OBTYPE,
                 const char *OBSNAME,
                 const int *IREFSP,
                 const Real *TOFFSET,
                 const Real *HOB,
                 const Real *STAT,
                 const int *STATFLG,
                 const int *PLOT);
    void FloObs4_5(const char *OBTYPE,
                   const char *OBSNAME,
                   const int *IREFSP,
                   const Real *TOFFSET,
                   const Real *FLWOBS);
    void FloObs5(const int *NUM,
                 const int *START,
                 const Real *QCLS);
    void SENParList(const int *NPLIST,
                    const char *PARNAM,
                    const int *ISENS,
                    const int *LN,
                    const Real *B,
                    const Real *BL,
                    const Real *BU,
                    const Real *BSCAL);
    void LAKSPPackage(const int *NSOL,
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
    void STRPackage(const int *ITMP,
                    const int *IRDFLG,
                    const int *IPTFLG,
                    const Real *STRM,
                    const int *ISTRM,
                    const int *NSTREM,
                    const int *MXSTRM,
                    const int *ITRBAR,
                    const int *IDIVAR);
    void SFRPackageLine2(const int *ISTRM,
                         int NISTRMD,
                         const Real *STRM,
                         int NSTRMD);
    void SFRPackageLine6(const int *ISEG,
                         const int *IOTSG,
                         const int *IDIVAR,
                         const Real *SEG,
                         const Real *XSEC,
                         const Real *QSTAGE);
    void MnwPackageSetup(const int *MXWEL2,
                         const int *IWL2CB,
                         const int *IWELPT,
                         const int *KSPREF,
                         const double *PLoss, // double precision in MODFLOW
                         const int *IOWELL2,
                         const int* NOMOITER,
                         const char* FTAG,
                         const char* PREFIX,
                         const char* NAMES);
    void MnwPackageStressPeriod(const int *ITMP,
                                const int *NWELL2,
                                const double *WELL2, // double precision in MODFLOW
                                const char *MNWSITE,
                                const double *MNWFLGS);
    void ListPar(const char *PNAME,
                 const char *PTYPE,
                 Real *PVAL);
    void ListParData(const char *PNAME,
                     const char *PTYPE,
                     const int *START,
                     const int *LSTDIM,
                     const int *NBC,
                     const int *NVALS,
                     const int *NAUX,
                     const Real *BCDATA,
                     const char *AUXNMS);
    void StreamParInfo(const Real *PVAL,
                       const int *START,
                       const int *NUMBC);
    void ArrayPar(bool a_hufPar,
                  const char *PNAME,
                  const char *PTYPE,
                  Real *PVAL,
                  const int *NP,
                  const int *IPLOC,
                  const int *IPCLST,
                  const char *MLTNAM,
                  const char *ZONNAM,
                  const char *INAME);
    void ArrayParUsed(const char *PNAME,
                      const char *PTYPE,
                      const char *INAME);
    void PilotPointData(int a_scatIdx,
                        int a_nPts,
                        int a_nWts,
                        const Real *a_weights,
                        const int *a_indices);
    void NameFileItem(const char *FTYPE,
                      const char *FNAME,
                      const int *NIU);
    void Gage(const int* IGGLST, const int *NUMGAGE);
    void NameFileFilename(const char *FNAME);
    bool GetParamKeyAndDataStart(MfData::MfPackage* a_package,
                                 Real &a_key,
                                 int &a_start);
    bool GetBcData(MfData::MfPackage* a_package,
                   const char *a_packName,
                   const int **a_nBcs,
                   int *a_nFields,
                   const int **a_nAux,
                   const Real **a_data,
                   const int **a_dataFields,
                   std::vector<CStr> &a_names);
    bool GetBcFieldNames(const char * const a_type,
                         std::vector<CStr> &bcFieldNames);
    bool GetPackNameFromParameter(MfData::MfPackage* a_package,
                                  CStr &a_name);
    bool GetParamSrcDestFields(const CStr &a_packName,
                               const std::vector<CStr> &a_fields,
                               std::map<int, CStr> &a_srcIdx_destField);
    bool GetParamSrcDestFields(const CStr &a_packName,
                               const std::vector<CStr> &a_fields,
                               std::map<int, int> &a_srcIdx_destField);
    void SaveHfbParameterData(MfPackage *a_p,
                              std::map<CStr, std::vector<Real> >& a_hfbPar);
    bool HUF1 (const int *IHUFCB,
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
    bool UZFPackageLine1 (const int *NUZTOP,
                          const int *IUZFOPT,
                          const int *IRUNFLG,
                          const int *IETFLG,
                          const int *IUZFCB1,
                          const int *IUZFCB2,
                          const int *NTRAIL2,
                          const int *NSETS2,
                          const int *NUZGAG,
                          const Real *SURFDEP);
    bool UZFPackageSP (const int *NUZF1,
                       const int *NUZF2,
                       const int *NUZF3,
                       const int *NUZF4);
    bool UZFPackageLine8 (const int *IUZLIST);
    bool VDFPackageLine5(const int *MT3DRHOFLG,
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
    bool VDFPackageStressPeriod(const int *INDENSE);
    bool VSCPackageLine3(const int *MT3DMUFLG,
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
    bool VSCPackageStressPeriod(const int *INVISC);
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
                const Real* Breducdum);
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
    bool Comment(const CStr& a_pack,
                 const CStr& a_line);
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
}

#endif

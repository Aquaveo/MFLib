!-------------------------------------------------------------------------------
! BRIEF: Contains code for exporting to a geo database
!-------------------------------------------------------------------------------

!///////////////////////////////////////////////////////////////////////////////
!
! MODULE: module_exportData
!
!///////////////////////////////////////////////////////////////////////////////

module module_exportData
  use module_aquaveo_data
  implicit none
  
      INTERFACE
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_GeoDb(ARG,FNAME)
          CHARACTER  ARG (*)
          CHARACTER  FNAME (*)
        END SUBROUTINE mfLibExp_GeoDb
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_GMS(ARG,FNAME)
          CHARACTER  ARG (*)
          CHARACTER  FNAME (*)
        END SUBROUTINE mfLibExp_GMS
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_Tables(ARG)
          CHARACTER  ARG (*)
        END SUBROUTINE mfLibExp_Tables
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_putCurrentPeriod(KPER)
          INTEGER KPER [REFERENCE]
        END SUBROUTINE mfLibExp_putCurrentPeriod
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_putCurrentGrid(IGRID)
          INTEGER IGRID [REFERENCE]
        END SUBROUTINE mfLibExp_putCurrentGrid
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_DisPackage1(NLAY,NROW,NCOL,NPER,ITMUNI,LENUNI,LAYCBD,IUNSTR)
          INTEGER NLAY [REFERENCE]
          INTEGER NROW [REFERENCE]
          INTEGER NCOL [REFERENCE]
          INTEGER NPER [REFERENCE]
          INTEGER ITMUNI [REFERENCE]
          INTEGER LENUNI [REFERENCE]
          INTEGER LAYCBD (*)
          INTEGER IUNSTR [REFERENCE]
        END SUBROUTINE mfLibExp_DisPackage1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_DisPackage2(DELR,DELC,NBOTM,BOTM,&
                                        PERLEN,NSTP,TSMULT,ISSFLG)
          DIMENSION DELR (*)
          DIMENSION DELC (*)
          INTEGER   NBOTM [REFERENCE]
          DIMENSION BOTM (*)
          DIMENSION PERLEN (*)
          DIMENSION NSTP (*)
          DIMENSION TSMULT (*)
          DIMENSION ISSFLG (*)
        END SUBROUTINE mfLibExp_DisPackage2
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ListPackage(BTYPE,ITMP,MXBC,NBC,NVALS,NAUX,BCDATA,NP,AUXNMS)
          CHARACTER*3  BTYPE [REFERENCE]
          INTEGER      ITMP [REFERENCE]
          INTEGER      MXBC [REFERENCE]
          INTEGER      NBC [REFERENCE]
          INTEGER      NVALS [REFERENCE]
          INTEGER      NAUX [REFERENCE]
          DIMENSION    BCDATA (*)
          INTEGER      NP [REFERENCE]
          CHARACTER    AUXNMS (*)
        END SUBROUTINE mfLibExp_ListPackage
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SipPackage(BCTYPE,MXITER,NPARM,ACCL,HCLOSE,IPCALC,WSEED,IPRSIP)
          CHARACTER*3  BCTYPE [REFERENCE]
          INTEGER      MXITER [REFERENCE]
          INTEGER      NPARM [REFERENCE]
          REAL         ACCL [REFERENCE]
          REAL         HCLOSE [REFERENCE]
          INTEGER      IPCALC [REFERENCE]
          REAL         WSEED [REFERENCE]
          INTEGER      IPRSIP [REFERENCE]
        END SUBROUTINE mfLibExp_SipPackage
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_De4Line1(ITMX,MXUP,MXLOW,MXBW)
          INTEGER      ITMX [REFERENCE]
          INTEGER      MXUP [REFERENCE]
          INTEGER      MXLOW [REFERENCE]
          INTEGER      MXBW [REFERENCE]
        END SUBROUTINE mfLibExp_De4Line1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_De4Line2(IFREQ,MUTD4,ACCL,HCLOSE,IPRD4)
          INTEGER      IFREQ [REFERENCE]
          INTEGER      MUTD4 [REFERENCE]
          REAL         ACCL [REFERENCE]
          REAL         HCLOSE [REFERENCE]
          INTEGER      IPRD4 [REFERENCE]
        END SUBROUTINE mfLibExp_De4Line2
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SorPackage(BCTYPE,MXITER,ACCL,HCLOSE,IPRSOR)
          CHARACTER*3  BCTYPE [REFERENCE]
          INTEGER      MXITER [REFERENCE]
          REAL         ACCL [REFERENCE]
          REAL         HCLOSE [REFERENCE]
          INTEGER      IPRSOR [REFERENCE]
        END SUBROUTINE mfLibExp_SorPackage
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_PcgPackage(BCTYPE,MXITER,ITER1,NPCOND,HCLOSE,RCLOSE,RELAX,NBPOL,IPRPCG,MUTPCG,DAMP)
          CHARACTER*3  BCTYPE [REFERENCE]
          INTEGER      MXITER [REFERENCE]
          INTEGER      ITER1 [REFERENCE]
          INTEGER      NPCOND [REFERENCE]
          REAL         HCLOSE [REFERENCE]
          REAL         RCLOSE [REFERENCE]
          REAL         RELAX [REFERENCE]
          INTEGER      NBPOL [REFERENCE]
          INTEGER      IPRPCG [REFERENCE]
          INTEGER      MUTPCG [REFERENCE]
          REAL         DAMP [REFERENCE]
        END SUBROUTINE mfLibExp_PcgPackage
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_LmgPackage(BCTYPE,STOR1,STOR2,STOR3,ICG,MXITER,MXCYC,BCLOSE,DAMP,IOUTAMG,DUP,DLOW,HCLOSE,CONTROL)
          CHARACTER*3  BCTYPE [REFERENCE]
          REAL         STOR1 [REFERENCE]
          REAL         STOR2 [REFERENCE]
          REAL         STOR3 [REFERENCE]
          INTEGER      ICG [REFERENCE]
          INTEGER      MXITER [REFERENCE]
          INTEGER      MXCYC [REFERENCE]
          REAL         BCLOSE [REFERENCE]
          REAL         DAMP [REFERENCE]
          INTEGER      IOUTAMG [REFERENCE]
          REAL         DUP [REFERENCE]
          REAL         DLOW [REFERENCE]
          REAL         HCLOSE [REFERENCE]
          INTEGER      CONTROL [REFERENCE]
        END SUBROUTINE mfLibExp_LmgPackage
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_GmgPackage(BCTYPE,RCLOSE,IITER,HCLOSE,MXITER,DAMP,IADAMP,IOUTGMG,ISM,ISC,RELAX)
          CHARACTER*3  BCTYPE [REFERENCE]
          REAL         RCLOSE [REFERENCE]
          INTEGER      IITER [REFERENCE]
          REAL         HCLOSE [REFERENCE]
          INTEGER      MXITER [REFERENCE]
          REAL         DAMP [REFERENCE]
          INTEGER      IADAMP [REFERENCE]
          INTEGER      IOUTGMG [REFERENCE]
          INTEGER      ISM [REFERENCE]
          INTEGER      ISC [REFERENCE]
          DOUBLEPRECISION RELAX [REFERENCE]
        END SUBROUTINE mfLibExp_GmgPackage
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SmsPackage(IFDPARAM,HCLOSE,HICLOSE,MXITER,ITER1,IPRSMS,NONMETH,LINMETH,THETA,AKAPPA,GAMMA,AMOMENTUM,NUMTRACK,BTOL,BREDUC,RESLIM)
          INTEGER      IFDPARAM [REFERENCE]
          DOUBLEPRECISION HCLOSE [REFERENCE]
          DOUBLEPRECISION HICLOSE [REFERENCE]
          INTEGER      MXITER [REFERENCE]
          INTEGER      ITER1 [REFERENCE]
          INTEGER      IPRSMS [REFERENCE]
          INTEGER      NONMETH [REFERENCE]
          INTEGER      LINMETH [REFERENCE]
          DOUBLEPRECISION THETA [REFERENCE]
          DOUBLEPRECISION AKAPPA [REFERENCE]
          DOUBLEPRECISION GAMMA [REFERENCE]
          DOUBLEPRECISION AMOMENTUM [REFERENCE]
          INTEGER      NUMTRACK [REFERENCE]
          DOUBLEPRECISION BTOL [REFERENCE]
          DOUBLEPRECISION BREDUC [REFERENCE]
          DOUBLEPRECISION RESLIM [REFERENCE]
        END SUBROUTINE mfLibExp_SmsPackage
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SmsXmdPackage(IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOL,IDROPTOL,EPSRN)
          INTEGER      IACL [REFERENCE]
          INTEGER      NORDER [REFERENCE]
          INTEGER      LEVEL [REFERENCE]
          INTEGER      NORTH [REFERENCE]
          INTEGER      IREDSYS [REFERENCE]
          DOUBLEPRECISION RRCTOL [REFERENCE]
          INTEGER      IDROPTOL [REFERENCE]
          DOUBLEPRECISION EPSRN [REFERENCE]
        END SUBROUTINE mfLibExp_SmsXmdPackage
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SmsPcguPackage(IPC,ISCL,IORD,RCLOSEPCGU)
          INTEGER      IPC [REFERENCE]
          INTEGER      ISCL [REFERENCE]
          INTEGER      IORD [REFERENCE]
          REAL         RCLOSEPCGU [REFERENCE]
        END SUBROUTINE mfLibExp_SmsPcguPackage
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIB_AREALOPT_EXDATA(OPT,NAME)
          INTEGER       OPT [REFERENCE]
          CHARACTER*10  NAME [REFERENCE]
        END SUBROUTINE MFLIB_AREALOPT_EXDATA
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIB_U2DREL_EXDATA(NAME,ARR,MULT,K,JJ,II)
          CHARACTER*24  NAME [REFERENCE]
          DIMENSION     ARR (*)
          REAL          MULT [REFERENCE]
          INTEGER       K [REFERENCE]
          INTEGER       JJ [REFERENCE]
          INTEGER       II [REFERENCE]
        END SUBROUTINE MFLIB_U2DREL_EXDATA
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIB_U2DREL8_EXDATA(NAME,ARR,MULT,K,JJ,II)
          CHARACTER*24  NAME [REFERENCE]
          REAL*8        ARR (*)
          REAL          MULT [REFERENCE]
          INTEGER       K [REFERENCE]
          INTEGER       JJ [REFERENCE]
          INTEGER       II [REFERENCE]
        END SUBROUTINE MFLIB_U2DREL8_EXDATA
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIB_U2DINT_EXDATA(NAME,ARR,MULT,K,JJ,II)
          CHARACTER*24  NAME [REFERENCE]
          INTEGER       ARR (*)
          INTEGER       MULT [REFERENCE]
          INTEGER       K [REFERENCE]
          INTEGER       JJ [REFERENCE]
          INTEGER       II [REFERENCE]
        END SUBROUTINE MFLIB_U2DINT_EXDATA
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_U2DREL(NAME,ARR,MULT,K,IPRN)
          CHARACTER*24  NAME [REFERENCE]
          DIMENSION     ARR (*)
          REAL          MULT [REFERENCE]
          INTEGER       K [REFERENCE]
          INTEGER       IPRN [REFERENCE]
        END SUBROUTINE mfLibExp_U2DREL
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_U2DREL8(NAME,ARR,MULT,K,IPRN)
          CHARACTER*24  NAME [REFERENCE]
          REAL*8        ARR (*)
          REAL          MULT [REFERENCE]
          INTEGER       K [REFERENCE]
          INTEGER       IPRN [REFERENCE]
        END SUBROUTINE mfLibExp_U2DREL8
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_U2DINT(NAME,ARR,MULT,K,IPRN)
          CHARACTER*24  NAME [REFERENCE]
          INTEGER       ARR (*)
          INTEGER       MULT [REFERENCE]
          INTEGER       K [REFERENCE]
          INTEGER       IPRN [REFERENCE]
        END SUBROUTINE mfLibExp_U2DINT
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ET(PACK,NEVTOP,INSURF,INEVTR,INEXDP,INIEVT,NETSEG,INSGDF)
          CHARACTER*3   PACK [REFERENCE]
          INTEGER       NEVTOP [REFERENCE]
          INTEGER       INSURF [REFERENCE]
          INTEGER       INEVTR [REFERENCE]
          INTEGER       INEXDP [REFERENCE]
          INTEGER       INIEVT [REFERENCE]
          INTEGER       NETSEG [REFERENCE]
          INTEGER       INSGDF [REFERENCE]
        END SUBROUTINE mfLibExp_ET
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_RCH(NRCHOP,INRECH,INIRCH)
          INTEGER       NRCHOP [REFERENCE]
          INTEGER       INRECH [REFERENCE]
          INTEGER       INIRCH [REFERENCE]
        END SUBROUTINE mfLibExp_RCH
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_Gnc1(NPGNCn,MXGNn,NGNCNPn,MXADJn,I2Kn,ISYMGNCn,IFLALPHAn,IPRGNCn,N1,N2,GNCn)
          INTEGER      NPGNCn [REFERENCE]
          INTEGER      MXGNn [REFERENCE]
          INTEGER      NGNCNPn [REFERENCE]
          INTEGER      MXADJn [REFERENCE]
          INTEGER      I2Kn [REFERENCE]
          INTEGER      ISYMGNCn [REFERENCE]
          INTEGER      IFLALPHAn [REFERENCE]
          INTEGER      IPRGNCn [REFERENCE]
          INTEGER      N1 [REFERENCE]
          INTEGER      N2 [REFERENCE]
          DIMENSION    GNCn (*)
        END SUBROUTINE mfLibExp_Gnc1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SingleValInt(PACKNAME,NAME,VAL)
          CHARACTER     PACKNAME (*)
          CHARACTER     NAME (*)
          INTEGER       VAL [REFERENCE]
        END SUBROUTINE mfLibExp_SingleValInt
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SingleValFlt(PACKNAME,NAME,VAL)
          CHARACTER     PACKNAME (*)
          CHARACTER     NAME (*)
          REAL          VAL [REFERENCE]
        END SUBROUTINE mfLibExp_SingleValFlt
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SingleValDbl(PACKNAME,NAME,VAL)
          CHARACTER         PACKNAME (*)
          CHARACTER         NAME (*)
          DOUBLE PRECISION  VAL [REFERENCE]
        END SUBROUTINE mfLibExp_SingleValDbl
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ArrayValFlt(PACKNAME,NAME,VAL)
          CHARACTER     PACKNAME (*)
          CHARACTER     NAME (*)
          DIMENSION     VAL (*)
        END SUBROUTINE mfLibExp_ArrayValFlt
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ArrayValInt(PACKNAME,NAME,IVAL)
          CHARACTER     PACKNAME (*)
          CHARACTER     NAME (*)
          DIMENSION     IVAL (*)
        END SUBROUTINE mfLibExp_ArrayValInt
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SingleValStr(PACKNAME,NAME,VAL,LEN)
          CHARACTER     PACKNAME (*)
          CHARACTER     NAME (*)
          CHARACTER     VAL (*)
          INTEGER       LEN [REFERENCE]
        END SUBROUTINE mfLibExp_SingleValStr
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ExpPack(PACKNAME)
          CHARACTER     PACKNAME (*)
        END SUBROUTINE mfLibExp_ExpPack
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_LPF1(NLAY,ILPFCB,HDRY,LAYTYP,LAYAVG,CHANI,LAYVKA,LAYWET,VERTLEAKFLAG, MF2K5)
          INTEGER       MF2K5
          INTEGER       NLAY [REFERENCE]
          INTEGER       ILPFCB [REFERENCE]
          REAL          HDRY [REFERENCE]
          DIMENSION     LAYTYP (*)
          DIMENSION     LAYAVG (*)
          DIMENSION     CHANI (*)
          DIMENSION     LAYVKA (*)
          DIMENSION     LAYWET (*)
          INTEGER       VERTLEAKFLAG [REFERENCE]
        END SUBROUTINE mfLibExp_LPF1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_FP_OPT(PCK,ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC)
          CHARACTER     PCK(3)
          INTEGER       ISFAC [REFERENCE]
          INTEGER       ICONCV [REFERENCE]
          INTEGER       ITHFLG [REFERENCE]
          INTEGER       NOCVCO [REFERENCE]
          INTEGER       NOVFC [REFERENCE]
        END SUBROUTINE mfLibExp_FP_OPT
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_BCF1(NLAY,IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET,LAYCON,LAYAVG)
          INTEGER       NLAY [REFERENCE]
          INTEGER       IBCFCB [REFERENCE]
          REAL          HDRY [REFERENCE]
          INTEGER       IWDFLG [REFERENCE]
          REAL          WETFCT [REFERENCE]
          INTEGER       IWETIT [REFERENCE]
          INTEGER       IHDWET [REFERENCE]
          DIMENSION     LAYCON (*)
          DIMENSION     LAYAVG (*)
        END SUBROUTINE mfLibExp_BCF1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_HFB(NHFBNP,HFB)
          INTEGER       NHFBNP [REFERENCE]
          REAL          HFB (*)
        END SUBROUTINE mfLibExp_HFB
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SEN(ISENALL,IUHEAD,IPRINTS,INENSU,ISENPU,ISENFM)
          INTEGER       ISENALL [REFERENCE]
          INTEGER       IUHEAD [REFERENCE]
          INTEGER       IPRINTS [REFERENCE]
          INTEGER       INENSU [REFERENCE]
          INTEGER       ISENPU [REFERENCE]
          INTEGER       ISENFM [REFERENCE]
        END SUBROUTINE mfLibExp_SEN
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_PVAL(PARNAM,B,NPVAL)
          CHARACTER     PARNAM (*)
          REAL          B      (*)
          INTEGER       NPVAL  [REFERENCE]
        END SUBROUTINE mfLibExp_PVAL
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_PES(ITMXP,DMAX,RTOL,SOSC,IBEFLG,IYCFLG,IOSTAR,NOPT,&
                                NFIT,SOSR,RMAR,RMARM,IAP,IPRC,IPRINT,LPRINT,&
                                CSA,FCONV,LASTX)
          INTEGER       ITMXP [REFERENCE]
          REAL          DMAX [REFERENCE]
          REAL          RTOL [REFERENCE]
          REAL          SOSC [REFERENCE]
          INTEGER       IBEFLG [REFERENCE]
          INTEGER       IYCFLG [REFERENCE]
          INTEGER       IOSTAR [REFERENCE]
          INTEGER       NOPT [REFERENCE]
          INTEGER       NFIT [REFERENCE]
          REAL          SOSR [REFERENCE]
          REAL          RMAR [REFERENCE]
          REAL          RMARM [REFERENCE]
          INTEGER       IAP [REFERENCE]
          INTEGER       IPRC [REFERENCE]
          INTEGER       IPRINT [REFERENCE]
          INTEGER       LPRINT [REFERENCE]
          REAL          CSA [REFERENCE]
          REAL          FCONV [REFERENCE]
          INTEGER       LASTX [REFERENCE]
        END SUBROUTINE mfLibExp_PES
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ObsHd(OBSNAME,LAYER,ROW,COL,IREFSP,TOFFSET,ROFF,&
                                  COFF,HOBS,STAT,STATFLG,PLOT)
          CHARACTER     OBSNAME (*)
          INTEGER       LAYER [REFERENCE]
          INTEGER       ROW [REFERENCE]
          INTEGER       COL [REFERENCE]
          INTEGER       IREFSP [REFERENCE]
          REAL          TOFFSET [REFERENCE]
          REAL          ROFF [REFERENCE]
          REAL          COFF [REFERENCE]
          REAL          HOBS [REFERENCE]
          REAL          STAT [REFERENCE]
          INTEGER       STATFLG [REFERENCE]
          INTEGER       PLOT [REFERENCE]
        END SUBROUTINE mfLibExp_ObsHd
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ObsHd4(MLAY,PR,ML)
          DIMENSION     MLAY (*)
          DIMENSION     PR (*)
          INTEGER       ML [REFERENCE]
        END SUBROUTINE mfLibExp_ObsHd4
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ObsHd5(ITT)
          INTEGER       ITT [REFERENCE]
        END SUBROUTINE mfLibExp_ObsHd5
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ObsHd6(OBSNAME,IREFSP,TOFFSET,HOBS,STATH,STATDD,&
                                   STATFLG,PLOT)
          CHARACTER     OBSNAME (*)
          INTEGER       IREFSP [REFERENCE]
          REAL          TOFFSET [REFERENCE]
          REAL          HOBS [REFERENCE]
          REAL          STATH [REFERENCE]
          REAL          STATDD [REFERENCE]
          INTEGER       STATFLG [REFERENCE]
          INTEGER       PLOT [REFERENCE]
        END SUBROUTINE mfLibExp_ObsHd6
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ObsFlo4(OBTYPE,NAME,IREFSP,TOFFSET,HOBS,STAT,&
                                    STATFLG,PLOT)
          CHARACTER*4   OBTYPE [REFERENCE]
          CHARACTER     NAME (*)
          INTEGER       IREFSP [REFERENCE]
          REAL          TOFFSET [REFERENCE]
          REAL          HOBS [REFERENCE]
          REAL          STAT [REFERENCE]
          INTEGER       STATFLG [REFERENCE]
          INTEGER       PLOT [REFERENCE]
        END SUBROUTINE mfLibExp_ObsFlo4
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ObsFlo4_5(OBTYPE,NAME,IREFSP,TOFFSET,FLWOBS)
          CHARACTER*4   OBTYPE [REFERENCE]
          CHARACTER     NAME (*)
          INTEGER       IREFSP [REFERENCE]
          REAL          TOFFSET [REFERENCE]
          REAL          FLWOBS [REFERENCE]
        END SUBROUTINE mfLibExp_ObsFlo4_5
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ObsFlo5(NUM,START,QCLS)
          INTEGER       NUM [REFERENCE]
          INTEGER       START [REFERENCE]
          DIMENSION     QCLS (*)
        END SUBROUTINE mfLibExp_ObsFlo5
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SENParList(NPLIST,PARNAM,ISENS,LN,B,BL,BU,BSCAL)
          INTEGER       NPLIST [REFERENCE]
          CHARACTER     PARNAM (*)
          INTEGER       ISENS (*)
          INTEGER       LN (*)
          DIMENSION     B (*)
          DIMENSION     BL (*)
          DIMENSION     BU (*)
          DIMENSION     BSCAL (*)
        END SUBROUTINE mfLibExp_SENParList
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_LAKSP(NSOL,STAGES,SSMN,SSMX,CLAKE,ITMP,ITMP1,&
                                  LWRT,LKARR,BDLKNC,NSLMS,IC,ISUB,SILLVT,&
                                  PRCPLK,EVAPLK,RNF,WTHDRW,CPPT,CRNF,CAUG,&
                                  NLAKES)
          INTEGER       NSOL [REFERENCE]
          DIMENSION     STAGES (*)
          DIMENSION     SSMN (*)
          DIMENSION     SSMX (*)
          DIMENSION     CLAKE (*)
          INTEGER       ITMP [REFERENCE]
          INTEGER       ITMP1 [REFERENCE]
          INTEGER       LWRT [REFERENCE]
          INTEGER       LKARR (*)
          DIMENSION     BDLKNC (*)
          INTEGER       NSLMS
          INTEGER       IC (*)
          INTEGER       ISUB (*)
          DIMENSION     SILLVT (*)
          DIMENSION     PRCPLK (*)
          DIMENSION     EVAPLK (*)
          DIMENSION     RNF (*)
          DIMENSION     WTHDRW (*)
          DIMENSION     CPPT (*)
          DIMENSION     CRNF (*)
          DIMENSION     CAUG (*)
          INTEGER       NLAKES
        END SUBROUTINE mfLibExp_LAKSP
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_LAKSP_dbl(NSOL,STAGES,SSMN,SSMX,CLAKE,ITMP,ITMP1,&
                                      LWRT,LKARR,BDLKNC,NSLMS,IC,ISUB,SILLVT,&
                                      PRCPLK,EVAPLK,RNF,WTHDRW,CPPT,CRNF,CAUG)
          INTEGER       NSOL [REFERENCE]
          DIMENSION     STAGES (*)
          DIMENSION     SSMN (*)
          DIMENSION     SSMX (*)
          DIMENSION     CLAKE (*)
          INTEGER       ITMP [REFERENCE]
          INTEGER       ITMP1 [REFERENCE]
          INTEGER       LWRT [REFERENCE]
          INTEGER       LKARR (*)
          DIMENSION     BDLKNC (*)
          INTEGER       NSLMS
          INTEGER       IC (*)
          INTEGER       ISUB (*)
          DIMENSION     SILLVT (*)
          DOUBLE PRECISION PRCPLK (*)
          DOUBLE PRECISION EVAPLK (*)
          DIMENSION     RNF (*)
          DIMENSION     WTHDRW (*)
          DIMENSION     CPPT (*)
          DIMENSION     CRNF (*)
          DIMENSION     CAUG (*)
        END SUBROUTINE mfLibExp_LAKSP_dbl
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_STR(ITMP,IRDFLG,IPTFLG,STRM,ISTRM,NSTREM,&
                                MXSTRM,ITRBAR,IDIVAR)
          INTEGER       ITMP [REFERENCE]
          INTEGER       IRDFLG [REFERENCE]
          INTEGER       IPTFLG [REFERENCE]
          DIMENSION     STRM (*)
          INTEGER       ISTRM (*)
          INTEGER       NSTREM [REFERENCE]
          INTEGER       MXSTRM [REFERENCE]
          INTEGER       ITRBAR (*)
          INTEGER       IDIVAR (*)
        END SUBROUTINE mfLibExp_STR
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SFRLine2(ISTRM,NISTRMD,STRM,NSTRMD)
          INTEGER       ISTRM (*)
          INTEGER       NISTRMD [VALUE]
          DIMENSION     STRM (*)
          INTEGER       NSTRMD [VALUE]
        END SUBROUTINE mfLibExp_SFRLine2
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_SFRLine6(ISEG,IOTSG,IDIVAR,SEG,XSEC,&
                                     QSTAGE)
          INTEGER       ISEG (*)
          INTEGER       IOTSG (*)
          INTEGER       IDIVAR (*)
          DIMENSION     SEG (*)
          DIMENSION     XSEC (*)
          DIMENSION     QSTAGE (*)
        END SUBROUTINE mfLibExp_SFRLine6
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_MnwSetup(MXWEL2,IWL2CB,IWELPT,KSPREF,PLOSS,IOWELL2,&
                                     NOMOITER,FTAG,PREFIX,NAMES)
          INTEGER          MXWEL2 [REFERENCE]
          INTEGER          IWL2CB [REFERENCE]
          INTEGER          IWELPT [REFERENCE]
          INTEGER          KSPREF [REFERENCE]
          DOUBLE PRECISION PLOSS [REFERENCE]
          INTEGER          IOWELL2 (*)
          INTEGER          NOMOITER [REFERENCE]
          CHARACTER*6      FTAG (*)
          CHARACTER        PREFIX (*)
          CHARACTER        NAMES (*)
        END SUBROUTINE mfLibExp_MnwSetup
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_MnwStressPeriod(ITMP,NWELL2,WELL2,MNWSITE,MNWFLGS)
          INTEGER          ITMP [REFERENCE]
          INTEGER          NWELL2 [REFERENCE]
          DOUBLE PRECISION WELL2 (*)
          CHARACTER*32     MNWSITE (*)
          DOUBLE PRECISION MNWFLGS (*)
        END SUBROUTINE mfLibExp_MnwStressPeriod
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ListPar(PNAME,PTYPE,PVAL)
          CHARACTER*10  PNAME
          CHARACTER*4   PTYPE
          REAL          PVAL [REFERENCE]
        END SUBROUTINE mfLibExp_ListPar
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ListParData(PNAME,PTYPE,START,LSTDIM,NBC,NVALS,NAUX,BCDATA,AUXNMS)
          CHARACTER*10  PNAME
          CHARACTER*3   PTYPE
          INTEGER       START [REFERENCE]
          INTEGER       LSTDIM [REFERENCE]
          INTEGER       NBC [REFERENCE]
          INTEGER       NVALS [REFERENCE]
          INTEGER       NAUX [REFERENCE]
          DIMENSION     BCDATA (*)
          CHARACTER     AUXNMS (*)
        END SUBROUTINE mfLibExp_ListParData
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_StreamParInfo(PVAL,START,NUMBC)
          REAL          PVAL [REFERENCE]
          INTEGER       START [REFERENCE]
          INTEGER       NUMBC [REFERENCE]
        END SUBROUTINE mfLibExp_StreamParInfo
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ArrayPar(PNAME,PTYPE,PVAL,NP,IPLOC,IPCLST,MLT,ZON,INAME)
          CHARACTER*10  PNAME
          CHARACTER*3   PTYPE
          REAL          PVAL [REFERENCE]
          INTEGER       NP [REFERENCE]
          INTEGER       IPLOC (*)
          INTEGER       IPCLST (*)
          CHARACTER     MLT (*)
          CHARACTER     ZON (*)
          CHARACTER     INAME (*)
        END SUBROUTINE mfLibExp_ArrayPar
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_ArrayParUsed(PNAME,PTYPE,INAME)
          CHARACTER*10  PNAME
          CHARACTER*3   PTYPE
          CHARACTER*10  INAME
        END SUBROUTINE mfLibExp_ArrayParUsed
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_NameFileItem(FTYPE,FNAME,NIU)
          CHARACTER   FTYPE (*)
          CHARACTER   FNAME (*)
          INTEGER     NIU
        END SUBROUTINE mfLibExp_NameFileItem
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_GAGE(IGGLST,NUMGAGE)
          INTEGER     IGGLST(*)
          INTEGER     NUMGAGE
        END SUBROUTINE mfLibExp_GAGE
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_LGR_Name(FNAME)
          CHARACTER   FNAME (*)
        END SUBROUTINE mfLibExp_LGR_Name
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_NameFile_Filename(MODTYPE,IGRID,FNAME)
          INTEGER     MODTYPE [REFERENCE]
          INTEGER     IGRID [REFERENCE]
          CHARACTER   FNAME (*)
        END SUBROUTINE mfLibExp_NameFile_Filename
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_HUF1(IHUFCB,HDRY,NHUF,NPHUF,IOHUFHEADS,IOHUFFLOWS,LTHUF,LAYWT)
          INTEGER      IHUFCB [REFERENCE]
          REAL         HDRY [REFERENCE]
          INTEGER      NHUF [REFERENCE]
          INTEGER      NPHUF [REFERENCE]
          INTEGER      IOHUFHEADS [REFERENCE]
          INTEGER      IOHUFFLOWS [REFERENCE]
          INTEGER      LTHUF (*)
          INTEGER      LAYWT (*)
        END SUBROUTINE mfLibExp_HUF1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_HufFlag(IHGUFLG)
          INTEGER      IHGUFLG (*)
        END SUBROUTINE mfLibExp_HufFlag
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_HufWet(WETFCT,IWETIT,IHDWET)
          REAL         WETFCT [REFERENCE]
          INTEGER      IWETIT [REFERENCE]
          INTEGER      IHDWET [REFERENCE]
        END SUBROUTINE mfLibExp_HufWet
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_HufHani(HGUNAM,HGUHANI,HGUVANI)
          CHARACTER    HGUNAM (*)
          REAL         HGUHANI (*)
          REAL         HGUVANI (*)
        END SUBROUTINE mfLibExp_HufHani
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_HufPar(PNAME,PTYPE,PVAL,NP,IPLOC,IPCLST,MLT,ZON,HGUNAM)
          CHARACTER*10  PNAME
          CHARACTER*3   PTYPE
          REAL          PVAL [REFERENCE]
          INTEGER       NP [REFERENCE]
          INTEGER       IPLOC (*)
          INTEGER       IPCLST (*)
          CHARACTER     MLT (*)
          CHARACTER     ZON (*)
          CHARACTER     HGUNAM (*)
        END SUBROUTINE mfLibExp_HufPar
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_UZFLine1(NUZTOP,IUZFOPT,IRUNFLG,IETFLG,IUZFCB1,IUZFCB2,NTRAIL2,NSETS2,NUZGAG,SURFDEP)
          INTEGER  NUZTOP [REFERENCE]
          INTEGER  IUZFOPT [REFERENCE]
          INTEGER  IRUNFLG [REFERENCE]
          INTEGER  IETFLG [REFERENCE]
          INTEGER  IUZFCB1 [REFERENCE]
          INTEGER  IUZFCB2 [REFERENCE]
          INTEGER  NTRAIL2 [REFERENCE]
          INTEGER  NSETS2 [REFERENCE]
          INTEGER  NUZGAG [REFERENCE]
          REAL     SURFDEP [REFERENCE]
        END SUBROUTINE mfLibExp_UZFLine1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_UZFLine8(IUZLIST)
          INTEGER IUZLIST (*)
        END SUBROUTINE mfLibExp_UZFLine8
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_UZFStressPeriod(NUZF1,NUZF2,NUZF3,NUZF4)
          INTEGER  NUZF1 [REFERENCE]
          INTEGER  NUZF2 [REFERENCE]
          INTEGER  NUZF3 [REFERENCE]
          INTEGER  NUZF4 [REFERENCE]
        END SUBROUTINE mfLibExp_UZFStressPeriod
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_VDFLine5(MT3DRHOFLG,MFNADVFD,NSWTCPL,IWTABLE,&
                                     DENSEMIN,DENSEMAX,DNSCRIT,DENSEREF,DRHODC,&
                                     DRHODPRHD,PRHDREF,NSRHOEOS,MTRHOSPEC,&
                                     CRHOREF,FIRSTDT)
          INTEGER  MT3DRHOFLG [REFERENCE]
          INTEGER  MFNADVFD [REFERENCE]
          INTEGER  NSWTCPL [REFERENCE]
          INTEGER  IWTABLE [REFERENCE]
          REAL     DENSEMIN [REFERENCE]
          REAL     DENSEMAX [REFERENCE]
          REAL     DNSCRIT [REFERENCE]
          REAL     DENSEREF [REFERENCE]
          REAL     DRHODC(*)
          REAL     DRHODPRHD [REFERENCE]
          REAL     PRHDREF [REFERENCE]
          INTEGER  NSRHOEOS [REFERENCE]
          INTEGER  MTRHOSPEC(*)
          REAL     CRHOREF(*)
          REAL     FIRSTDT [REFERENCE]
        END SUBROUTINE mfLibExp_VDFLine5
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_VDFStressPeriod(INDENSE)
          INTEGER INDENSE [REFERENCE]
        END SUBROUTINE mfLibExp_VDFStressPeriod
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_VSCLine3(MT3DMUFLG,VISCMIN,VISCMAX,VISCREF,DMUDC,&
                                     CMUREF,NSMUEOS,MUTEMPOPT,MTMUSPEC,&
                                     MTMUTEMPSPEC,AMUCOEFF)
          INTEGER  MT3DMUFLG [REFERENCE]
          REAL     VISCMIN [REFERENCE]
          REAL     VISCMAX [REFERENCE]
          REAL     VISCREF [REFERENCE]
          REAL     DMUDC(*)
          REAL     CMUREF(*)
          INTEGER  NSMUEOS [REFERENCE]
          INTEGER  MUTEMPOPT [REFERENCE]
          INTEGER  MTMUSPEC(*)
          INTEGER  MTMUTEMPSPEC [REFERENCE]
          REAL     AMUCOEFF(*)
        END SUBROUTINE mfLibExp_VSCLine3
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE mfLibExp_VSCStressPeriod(INVISC)
          INTEGER INVISC [REFERENCE]
        END SUBROUTINE mfLibExp_VSCStressPeriod
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_NWTLN1(toldum,ftoldum,Mxiter,Thickdum,Linmeth,&
                                   IPRNWT,IBOTAV,IFDPARAM,thetadum,&
                                   akappadum,gammadum,amomentdum,Btrack,&
                                   Numtrack,Btoldum,Breducdum)
          REAL    toldum [REFERENCE]
          REAL    ftoldum [REFERENCE]
          INTEGER Mxiter [REFERENCE]
          REAL    Thickdum [REFERENCE]
          INTEGER Linmeth [REFERENCE]
          INTEGER IPRNWT [REFERENCE]
          INTEGER IBOTAV [REFERENCE]
          INTEGER IFDPARAM [REFERENCE]
          REAL    thetadum [REFERENCE]
          REAL    akappadum [REFERENCE]
          REAL    gammadum [REFERENCE]
          REAL    amomentdum [REFERENCE]
          INTEGER Btrack [REFERENCE]
          INTEGER Numtrack [REFERENCE]
          REAL    Btoldum [REFERENCE]
          REAL    Breducdum [REFERENCE]
        END SUBROUTINE MFLIBEXP_NWTLN1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_NWTLN2(IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOLS,&
                                   IDROPTOL,EPSRNS,HCLOSEXMDDUM,MXITERXMD)
          INTEGER IACL [REFERENCE]
          INTEGER NORDER [REFERENCE]
          INTEGER LEVEL [REFERENCE]
          INTEGER NORTH [REFERENCE]
          INTEGER IREDSYS [REFERENCE]
          REAL    RRCTOLS [REFERENCE]
          INTEGER IDROPTOL [REFERENCE]
          REAL    EPSRNS [REFERENCE]
          REAL    HCLOSEXMDDUM [REFERENCE]
          INTEGER MXITERXMD [REFERENCE]
        END SUBROUTINE MFLIBEXP_NWTLN2
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_NWTLN2A(Maxitr_gmres,Ilu_method,Lev_fill,&
                                    Stop_toldum,Msdr)
          INTEGER Maxitr_gmres [REFERENCE]
          INTEGER Ilu_method [REFERENCE]
          INTEGER Lev_fill [REFERENCE]
          REAL    Stop_toldum [REFERENCE]
          INTEGER Msdr [REFERENCE]
        END SUBROUTINE MFLIBEXP_NWTLN2A
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_UPWA(NLAY,IUPWCB,HDRY,IPHDRY,LAYTYPUPW,&
                                 LAYAVG,CHANI,LAYVKAUPW,LAYWET)
          INTEGER NLAY [REFERENCE]
          INTEGER IUPWCB [REFERENCE]
          REAL    HDRY [REFERENCE]
          INTEGER IPHDRY [REFERENCE]
          INTEGER LAYTYPUPW (*)
          INTEGER LAYAVG (*)
          REAL    CHANI (*)
          INTEGER LAYVKAUPW (*)
          INTEGER LAYWET (*)
        END SUBROUTINE MFLIBEXP_UPWA
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_IUNIT(IUNIT,CUNIT)
          INTEGER       IUNIT [REFERENCE]
          CHARACTER*4   CUNIT
        END SUBROUTINE MFLIBEXP_IUNIT
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNW2LN1(MNWMAX,IWL2CB,MNWPRNT,NAUX,MNWAUX)
          INTEGER       MNWMAX [REFERENCE]
          INTEGER       IWL2CB [REFERENCE]
          INTEGER       MNWPRNT [REFERENCE]
          INTEGER       NAUX [REFERENCE]
          CHARACTER     MNWAUX (*)
        END SUBROUTINE MFLIBEXP_MNW2LN1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNW2LN2AB(WELLID,NNODES,LOSSTYPE,PUMPLOC,&
                                      Qlimit,PPFLAG,PUMPCAP)
          CHARACTER     WELLID(*)
          INTEGER       NNODES [REFERENCE]
          CHARACTER     LOSSTYPE(*)
          INTEGER       PUMPLOC [REFERENCE]
          INTEGER       Qlimit [REFERENCE]
          INTEGER       PPFLAG [REFERENCE]
          INTEGER       PUMPCAP [REFERENCE]
        END SUBROUTINE MFLIBEXP_MNW2LN2AB
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNW2LN2C(Rw,Rskin,Kskin,B,C,P,CWC,LnDesc)
          DOUBLE PRECISION          Rw [REFERENCE]
          DOUBLE PRECISION          Rskin [REFERENCE]
          DOUBLE PRECISION          Kskin [REFERENCE]
          DOUBLE PRECISION          B [REFERENCE]
          DOUBLE PRECISION          C [REFERENCE]
          DOUBLE PRECISION          P [REFERENCE]
          DOUBLE PRECISION          CWC [REFERENCE]
          CHARACTER     LnDesc(*)
        END SUBROUTINE MFLIBEXP_MNW2LN2C
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNW2LN2D1(IL,IR,IC,RwNode,RskinNode,&
                                      KskinNode,BNode,CNode,PNode,&
                                      CWCNode,PP,LnDesc)
          INTEGER                   IL [REFERENCE]
          INTEGER                   IR [REFERENCE]
          INTEGER                   IC [REFERENCE]
          DOUBLE PRECISION          RwNode [REFERENCE]
          DOUBLE PRECISION          RskinNode [REFERENCE]
          DOUBLE PRECISION          KskinNode [REFERENCE]
          DOUBLE PRECISION          BNode [REFERENCE]
          DOUBLE PRECISION          CNode [REFERENCE]
          DOUBLE PRECISION          PNode [REFERENCE]
          DOUBLE PRECISION          CWCNode [REFERENCE]
          DOUBLE PRECISION          PP [REFERENCE]
          CHARACTER                 LnDesc(*)
        END SUBROUTINE MFLIBEXP_MNW2LN2D1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNW2LN2D2(Ztop,Zbotm,IR,IC,RwNode,RskinNode,&
                                      KskinNode,BNode,CNode,PNode,&
                                      CWCNode,PP,LnDesc)
          DOUBLE PRECISION          Ztop [REFERENCE]
          DOUBLE PRECISION          Zbotm [REFERENCE]
          INTEGER                   IR [REFERENCE]
          INTEGER                   IC [REFERENCE]
          DOUBLE PRECISION          RwNode [REFERENCE]
          DOUBLE PRECISION          RskinNode [REFERENCE]
          DOUBLE PRECISION          KskinNode [REFERENCE]
          DOUBLE PRECISION          BNode [REFERENCE]
          DOUBLE PRECISION          CNode [REFERENCE]
          DOUBLE PRECISION          PNode [REFERENCE]
          DOUBLE PRECISION          CWCNode [REFERENCE]
          DOUBLE PRECISION          PP [REFERENCE]
          CHARACTER                 LnDesc(*)
        END SUBROUTINE MFLIBEXP_MNW2LN2D2
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNW2LN2E(PUMPLAY,PUMPROW,PUMPCOL,Zpump,LnDesc)
          INTEGER                   PUMPLAY [REFERENCE]
          INTEGER                   PUMPROW [REFERENCE]
          INTEGER                   PUMPCOL [REFERENCE]
          DOUBLE PRECISION          Zpump [REFERENCE]
          CHARACTER                 LnDesc(*)
        END SUBROUTINE MFLIBEXP_MNW2LN2E
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNW2LN2F(Hlim,QCUT,Qfrcmn,Qfrcmx,LnDesc)
          DOUBLE PRECISION          Hlim [REFERENCE]
          INTEGER                   QCUT [REFERENCE]
          DOUBLE PRECISION          Qfrcmn [REFERENCE]
          DOUBLE PRECISION          Qfrcmx [REFERENCE]
          CHARACTER                 LnDesc(*)
        END SUBROUTINE MFLIBEXP_MNW2LN2F
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNW2LN2G(Hlift,LIFTq0,LIFTqdes,HWtol,LnDesc)
          DOUBLE PRECISION          Hlift [REFERENCE]
          DOUBLE PRECISION          LIFTq0 [REFERENCE]
          DOUBLE PRECISION          LIFTqdes [REFERENCE]
          DOUBLE PRECISION          HWtol [REFERENCE]
          CHARACTER                 LnDesc(*)
        END SUBROUTINE MFLIBEXP_MNW2LN2G
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNW2LN2H(Liftn,Qn,LnDesc)
          DOUBLE PRECISION          Liftn [REFERENCE]
          DOUBLE PRECISION          Qn [REFERENCE]
          CHARACTER                 LnDesc(*)
        END SUBROUTINE MFLIBEXP_MNW2LN2H
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNW2LN34(ITMP,MNW2,NMNWVL,MNWMAX,NAUX)
          INTEGER          ITMP [REFERENCE]
          DOUBLE PRECISION MNW2(*)
          INTEGER          NMNWVL [REFERENCE]
          INTEGER          MNWMAX [REFERENCE]
          INTEGER          NAUX   [REFERENCE]
        END SUBROUTINE MFLIBEXP_MNW2LN34
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNWILN1(Wel1flag,QSUMflag,BYNDflag)
          INTEGER       Wel1flag [REFERENCE]
          INTEGER       QSUMflag [REFERENCE]
          INTEGER       BYNDflag [REFERENCE]
        END SUBROUTINE MFLIBEXP_MNWILN1
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNWILN2(MNWOBS)
          INTEGER       MNWOBS [REFERENCE]
        END SUBROUTINE MFLIBEXP_MNWILN2
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNWILN3(WELLID,UNIT,QNDflag,QBHflag,CONCflag)
          CHARACTER     WELLID(*)
          INTEGER       UNIT [REFERENCE]
          INTEGER       QNDflag [REFERENCE]
          INTEGER       QBHflag [REFERENCE]
          INTEGER       CONCflag [REFERENCE]
        END SUBROUTINE MFLIBEXP_MNWILN3
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_MNWIEND()
        END SUBROUTINE MFLIBEXP_MNWIEND
!     ------------------------------------------------------------------
!      Declare the C function
        SUBROUTINE MFLIBEXP_COMMENT(UNIT,LINE)
          INTEGER          UNIT [REFERENCE]
          CHARACTER        LINE(*)
        END SUBROUTINE MFLIBEXP_COMMENT

     END INTERFACE

  public:: exp_GeoDB, exp_GLO1BAS6DF, exp_GLO1BAS6RP, &
           exp_ListPackage, exp_SipPackage, exp_De4Line1, exp_De4Line2, &
           exp_SorPackage, exp_PcgPackage, exp_LmgPackage, exp_GmgPackage, &
           exp_SmsPackage, exp_SmsXmdPackage, exp_SmsPcguPackage, &
           exp_Gnc1
  save
  REAL    ::  m_LMG_STOR1, m_LMG_STOR2, m_LMG_STOR3
  integer ::  m_LMG_ICG

  contains

  ! code starts here
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_GeoDB (a_arg, a_fileName)
    implicit none
    character(*), intent(in) :: a_arg, a_fileName
    
    call mfLibExp_GeoDb(a_arg, a_fileName)
  end subroutine exp_GeoDB

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_GmsH5 (a_arg, a_fileName)
    implicit none
    character(*), intent(in) :: a_arg, a_fileName
    
    call mfLibExp_GMS(a_arg, a_fileName)
  end subroutine exp_GmsH5

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_Tables (a_arg)
    implicit none
    character(*), intent(in) :: a_arg
    
    call mfLibExp_Tables(a_arg)
  end subroutine exp_Tables

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_putCurrentPeriod (KPER)
    implicit none
    integer, intent(in) :: KPER

    !if (NOT(ed_getExportData())) return
    call mfLibExp_putCurrentPeriod(KPER)
  end subroutine exp_putCurrentPeriod

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_putCurrentGrid (IGRID)
    implicit none
    integer, intent(in) :: IGRID

    !if (NOT(ed_getExportData())) return
    call mfLibExp_putCurrentGrid(IGRID)
  end subroutine exp_putCurrentGrid

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_GLO1BAS6DF (NLAY,NROW,NCOL,NPER,ITMUNI,LENUNI,LAYCBD,IUNSTR)
    implicit none
    integer, intent(in) :: NLAY,NROW,NCOL,NPER,ITMUNI,LENUNI,LAYCBD(*),IUNSTR

    !if (NOT(ed_getExportData())) return
    call mfLibExp_DisPackage1(NLAY,NROW,NCOL,NPER,ITMUNI,LENUNI,LAYCBD,IUNSTR)
  end subroutine exp_GLO1BAS6DF

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_GLO1BAS6RP (DELR,DELC,NBOTM,BOTM,PERLEN,NSTP,&
                             TSMULT,ISSFLG)
    implicit none
    integer, intent(in)    :: NBOTM,NSTP(*),ISSFLG(*)
    real, intent(in)       :: DELR(*),DELC(*),BOTM(*),PERLEN(*),TSMULT(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_DisPackage2(DELR,DELC,NBOTM,BOTM,PERLEN,NSTP,TSMULT,ISSFLG)
  end subroutine exp_GLO1BAS6RP

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ListPackage (BCTYPE,ITMP,MXBC,NBC,NVALS,NAUX,BCDATA,NP,AUXNMS)
    implicit none
    integer, intent(in)      :: ITMP,MXBC,NBC,NVALS,NAUX,NP
    real, intent(in)         :: BCDATA(NVALS,MXBC)
    character*16, intent(in) :: AUXNMS(5)
    character*3, intent(in)  :: BCTYPE

    character*3 myType

    myType = BCTYPE
    if (NOT(ed_getExportData())) return
    call mfLibExp_ListPackage(BCTYPE,ITMP,MXBC,NBC,NVALS,NAUX,BCDATA,NP,AUXNMS)
  end subroutine exp_ListPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SipPackage (BCTYPE,MXITER,NPARM,ACCL,HCLOSE,IPCALC,WSEED,IPRSIP)
    implicit none
    integer, intent(in)      :: MXITER,NPARM,IPCALC,IPRSIP
    real, intent(in)         :: ACCL,HCLOSE,WSEED
    character*3, intent(in)  :: BCTYPE

    character*3 myType
    integer myIPRSIP

    myType = BCTYPE
    if (NOT(ed_getExportData())) return
    myIPRSIP = IPRSIP
    IF(myIPRSIP.EQ.999)myIPRSIP=0
    call mfLibExp_SipPackage(BCTYPE,MXITER,NPARM,ACCL,HCLOSE,IPCALC,WSEED,myIPRSIP)
  end subroutine exp_SipPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_De4Line1 (ITMX,MXUP,MXLOW,MXBW)
    implicit none
    integer, intent(in)      :: ITMX,MXUP,MXLOW,MXBW

    if (NOT(ed_getExportData())) return
    call mfLibExp_De4Line1(ITMX,MXUP,MXLOW,MXBW)
  end subroutine exp_De4Line1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_De4Line2 (IFREQ,MUTD4,ACCL,HCLOSE,IPRD4)
    implicit none
    integer, intent(in)      :: IFREQ,MUTD4,IPRD4
    real, intent(in)         :: ACCL,HCLOSE

    if (NOT(ed_getExportData())) return
    call mfLibExp_De4Line2(IFREQ,MUTD4,ACCL,HCLOSE,IPRD4)
  end subroutine exp_De4Line2

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SorPackage (BCTYPE,MXITER,ACCL,HCLOSE,IPRSOR)
    implicit none
    integer, intent(in)      :: MXITER,IPRSOR
    real, intent(in)         :: ACCL,HCLOSE
    character*3, intent(in)  :: BCTYPE

    character*3 myType
    integer myIPRSOR

    myType = BCTYPE
    if (NOT(ed_getExportData())) return

    myIPRSOR = IPRSOR
    IF(myIPRSOR.EQ.999)myIPRSOR=0
    call mfLibExp_SorPackage(BCTYPE,MXITER,ACCL,HCLOSE,myIPRSOR)
  end subroutine exp_SorPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_PcgPackage (BCTYPE,MXITER,ITER1,NPCOND,HCLOSE,RCLOSE,RELAX,NBPOL,IPRPCG,MUTPCG,DAMP)
    implicit none
    integer, intent(in)      :: MXITER,ITER1,NPCOND,NBPOL,IPRPCG,MUTPCG
    real, intent(in)         :: HCLOSE,RCLOSE,RELAX,DAMP
    character*3, intent(in)  :: BCTYPE

    character*3 myType
    integer myIPRPCG

    myType = BCTYPE
    if (NOT(ed_getExportData())) return

    myIPRPCG = IPRPCG
    IF(myIPRPCG.EQ.999)myIPRPCG=0
    call mfLibExp_PcgPackage(BCTYPE,MXITER,ITER1,NPCOND,HCLOSE,RCLOSE,RELAX,NBPOL,myIPRPCG,MUTPCG,DAMP)
  end subroutine exp_PcgPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_LmgLine1 (STOR1, STOR2, STOR3, ICG)
    implicit none
    integer, intent(in)      :: ICG
    real, intent(in)         :: STOR1,STOR2,STOR3

    if (NOT(ed_getExportData())) return
    m_LMG_STOR1 = STOR1
    m_LMG_STOR2 = STOR2
    m_LMG_STOR3 = STOR3
    m_LMG_ICG = ICG
  end subroutine exp_LmgLine1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_LmgPackage(BCTYPE,MXITER,MXCYC,BCLOSE,DAMP,IOUTAMG,DUP,DLOW,HCLOSE,CONTROL)
    implicit none
    integer, intent(in)      :: MXITER,MXCYC,IOUTAMG,CONTROL
    real, intent(in)         :: BCLOSE,DAMP,DUP,DLOW,HCLOSE
    character*3, intent(in)  :: BCTYPE

    character*3 myType
    if (NOT(ed_getExportData())) return
    call mfLibExp_LmgPackage(BCTYPE,m_LMG_STOR1,m_LMG_STOR2,m_LMG_STOR3,m_LMG_ICG,MXITER,MXCYC,BCLOSE,DAMP,IOUTAMG,DUP,DLOW,HCLOSE,CONTROL)

  end subroutine exp_LmgPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_GmgPackage(BCTYPE,RCLOSE,IITER,HCLOSE,MXITER,DAMP,IADAMP,IOUTGMG,ISM,ISC,RELAX)
    implicit none
    integer, intent(in)      :: IITER,MXITER,IADAMP,IOUTGMG,ISM,ISC
    real, intent(in)         :: RCLOSE,HCLOSE,DAMP
    doubleprecision, intent(in) :: RELAX
    character*3, intent(in)  :: BCTYPE

    character*3 myType
    !integer myIPRSOR

    !myType = BCTYPE
    if (NOT(ed_getExportData())) return

    !myIPRSOR = IPRSOR
    !IF(myIPRSOR.EQ.999)myIPRSOR=0
    call mfLibExp_GmgPackage(BCTYPE,RCLOSE,IITER,HCLOSE,MXITER,DAMP,IADAMP,IOUTGMG,ISM,ISC,RELAX)
  end subroutine exp_GmgPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SmsPackage (IFDPARAM,HCLOSE,HICLOSE,MXITER,ITER1,IPRSMS,NONMETH,LINMETH,THETA,AKAPPA,GAMMA,AMOMENTUM,NUMTRACK,BTOL,BREDUC,RESLIM)
    implicit none
    integer, intent(in)         :: IFDPARAM,MXITER,ITER1,IPRSMS,NONMETH,LINMETH,NUMTRACK
    doubleprecision, intent(in) :: HCLOSE,HICLOSE,THETA,AKAPPA,GAMMA,AMOMENTUM,BTOL,BREDUC,RESLIM

    if (NOT(ed_getExportData())) return

    call mfLibExp_SmsPackage(IFDPARAM,HCLOSE,HICLOSE,MXITER,ITER1,IPRSMS,NONMETH,LINMETH,THETA,AKAPPA,GAMMA,AMOMENTUM,NUMTRACK,BTOL,BREDUC,RESLIM)
  end subroutine exp_SmsPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SmsXmdPackage (IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOL,IDROPTOL,EPSRN)
    implicit none
    integer, intent(in)         :: IACL,NORDER,LEVEL,NORTH,IREDSYS,IDROPTOL
    doubleprecision, intent(in) :: RRCTOL,EPSRN

    if (NOT(ed_getExportData())) return

    call mfLibExp_SmsXmdPackage(IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOL,IDROPTOL,EPSRN)
  end subroutine exp_SmsXmdPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SmsPcguPackage (IPC,ISCL,IORD,RCLOSEPCGU)
    implicit none
    integer, intent(in)      :: IPC,ISCL,IORD
    real, intent(in)         :: RCLOSEPCGU

    if (NOT(ed_getExportData())) return

    call mfLibExp_SmsPcguPackage(IPC,ISCL,IORD,RCLOSEPCGU)
  end subroutine exp_SmsPcguPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_NwtPackageLn1(toldum,ftoldum,Mxiter,Thickdum,Linmeth,IPRNWT,&
                               IBOTAV,IFDPARAM,thetadum,akappadum,gammadum,&
                               amomentdum,Btrack,Numtrack,Btoldum,Breducdum)
    implicit none
    real, intent(in)::    toldum,ftoldum,Thickdum,thetadum,akappadum,gammadum,&
                          amomentdum,Btoldum,Breducdum
    integer, intent(in):: Mxiter,Linmeth,IPRNWT,IBOTAV,IFDPARAM,Btrack,&
                          Numtrack
                          
    if (NOT(ed_getExportData())) return
    call MFLIBEXP_NWTLN1(toldum,ftoldum,Mxiter,Thickdum,Linmeth,IPRNWT,&
                         IBOTAV,IFDPARAM,thetadum,akappadum,gammadum,&
                         amomentdum,Btrack,Numtrack,Btoldum,Breducdum)

  end subroutine exp_NwtPackageLn1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_NwtPackageLn2(IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOLS,&
                               IDROPTOL,EPSRNS,HCLOSEXMDDUM,MXITERXMD)
    implicit none
    integer, intent(in) :: IACL,NORDER,LEVEL,NORTH,IREDSYS,IDROPTOL,MXITERXMD
    real, intent(in)    :: RRCTOLS,EPSRNS,HCLOSEXMDDUM
    
    if (NOT(ed_getExportData())) return
    call MFLIBEXP_NWTLN2(IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOLS,&
                         IDROPTOL,EPSRNS,HCLOSEXMDDUM,MXITERXMD)
  end subroutine exp_NwtPackageLn2
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_NwtPackageLn2a(Maxitr_gmres,Ilu_method,Lev_fill,Stop_toldum,&
                                Msdr)
    implicit none
    integer, intent(in) :: Maxitr_gmres,Ilu_method,Lev_fill,Msdr
    real, intent(in)    :: Stop_toldum
    
    if (NOT(ed_getExportData())) return
    call MFLIBEXP_NWTLN2A(Maxitr_gmres,Ilu_method,Lev_fill,Stop_toldum,&
                          Msdr)
  end subroutine exp_NwtPackageLn2a

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_UPW1(NLAY,IUPWCB,HDRY,IPHDRY,LAYTYPUPW,LAYAVG,CHANI,&
                      LAYVKAUPW,LAYWET)
    implicit none
    integer, intent(in) :: NLAY,IUPWCB,IPHDRY,LAYTYPUPW(*),LAYAVG(*),&
                           LAYVKAUPW(*),LAYWET(*)
    real, intent(in)    :: HDRY,CHANI(*)
    
    if (NOT(ed_getExportData())) return
    call MFLIBEXP_UPWA(NLAY,IUPWCB,HDRY,IPHDRY,LAYTYPUPW,LAYAVG,CHANI,&
                       LAYVKAUPW,LAYWET)
  end subroutine exp_UPW1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ETPackage (PACK,NEVTOP,INSURF,INEVTR,INEXDP,INIEVT,NETSEG,INSGDF)
    implicit none
    character(*), intent(in)  :: PACK
    integer, intent(in)      :: NEVTOP,INSURF,INEVTR,INEXDP,INIEVT,NETSEG,INSGDF

    if (NOT(ed_getExportData())) return
    call mfLibExp_ET(PACK,NEVTOP,INSURF,INEVTR,INEXDP,INIEVT,NETSEG,INSGDF)
  end subroutine exp_ETPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_RCHPackage (NRCHOP,INRECH,INIRCH)
    implicit none
    integer, intent(in)      :: NRCHOP,INRECH,INIRCH

    if (NOT(ed_getExportData())) return
    call mfLibExp_RCH(NRCHOP,INRECH,INIRCH)
  end subroutine exp_RCHPackage

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_Gnc1 (NPGNCn,MXGNn,NGNCNPn,MXADJn,I2Kn,ISYMGNCn,IFLALPHAn, &
                      IPRGNCn,N1,N2,GNCn)
    implicit none
    integer, intent(in) :: NPGNCn,MXGNn,NGNCNPn,MXADJn,I2Kn,ISYMGNCn,&
                           IFLALPHAn,IPRGNCn
    integer, intent(in) :: N1, N2
    real, intent(in)    :: GNCn(N1,N2)

    if (NOT(ed_getExportData())) return
    call mfLibExp_Gnc1(NPGNCn,MXGNn,NGNCNPn,MXADJn,I2Kn,ISYMGNCn,IFLALPHAn, &
                      IPRGNCn,N1,N2,GNCn)
  end subroutine exp_Gnc1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SingleValInt (PACKNAME,NAME,VAL)
    implicit none
    character*3, intent(in)  :: PACKNAME
    character*10, intent(in) :: NAME
    integer, intent(in)      :: VAL

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValInt(PACKNAME,NAME,VAL);
  end subroutine exp_SingleValInt

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SingleValFlt (PACKNAME,NAME,VAL)
    implicit none
    character*3, intent(in)  :: PACKNAME
    character*10, intent(in) :: NAME
    real, intent(in)         :: VAL

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt(PACKNAME,NAME,VAL);
  end subroutine exp_SingleValFlt

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SingleValDbl (PACKNAME,NAME,VAL)
    implicit none
    character*3, intent(in)      :: PACKNAME
    character*10, intent(in)     :: NAME
    double precision, intent(in) :: VAL

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValDbl(PACKNAME,NAME,VAL);
  end subroutine exp_SingleValDbl

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SingleValStr (PACKNAME,NAME,VAL,LEN)
    implicit none
    character*3, intent(in)  :: PACKNAME
    character*10, intent(in) :: NAME
    character, intent(in)    :: VAL(*)
    integer, intent(in)      :: LEN

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValStr(PACKNAME,NAME,VAL,LEN);
  end subroutine exp_SingleValStr

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ExportPack (PACKNAME)
    implicit none
    character*3, intent(in)  :: PACKNAME

    if (NOT(ed_getExportData())) return
    call mfLibExp_ExpPack(PACKNAME);
  end subroutine exp_ExportPack

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_LPF1 (NLAY,ILPFCB,HDRY,LAYTYP,LAYAVG,CHANI,LAYVKA,LAYWET,VERTLEAKFLAG, MF2K5)
    implicit none
    integer ::MF2K5
    integer, intent(in)      :: NLAY,ILPFCB,VERTLEAKFLAG,LAYTYP(*),&
                                LAYAVG(*),LAYVKA(*),LAYWET(*)
    real, intent (in)        :: HDRY,CHANI(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_LPF1(NLAY,ILPFCB,HDRY,LAYTYP,LAYAVG,CHANI,LAYVKA,LAYWET,VERTLEAKFLAG, MF2K5);
  end subroutine exp_LPF1
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_FP_opt (PCK,ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC)
    implicit none
    character*3, intent(in)  :: PCK
    integer, intent(in)      :: ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC

    if (NOT(ed_getExportData())) return
    call mfLibExp_FP_opt(PCK,ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC);
  end subroutine exp_FP_opt

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_BCF1 (NLAY,IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET,LAYCON,LAYAVG)
    implicit none
    integer, intent(in)      :: NLAY,IBCFCB,IWDFLG,IWETIT,IHDWET,LAYCON(*),LAYAVG(*)
    real, intent (in)        :: HDRY,WETFCT

    if (NOT(ed_getExportData())) return
    call mfLibExp_BCF1(NLAY,IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET,LAYCON,LAYAVG)
  end subroutine exp_BCF1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_BCF2 (TRPY)
    implicit none
    real, intent (in)        :: TRPY(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_ArrayValFlt('BCF','TRPY      ',TRPY)
    call mfLibExp_ExpPack('BCF')
  end subroutine exp_BCF2

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_HFB (NHFBNP,HFB)
    implicit none
    integer, intent (in)     :: NHFBNP
    real, intent (in)        :: HFB(*)
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_HFB(NHFBNP,HFB)
  end subroutine exp_HFB

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SEN (ISENALL,IUHEAD,IPRINTS,INENSU,ISENPU,ISENFM)
    implicit none
    integer, intent (in)     :: ISENALL,IUHEAD,IPRINTS,INENSU,ISENPU,ISENFM
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SEN(ISENALL,IUHEAD,IPRINTS,INENSU,ISENPU,ISENFM)
  end subroutine exp_SEN

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_PVAL (PARNAM,B,NPVAL)
    implicit none
    integer, intent (in)            :: NPVAL
    CHARACTER(LEN=10),intent (in)   :: PARNAM(*)
    REAL,intent (in)                :: B(*)
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_PVAL(PARNAM,B,NPVAL)
  end subroutine exp_PVAL

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_PES (ITMXP,DMAX,RTOL,SOSC,IBEFLG,IYCFLG,IOSTAR,NOPT,&
                      NFIT,SOSR,RMAR,RMARM,IAP,IPRC,IPRINT,LPRINT,&
                      CSA,FCONV,LASTX)
    implicit none
    integer, intent (in)     :: ITMXP,IBEFLG,IYCFLG,IOSTAR,NOPT,&
                                NFIT,IAP,IPRC,IPRINT,LPRINT,LASTX
    real, intent (in)        :: DMAX,RTOL,SOSC,SOSR,RMAR,RMARM,&
                                CSA,FCONV
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_PES(ITMXP,DMAX,RTOL,SOSC,IBEFLG,IYCFLG,IOSTAR,NOPT,&
                      NFIT,SOSR,RMAR,RMARM,IAP,IPRC,IPRINT,LPRINT,&
                      CSA,FCONV,LASTX)
  end subroutine exp_PES

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_U1DREL (NAME,JJ,ARR,MULT,IPRN)
    implicit none
    character*24, intent(in)  :: NAME
    integer, intent(in)       :: JJ,IPRN
    real, intent(in)          :: ARR(JJ),MULT


    if (NOT(ed_getExportData())) return

    call mfLibExp_SingleValInt(NAME,'JJ',JJ)
    call mfLibExp_SingleValInt(NAME,'IPRN',IPRN)
    call mfLibExp_SingleValFlt(NAME,'MULT',MULT)
    call mfLibExp_ArrayValFlt(NAME,'ARR',ARR)
    call mfLibExp_ExpPack(NAME)
  end subroutine exp_U1DREL

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_U2DREL (NAME,JJ,II,ARR,MULT,K,IPRN)
    implicit none
    character*24, intent(in)  :: NAME
    integer, intent(in)       :: JJ,II,K,IPRN
    real, intent(in)          :: ARR(JJ,II),MULT


    if (NOT(ed_getExportData())) then
      call mfLib_U2DREL_EXDATA(NAME,ARR,MULT,K,JJ,II)
      return
    end if

    call mfLibExp_U2DREL(NAME,ARR,MULT,K,IPRN)
  end subroutine exp_U2DREL

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_U2DREL8 (NAME,JJ,II,ARR,MULT,K,IPRN)
    implicit none
    character*24, intent(in)     :: NAME
    integer, intent(in)          :: JJ,II,K,IPRN
    double precision, intent(in) :: ARR(JJ,II)
    real, intent(in)             :: MULT


    if (NOT(ed_getExportData())) then
      call mfLib_U2DREL8_EXDATA(NAME,ARR,MULT,K,JJ,II)
      return
    end if

    call mfLibExp_U2DREL8(NAME,ARR,MULT,K,IPRN)
  end subroutine exp_U2DREL8

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_U2DINT (NAME,JJ,II,ARR,MULT,K,IPRN)
    implicit none
    character*24, intent(in)  :: NAME
    integer, intent(in)       :: JJ,II,K,ARR(JJ,II),MULT,IPRN

    if (NOT(ed_getExportData())) then
      call mfLib_U2DINT_EXDATA(NAME,ARR,MULT,K,JJ,II)
      return
    end if

    call mfLibExp_U2DINT(NAME,ARR,MULT,K,IPRN)
  end subroutine exp_U2DINT

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars1 (OUTNAM,ISCALS)
    implicit none
    character*200, intent(in) :: OUTNAM
    integer, intent(in)       :: ISCALS
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValStr('OB1','OUTNAM   ',OUTNAM,200)
    call mfLibExp_SingleValInt('OB1','ISCALS   ',ISCALS)
    call mfLibExp_ExpPack('OB1')
  end subroutine exp_OBSVars1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars2 (TOMULTHD,EVH)
    implicit none
    real, intent(in)       :: TOMULTHD, EVH
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OB2','TOMULTH  ',TOMULTHD)
    call mfLibExp_SingleValFlt('OB2','EVH      ',EVH)
    call mfLibExp_ExpPack('OB2')
  end subroutine exp_OBSVars2
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars2_2005 (TOMULTHD,IUHOBSV,HOBDRY)
    implicit none
    real, intent(in)       :: TOMULTHD, HOBDRY
    integer, intent(in)    :: IUHOBSV
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OV2','TOMULTH  ',TOMULTHD)
    call mfLibExp_SingleValInt('OV2','IUHOBSV  ',IUHOBSV)
    call mfLibExp_SingleValFlt('OV2','HOBDRY   ',HOBDRY)
    call mfLibExp_ExpPack('OV2')
  end subroutine exp_OBSVars2_2005

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars3 (TOMULTGB,EVFGB)
    implicit none
    real, intent(in)       :: TOMULTGB, EVFGB
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OB3','TOMULTGB ',TOMULTGB)
    call mfLibExp_SingleValFlt('OB3','EVFGB    ',EVFGB)
    call mfLibExp_ExpPack('OB3')
  end subroutine exp_OBSVars3
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars3_2005 (TOMULTGB,IUGBOBSV)
    implicit none
    real, intent(in)       :: TOMULTGB
    integer, pointer, intent(in)    :: IUGBOBSV
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OV3','TOMULTGB ',TOMULTGB)
    call mfLibExp_SingleValInt('OV3','IUGBOBSV ',IUGBOBSV)
    call mfLibExp_ExpPack('OV3')
  end subroutine exp_OBSVars3_2005

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars4 (TOMULTDR,EVFDR)
    implicit none
    real, intent(in)       :: TOMULTDR, EVFDR
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OB4','TOMULTDR ',TOMULTDR)
    call mfLibExp_SingleValFlt('OB4','EVFDR    ',EVFDR)
    call mfLibExp_ExpPack('OB4')
  end subroutine exp_OBSVars4
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars4_2005 (TOMULTDR,IUDROBSV)
    implicit none
    real, intent(in)       :: TOMULTDR
    integer, pointer, intent(in)    :: IUDROBSV
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OV4','TOMULTDR ',TOMULTDR)
    call mfLibExp_SingleValInt('OV4','IUDROBSV ',IUDROBSV)
    call mfLibExp_ExpPack('OV4')
  end subroutine exp_OBSVars4_2005

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars5 (TOMULTRV,EVFRV)
    implicit none
    real, intent(in)       :: TOMULTRV, EVFRV
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OB5','TOMULTRV ',TOMULTRV)
    call mfLibExp_SingleValFlt('OB5','EVFRV    ',EVFRV)
    call mfLibExp_ExpPack('OB5')
  end subroutine exp_OBSVars5
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars5_2005 (TOMULTRV,IURVOBSV)
    implicit none
    real, intent(in)       :: TOMULTRV
    integer, pointer, intent(in)    :: IURVOBSV
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OV5','TOMULTRV ',TOMULTRV)
    call mfLibExp_SingleValInt('OV5','IURVOBSV ',IURVOBSV)
    call mfLibExp_ExpPack('OV5')
  end subroutine exp_OBSVars5_2005
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars6 (TOMULTCH,EVFCH)
    implicit none
    real, intent(in)       :: TOMULTCH, EVFCH
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OB6','TOMULTCH ',TOMULTCH)
    call mfLibExp_SingleValFlt('OB6','EVFCH    ',EVFCH)
    call mfLibExp_ExpPack('OB6')
  end subroutine exp_OBSVars6
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars6_2005 (TOMULTCH,IUCHOBSV)
    implicit none
    real, intent(in)       :: TOMULTCH
    integer, pointer, intent(in)    :: IUCHOBSV
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OV6','TOMULTCH ',TOMULTCH)
    call mfLibExp_SingleValInt('OV6','IUCHOBSV ',IUCHOBSV)
    call mfLibExp_ExpPack('OV6')
  end subroutine exp_OBSVars6_2005
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars7 (TOMULTST,EVFST)
    implicit none
    real, intent(in)       :: TOMULTST, EVFST
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OB7','TOMULTST ',TOMULTST)
    call mfLibExp_SingleValFlt('OB7','EVFST    ',EVFST)
    call mfLibExp_ExpPack('OB7')
  end subroutine exp_OBSVars7
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars7_2005 (TOMULTST,IUSTOBSV)
    implicit none
    real, intent(in)       :: TOMULTST
    integer, pointer, intent(in)    :: IUSTOBSV
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OV7','TOMULTST ',TOMULTST)
    call mfLibExp_SingleValInt('OV7','IUSTOBSV ',IUSTOBSV)
    call mfLibExp_ExpPack('OV7')
  end subroutine exp_OBSVars7_2005
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OBSVars8 (TOMULTDT,EVFDT)
    implicit none
    real, intent(in)       :: TOMULTDT, EVFDT
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValFlt('OB8','TOMULTDT ',TOMULTDT)
    call mfLibExp_SingleValFlt('OB8','EVFDT    ',EVFDT)
    call mfLibExp_ExpPack('OB8')
  end subroutine exp_OBSVars8

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ObsHd3 (OBSNAME,LAYER,ROW,COL,IREFSP,TOFFSET,ROFF,COFF,HOBS,&
                         STAT,STATFLG,PLOT)
    implicit none
    character*12, intent(in) ::OBSNAME
    integer, intent(in)      ::LAYER,ROW,COL,IREFSP,STATFLG,PLOT
    real, intent(in)         ::TOFFSET,ROFF,COFF,HOBS,STAT
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_ObsHd(OBSNAME,LAYER,ROW,COL,IREFSP,TOFFSET,ROFF,COFF,HOBS,&
                        STAT,STATFLG,PLOT)
  end subroutine exp_ObsHd3

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ObsHd4 (MLAY,PR,ML)
    implicit none
    integer, intent(in)   :: ML
    integer, intent(in)   :: MLAY(ML)
    real, intent(in)      :: PR(ML)

    if (NOT(ed_getExportData())) return
    call mfLibExp_ObsHd4(MLAY,PR,ML)
  end subroutine exp_ObsHd4

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ObsHd5 (ITT)
    implicit none
    integer, intent(in)   :: ITT

    if (NOT(ed_getExportData())) return
    call mfLibExp_ObsHd5(ITT)
  end subroutine exp_ObsHd5

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ObsHd6 (NAME,IREFSP,TOFFSET,HOBS,STATH,STATDD,STATFLG,PLOT)
    implicit none
    character*12, intent(in) ::NAME
    integer, intent(in)      ::IREFSP,STATFLG,PLOT
    real, intent(in)         ::TOFFSET,HOBS,STATH,STATDD
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_ObsHd6(NAME,IREFSP,TOFFSET,HOBS,STATH,STATDD,STATFLG,PLOT)
  end subroutine exp_ObsHd6

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ObsFlo4 (OBTYPE,NAME,IREFSP,TOFFSET,HOBS,STAT,STATFLG,PLOT)
    implicit none
    character*4, intent(in)  ::OBTYPE
    character*12, intent(in) ::NAME
    integer, intent(in)      ::IREFSP,STATFLG,PLOT
    real, intent(in)         ::TOFFSET,HOBS,STAT

    if (NOT(ed_getExportData())) return
    call mfLibExp_ObsFlo4(OBTYPE,NAME,IREFSP,TOFFSET,HOBS,STAT,STATFLG,PLOT)
  end subroutine exp_ObsFlo4
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ObsFlo4_5 (OBTYPE,NAME,IREFSP,TOFFSET,FLWOBS)
    implicit none
    character*4, intent(in)  ::OBTYPE
    character*12, intent(in) ::NAME
    integer, intent(in)      ::IREFSP
    real, intent(in)         ::TOFFSET,FLWOBS

    if (NOT(ed_getExportData())) return
    call mfLibExp_ObsFlo4_5(OBTYPE,NAME,IREFSP,TOFFSET,FLWOBS)
  end subroutine exp_ObsFlo4_5

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ObsFlo5 (NUM,START,QCLS)
    implicit none
    integer, intent(in)      ::NUM,START
    real, intent(in)         ::QCLS(5,NUM)

    if (NOT(ed_getExportData())) return
    call mfLibExp_ObsFlo5(NUM,START,QCLS)
  end subroutine exp_ObsFlo5

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OCVars (IHEDFM,IDDNFM,IHEDUN,IDDNUN,IBDOPT,IAUXSV,CHEDFM)
    implicit none
    integer, intent(in)      ::IHEDFM,IDDNFM,IHEDUN,IDDNUN,IBDOPT,IAUXSV
    character*20, intent(in) ::CHEDFM

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValInt('OC ','IHEDFM   ',IHEDFM)
    call mfLibExp_SingleValInt('OC ','IDDNFM   ',IDDNFM)
    call mfLibExp_SingleValInt('OC ','IHEDUN   ',IHEDUN)
    call mfLibExp_SingleValInt('OC ','IDDNUN   ',IDDNUN)
    call mfLibExp_SingleValInt('OC ','IBDOPT   ',IBDOPT)
    call mfLibExp_SingleValInt('OC ','IAUXSV   ',IAUXSV)
    call mfLibExp_SingleValStr('OC ','CHEDFM   ',CHEDFM,20)
    call mfLibExp_ExpPack('OC ')
  end subroutine exp_OCVars

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_OCTS (KPER,KSTP,IHDDFL,IBUDFL,ICBCFL,NLAY,IOFLG)
    implicit none
    integer, intent(in)  ::KPER,KSTP,IHDDFL,IBUDFL,ICBCFL,NLAY,IOFLG(NLAY,5)

    if (NOT(ed_getExportData())) return
    if (IHDDFL.EQ.0 .AND. IBUDFL.EQ.0 .AND. ICBCFL.EQ.0 .AND. IOFLG(1,1).EQ.0 .AND. IOFLG(1,2).EQ.0 .AND. IOFLG(1,3).EQ.0 .AND. IOFLG(1,4).EQ.0) return
    call mfLibExp_SingleValInt('OCT','SPID     ',KPER)
    call mfLibExp_SingleValInt('OCT','TSNum    ',KSTP)
    call mfLibExp_SingleValInt('OCT','IHDDFL   ',IHDDFL)
    call mfLibExp_SingleValInt('OCT','IBUDFL   ',IBUDFL)
    call mfLibExp_SingleValInt('OCT','ICBCFL   ',ICBCFL)
    call mfLibExp_SingleValInt('OCT','Hdpr     ',IOFLG(1,1))
    call mfLibExp_SingleValInt('OCT','Ddpr     ',IOFLG(1,2))
    call mfLibExp_SingleValInt('OCT','Hdsv     ',IOFLG(1,3))
    call mfLibExp_SingleValInt('OCT','Ddsv     ',IOFLG(1,4))
    call mfLibExp_SingleValInt('OCT','Ibsv     ',IOFLG(1,5))
    call mfLibExp_ExpPack('OCT')
  end subroutine exp_OCTS

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_LAKSP (NSOL,STAGES,SSMN,SSMX,CLAKE,ITMP,ITMP1,LWRT,LKARR,&
                        BDLKNC,NSLMS,IC,ISUB,SILLVT,PRCPLK,EVAPLK,RNF,WTHDRW,&
                        CPPT,CRNF,CAUG,NLAKES)
    implicit none
    integer, intent(in)     ::NSOL
    real, intent(in)        ::STAGES(*),SSMN(*),SSMX(*),CLAKE (*)
    integer, intent(in)     ::ITMP,ITMP1,LWRT,LKARR(*)
    real, intent(in)        ::BDLKNC(*)
    integer, intent(in)     ::NSLMS,IC(*),ISUB(*)
    real, intent(in)        ::SILLVT(*),RNF(*),WTHDRW(*)
    real, intent(in)        ::PRCPLK(*),EVAPLK(*)
    real, intent(in)        ::CPPT(*),CRNF(*),CAUG(*)
    integer, intent(in)    ::NLAKES

    if (NOT(ed_getExportData())) return
    call mfLibExp_LAKSP(NSOL,STAGES,SSMN,SSMX,CLAKE,ITMP,ITMP1,LWRT,LKARR,&
                        BDLKNC,NSLMS,IC,ISUB,SILLVT,PRCPLK,EVAPLK,RNF,WTHDRW,&
                        CPPT,CRNF,CAUG,NLAKES)
  end subroutine exp_LAKSP

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_LAKSP_dbl (NSOL,STAGES,SSMN,SSMX,CLAKE,ITMP,ITMP1,LWRT,LKARR,&
                            BDLKNC,NSLMS,IC,ISUB,SILLVT,PRCPLK,EVAPLK,RNF,WTHDRW,&
                            CPPT,CRNF,CAUG)
    implicit none
    integer, intent(in)     ::NSOL
    real, intent(in)        ::STAGES(*),SSMN(*),SSMX(*),CLAKE (*)
    integer, intent(in)     ::ITMP,ITMP1,LWRT,LKARR(*)
    real, intent(in)        ::BDLKNC(*)
    integer, intent(in)     ::NSLMS,IC(*),ISUB(*)
    real, intent(in)        ::SILLVT(*),RNF(*),WTHDRW(*)
    double precision, intent(in) ::PRCPLK(*),EVAPLK(*)
    real, intent(in)        ::CPPT(*),CRNF(*),CAUG(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_LAKSP_dbl(NSOL,STAGES,SSMN,SSMX,CLAKE,ITMP,ITMP1,LWRT,LKARR,&
                            BDLKNC,NSLMS,IC,ISUB,SILLVT,PRCPLK,EVAPLK,RNF,WTHDRW,&
                            CPPT,CRNF,CAUG)
  end subroutine exp_LAKSP_dbl

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_STR (ITMP,IRDFLG,IPTFLG,STRM,ISTRM,NSTREM,MXSTRM,ITRBAR,&
                        IDIVAR)
    implicit none
    integer, intent(in)      ::ITMP,IRDFLG,IPTFLG
    real, intent(in)         ::STRM (*)
    integer, intent(in)      ::ISTRM (*),NSTREM,MXSTRM,ITRBAR (*),IDIVAR (*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_STR(ITMP,IRDFLG,IPTFLG,STRM,ISTRM,NSTREM,MXSTRM,ITRBAR,&
                        IDIVAR)
  end subroutine exp_STR

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SFRLine2 (ISTRM,NISTRMD,STRM,NSTRMD)
    implicit none
    integer, intent(in)      ::ISTRM (*)
    integer, intent(in)      ::NISTRMD
    real, intent(in)         ::STRM (*)
    integer, intent(in)      ::NSTRMD
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SFRLine2(ISTRM,NISTRMD,STRM,NSTRMD)
  end subroutine exp_SFRLine2

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SFRLine6 (ISEG,IOTSG,IDIVAR,SEG,XSEC,QSTAGE)
    implicit none
    integer, intent(in)      ::ISEG (*),IOTSG (*),IDIVAR (*)
    real, intent(in)         ::SEG (*),XSEC (*),QSTAGE (*)
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_SFRLine6(ISEG,IOTSG,IDIVAR,SEG,XSEC,QSTAGE)
  end subroutine exp_SFRLine6

  !-----------------------------------------------------------------------------
  ! BRIEF:
  !-----------------------------------------------------------------------------
  subroutine exp_MnwSetup (MXWEL2,IWL2CB,IWELPT,KSPREF,PLOSS,IOWELL2,&
                           NOMOITER,FTAG,PREFIX,NAMES)
    implicit none
    integer, intent(in)  ::MXWEL2, IWL2CB, IWELPT, KSPREF, IOWELL2 (*), NOMOITER
    double precision, intent(in)  ::PLOSS
    character*6, intent(in) ::FTAG (*)
    character*256, intent(in) :: PREFIX, NAMES(3)
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_MnwSetup(MXWEL2,IWL2CB,IWELPT,KSPREF,PLOSS,IOWELL2,NOMOITER,FTAG,PREFIX,NAMES)
  end subroutine exp_MnwSetup

  !-----------------------------------------------------------------------------
  ! BRIEF:
  !-----------------------------------------------------------------------------
  subroutine exp_MnwStressPeriod (ITMP,NWELL2,WELL2,MNWSITE,MNWFLGS)
    implicit none
    integer, intent(in)           ::ITMP, NWELL2
    double precision, intent(in)  ::WELL2 (*)
    character*32, intent(in)      ::MNWSITE (*)
    double precision, intent(in)  ::MNWFLGS (*)
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_MnwStressPeriod(ITMP,NWELL2,WELL2,MNWSITE,MNWFLGS)
  end subroutine exp_MnwStressPeriod

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MultFunction (NAME,LINE)
    implicit none
    character*10, intent(in)   ::NAME
    character*200, intent(in)  ::LINE

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValStr('FNC', 'NAME     ', NAME, 10);
    call mfLibExp_SingleValStr('FNC', 'FUNC     ', LINE, 200);
    call mfLibExp_ExpPack('FNC')
  end subroutine exp_MultFunction

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ParListSNN (NPLIST,PARNAM,ISENS,LN,B,BL,BU,BSCAL)
    implicit none
    integer, intent(in)      :: NPLIST,ISENS(NPLIST),LN(NPLIST)
    character*10, intent(in) :: PARNAM(NPLIST)
    real, intent(in)         :: B(NPLIST),BL(NPLIST),BU(NPLIST),BSCAL(NPLIST)

    if (NOT(ed_getExportData())) return
    call mfLibExp_SENParList(NPLIST,PARNAM,ISENS,LN,B,BL,BU,BSCAL)
  end subroutine exp_ParListSNN

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ListPar (PNAME,PTYPE,PVAL)
    implicit none
    character*10, intent(in)   ::PNAME
    character*4, intent(in)    ::PTYPE
    real, intent(in)           ::PVAL

    if (NOT(ed_getExportData())) return
    call mfLibExp_ListPar(PNAME,PTYPE,PVAL)
  end subroutine exp_ListPar

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ListParData (PNAME,PTYPE,START,LSTDIM,NBC,NVALS,NAUX,BCDATA,AUXNMS)
    implicit none
    character*10, intent(in)   ::PNAME
    character*(*), intent(in)  ::PTYPE
    integer, intent(in)        ::LSTDIM,NBC,NVALS,NAUX,START
    real, intent(in)           ::BCDATA(NVALS,LSTDIM)
    character*16, intent(in)   ::AUXNMS(5)

    if (NOT(ed_getExportData())) return
    call mfLibExp_ListParData(PNAME,PTYPE,START,LSTDIM,NBC,NVALS,NAUX,BCDATA,AUXNMS)
  end subroutine exp_ListParData

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_StreamParInfo (PVAL,START,NUMBC)
    implicit none
    integer, intent(in)      :: START, NUMBC
    real, intent(in)         :: PVAL
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_StreamParInfo(PVAL,START,NUMBC)
  end subroutine exp_StreamParInfo

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ArrayPar (PNAME,PTYPE,PVAL,NP,IPLOC,IPCLST,MLT,ZON,INAME)
    implicit none
    character*10, intent(in)   ::PNAME
    character*4, intent(in)    ::PTYPE
    real, intent(in)           ::PVAL
    integer, intent(in)        ::NP,IPLOC(4,*),IPCLST(14,*)
    character*10, intent(in)   ::MLT(*),ZON(*),INAME(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_ArrayPar(PNAME,PTYPE,PVAL,NP,IPLOC,IPCLST,MLT,ZON,INAME)
  end subroutine exp_ArrayPar

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_ArrayParUsed (PNAME,PTYPE,INAME)
    implicit none
    character*10, intent(in)   ::PNAME,INAME
    character*4, intent(in)    ::PTYPE

    if (NOT(ed_getExportData())) return
    call mfLibExp_ArrayParUsed(PNAME,PTYPE,INAME)
  end subroutine exp_ArrayParUsed

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_Parameters ()
    implicit none
    integer :: VAL

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValInt('PAR', 'NAME     ', VAL);
    call mfLibExp_ExpPack('PAR')
  end subroutine exp_Parameters
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_LGR_Name (FNAME)
    implicit none
    character(*), intent(in) :: FNAME
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_LGR_Name(FNAME)
  end subroutine exp_LGR_Name
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_NameFile_FileNameLGR (MODTYPE,IGRID,FNAME)
    implicit none
    character(*), intent(in) :: FNAME
    integer, intent(in) :: MODTYPE, IGRID
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_NameFile_Filename(MODTYPE,IGRID,FNAME)
  end subroutine exp_NameFile_FileNameLGR
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_NameFile_FileName (MODTYPE,FNAME)
    implicit none
    character(*), intent(in) :: FNAME
    integer, intent(in) :: MODTYPE
    integer :: IGRID, idx1, idx2, dum, len
    real    :: r
    character(256)      :: outName
    IGRID = -1
    
    call mfLibExp_NameFile_Filename(MODTYPE,IGRID,FNAME)
    if (NOT(ed_getExportData())) return
    if (ed_getExportDB()) return
    ! open a file for writing the output to on unit 98765
    idx1 = 1
    idx2 = 0
    do while (idx2.eq.0)
      if (FNAME(idx1:idx1).eq.' ') idx2 = idx1
      idx1 = idx1 + 1
    end do
    outName = FNAME(1:idx2-1)//'.98765'
    OPEN(UNIT=98765,FILE=outName)
  end subroutine exp_NameFile_FileName
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_NameFileItem (FTYPE,FNAME,NIU)
    implicit none
    integer, intent(in) :: NIU
    character(*), intent(in) :: FTYPE, FNAME
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_NameFileItem(FTYPE,FNAME,NIU)
  end subroutine exp_NameFileItem
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_GAGE (IGGLST,NUMGAGE)
    implicit none
    integer, intent(in) :: IGGLST(4,*)
    integer, intent(in) :: NUMGAGE
    
    if (NOT(ed_getExportData())) return
    call mfLibExp_GAGE(IGGLST,NUMGAGE)
  end subroutine exp_GAGE
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_HUF1 (IHUFCB,HDRY,NHUF,NPHUF,IOHUFHEADS,IOHUFFLOWS,&
                       LTHUF,LAYWT)
    implicit none
    integer, intent(in) :: IHUFCB,NHUF,NPHUF,IOHUFHEADS,IOHUFFLOWS,&
                           LTHUF(*),LAYWT(*)
    real, intent(in)    :: HDRY

    if (NOT(ed_getExportData())) return
    call mfLibExp_HUF1(IHUFCB,HDRY,NHUF,NPHUF,IOHUFHEADS,IOHUFFLOWS,LTHUF,LAYWT)
  end subroutine exp_HUF1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_HufFlag (IHGUFLG)
    implicit none
    integer, intent(in) :: IHGUFLG(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_HufFlag(IHGUFLG)
  end subroutine exp_HufFlag

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_HUF_Wet (WETFCT,IWETIT,IHDWET)
    implicit none
    integer, intent(in) :: IWETIT,IHDWET
    real, intent(in)    :: WETFCT

    if (NOT(ed_getExportData())) return
    call mfLibExp_HufWet(WETFCT,IWETIT,IHDWET)
  end subroutine exp_HUF_Wet

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_HUF_Hani (HGUNAM,HGUHANI,HGUVANI)
    implicit none
    character*10, intent(in) :: HGUNAM(*)
    real, intent(in)         :: HGUHANI(*),HGUVANI(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_HufHani(HGUNAM,HGUHANI,HGUVANI)
  end subroutine exp_HUF_Hani

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_HUF_Par (PN,PTYP,PV,NP,IPLOC,IPCLST,MLTNAM,ZONNAM,HGUNAM)
    implicit none
    character*10, intent(in)   ::PN
    character*4, intent(in)    ::PTYP
    real, intent(in)           ::PV
    integer, intent(in)        ::NP,IPLOC(4,*),IPCLST(14,*)
    character*10, intent(in)   ::MLTNAM(*),ZONNAM(*),HGUNAM(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_HufPar(PN,PTYP,PV,NP,IPLOC,IPCLST,MLTNAM,ZONNAM,HGUNAM)
  end subroutine exp_HUF_Par

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_UZF_Line1 (NUZTOP,IUZFOPT,IRUNFLG,IETFLG,IUZFCB1,IUZFCB2,NTRAIL2,NSETS2,NUZGAG,SURFDEP)
    implicit none
    integer, intent(in) ::NUZTOP,IUZFOPT,IRUNFLG,IETFLG,IUZFCB1,IUZFCB2
    integer, intent(in) ::NTRAIL2,NSETS2,NUZGAG
    real, intent(in)    ::SURFDEP

    if (NOT(ed_getExportData())) return
    call mfLibExp_UZFLine1(NUZTOP,IUZFOPT,IRUNFLG,IETFLG,IUZFCB1,IUZFCB2,NTRAIL2,NSETS2,NUZGAG,SURFDEP)
  end subroutine exp_UZF_Line1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_UZF_Line8 (IUZLIST)
    implicit none
    integer, intent(in) ::IUZLIST(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_UZFLine8(IUZLIST)
  end subroutine exp_UZF_Line8

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_UZF_StressPeriod (NUZF1,NUZF2,NUZF3,NUZF4)
    implicit none
    integer, intent(in) ::NUZF1, NUZF2, NUZF3, NUZF4

    if (NOT(ed_getExportData())) return
    call mfLibExp_UZFStressPeriod(NUZF1,NUZF2,NUZF3,NUZF4)
  end subroutine exp_UZF_StressPeriod

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_VDF_Line5 (MT3DRHOFLG,MFNADVFD,NSWTCPL,IWTABLE,DENSEMIN,&
                            DENSEMAX,DNSCRIT,DENSEREF,DRHODC,DRHODPRHD,PRHDREF,&
                            NSRHOEOS,MTRHOSPEC,CRHOREF,FIRSTDT)
    implicit none
    integer, intent(in) ::MT3DRHOFLG, MFNADVFD, NSWTCPL, IWTABLE
    real, intent(in) ::DENSEMIN, DENSEMAX, DNSCRIT, DENSEREF, DRHODC(*)
    real, intent(in) ::DRHODPRHD, PRHDREF
    integer, intent(in) ::NSRHOEOS, MTRHOSPEC(*)
    real, intent(in) ::CRHOREF(*), FIRSTDT

    if (NOT(ed_getExportData())) return
    call mfLibExp_VDFLine5(MT3DRHOFLG,MFNADVFD,NSWTCPL,IWTABLE,DENSEMIN,&
                           DENSEMAX,DNSCRIT,DENSEREF,DRHODC,DRHODPRHD,PRHDREF,&
                           NSRHOEOS,MTRHOSPEC,CRHOREF,FIRSTDT)
  end subroutine exp_VDF_Line5

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_VDF_StressPeriod (INDENSE)
    implicit none
    integer, intent(in) ::INDENSE

    if (NOT(ed_getExportData())) return
    call mfLibExp_VDFStressPeriod(INDENSE)
  end subroutine exp_VDF_StressPeriod

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_VSC_Line3 (MT3DMUFLG,VISCMIN,VISCMAX,VISCREF,DMUDC,CMUREF,&
                            NSMUEOS,MUTEMPOPT,MTMUSPEC,MTMUTEMPSPEC,AMUCOEFF)
    implicit none
    integer, intent(in) ::MT3DMUFLG
    real, intent(in) ::VISCMIN, VISCMAX, VISCREF, DMUDC(*), CMUREF(*)
    integer, intent(in) ::NSMUEOS, MUTEMPOPT, MTMUSPEC(*), MTMUTEMPSPEC
    real, intent(in) ::AMUCOEFF(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_VSCLine3(MT3DMUFLG,VISCMIN,VISCMAX,VISCREF,DMUDC,CMUREF,&
                           NSMUEOS,MUTEMPOPT,MTMUSPEC,MTMUTEMPSPEC,AMUCOEFF)
  end subroutine exp_VSC_Line3

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_VSC_StressPeriod (INVISC)
    implicit none
    integer, intent(in) ::INVISC

    if (NOT(ed_getExportData())) return
    call mfLibExp_VSCStressPeriod(INVISC)
  end subroutine exp_VSC_StressPeriod

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_NamesAndUnits (CUNIT,IUNIT,NIUNIT)
    implicit none
    integer, intent(in)     :: NIUNIT, IUNIT(NIUNIT)
    character*4, intent(in) :: CUNIT(NIUNIT)
    integer                 :: i

    if (NOT(ed_getExportData())) return

    do i=1,NIUNIT
      call mfLibExp_IUNIT(IUNIT(i),CUNIT(i))
    end do
  end subroutine exp_NamesAndUnits
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_Comments (UNIT,LINE)
    implicit none
    integer, intent(in)      :: UNIT
    character(*), intent(in) :: LINE

    if (NOT(ed_getExportData())) return
    call MFLIBEXP_COMMENT(UNIT,LINE)
  end subroutine exp_Comments
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNW2_Ln1 (MNWMAX,IWL2CB,MNWPRNT,NAUX,MNWAUX)
    implicit none
    integer, intent(in)       :: MNWMAX,IWL2CB,MNWPRNT,NAUX
    character(16), intent(in) :: MNWAUX(5)

    if (NOT(ed_getExportData())) return
    call mfLibExp_MNW2LN1(MNWMAX,IWL2CB,MNWPRNT,NAUX,MNWAUX)
  end subroutine exp_MNW2_Ln1
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNW2_Ln2ab (WELLID,NNODES,LOSSTYPE,PUMPLOC,Qlimit,PPFLAG,&
                             PUMPCAP)
    implicit none
    integer, intent(in)       :: NNODES,PUMPLOC,PPFLAG,PUMPCAP,Qlimit
    character(20), intent(in) :: WELLID,LOSSTYPE

    if (NOT(ed_getExportData())) return
    call MFLIBEXP_MNW2LN2AB(WELLID,NNODES,LOSSTYPE,PUMPLOC,Qlimit,PPFLAG,PUMPCAP)
  end subroutine exp_MNW2_Ln2ab
  
  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNW2_Ln2c (Rw,Rskin,Kskin,B,C,P,CWC,LnDesc)
    implicit none
    DOUBLE PRECISION, intent(in) :: Rw,Rskin,Kskin,B,C,P,CWC
    character*200, intent(in)    :: LnDesc

    if (NOT(ed_getExportData())) return
    call MFLIBEXP_MNW2LN2C(Rw,Rskin,Kskin,B,C,P,CWC,LnDesc);
  end subroutine exp_MNW2_Ln2c

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNW2_Ln2d1 (IL,IR,IC,RwNode,RskinNode,KskinNode,&
                             BNode,CNode,PNode,CWCNode,PP,LnDesc)
    implicit none
    DOUBLE PRECISION, intent(in) :: RwNode,RskinNode,KskinNode,BNode,CNode,&
                                    PNode,CWCNode,PP
    integer, intent(in)          :: IL,IR,IC
    character*200, intent(in)    :: LnDesc

    if (NOT(ed_getExportData())) return
    call MFLIBEXP_MNW2LN2D1(IL,IR,IC,RwNode,RskinNode,KskinNode,&
                            BNode,CNode,PNode,CWCNode,PP,LnDesc);
  end subroutine exp_MNW2_Ln2d1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNW2_Ln2d2 (Ztop,ZBotm,IR,IC,RwNode,RskinNode,KskinNode,&
                             BNode,CNode,PNode,CWCNode,PP,LnDesc)
    implicit none
    DOUBLE PRECISION, intent(in) :: RwNode,RskinNode,KskinNode,BNode,CNode,&
                                    PNode,CWCNode,PP,Ztop,Zbotm
    integer, intent(in)          :: IR,IC
    character*200, intent(in)    :: LnDesc

    if (NOT(ed_getExportData())) return
    call MFLIBEXP_MNW2LN2D2(Ztop,ZBotm,IR,IC,RwNode,RskinNode,KskinNode,&
                            BNode,CNode,PNode,CWCNode,PP,LnDesc);
  end subroutine exp_MNW2_Ln2d2

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNW2_Ln2e (PUMPLAY,PUMPROW,PUMPCOL,Zpump,LnDesc)
    implicit none
    DOUBLE PRECISION, intent(in) :: Zpump
    integer, intent(in)          :: PUMPLAY,PUMPROW,PUMPCOL
    character*200, intent(in)    :: LnDesc

    if (NOT(ed_getExportData())) return
    call MFLIBEXP_MNW2LN2E(PUMPLAY,PUMPROW,PUMPCOL,Zpump,LnDesc)
  end subroutine exp_MNW2_Ln2e

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNW2_Ln2f (Hlim,QCUT,Qfrcmn,Qfrcmx,LnDesc)
    implicit none
    DOUBLE PRECISION, intent(in) :: Hlim,Qfrcmn,Qfrcmx
    integer, intent(in)          :: QCUT
    character*200, intent(in)    :: LnDesc

    if (NOT(ed_getExportData())) return
    call MFLIBEXP_MNW2LN2F(Hlim,QCUT,Qfrcmn,Qfrcmx,LnDesc)
  end subroutine exp_MNW2_Ln2f

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNW2_Ln2g (Hlift,LIFTq0,LIFTqdes,HWtol,LnDesc)
    implicit none
    DOUBLE PRECISION, intent(in) :: Hlift,LIFTq0,LIFTqdes,HWtol
    character*200, intent(in)    :: LnDesc

    if (NOT(ed_getExportData())) return
    call MFLIBEXP_MNW2LN2G(Hlift,LIFTq0,LIFTqdes,HWtol,LnDesc)
  end subroutine exp_MNW2_Ln2g

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNW2_Ln2h (Liftn,Qn,LnDesc)
    implicit none
    DOUBLE PRECISION, intent(in) :: Liftn,Qn
    character*200, intent(in)    :: LnDesc

    if (NOT(ed_getExportData())) return
    call MFLIBEXP_MNW2LN2H(Liftn,Qn,LnDesc)
  end subroutine exp_MNW2_Ln2h

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNW2_Ln34 (ITMP,MNW2,NMNWVL,MNWMAX,NAUX)
    implicit none
    DOUBLE PRECISION, intent(in)  :: MNW2
    integer, intent(in)           :: ITMP,NMNWVL,MNWMAX,NAUX
    dimension mnw2(NMNWVL,MNWMAX)

    if (NOT(ed_getExportData())) return
    call MFLIBEXP_MNW2LN34(ITMP,MNW2,NMNWVL,MNWMAX,NAUX)
  end subroutine exp_MNW2_Ln34

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNWI_Ln1 (Wel1flag,QSUMflag,BYNDflag)
    implicit none
    integer, intent(in)       :: Wel1flag,QSUMflag,BYNDflag

    if (NOT(ed_getExportData())) return
    call mfLibExp_MNWILN1(Wel1flag,QSUMflag,BYNDflag)
  end subroutine exp_MNWI_Ln1

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNWI_Ln2 (MNWOBS)
    implicit none
    integer, intent(in)       :: MNWOBS

    if (NOT(ed_getExportData())) return
    call mfLibExp_MNWILN2(MNWOBS)
  end subroutine exp_MNWI_Ln2

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNWI_Ln3 (WELLID,UNIT,QNDflag,QBHflag,CONCflag)
    implicit none
    character(20), intent(in) :: WELLID
    integer, intent(in)       :: UNIT,QNDflag,QBHflag,CONCflag

    if (NOT(ed_getExportData())) return
    call mfLibExp_MNWILN3(WELLID,UNIT,QNDflag,QBHflag,CONCflag)
  end subroutine exp_MNWI_Ln3

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_MNWI_End ()
    implicit none

    if (NOT(ed_getExportData())) return
    call mfLibExp_MNWIEND()
  end subroutine exp_MNWI_End

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SUB (ISUBCB,ISUBOC,NNDB,NDB,NMZ,NN,AC1,AC2,ITMIN,IDSAVE,IDREST,LN,LDN,DP)
    implicit none
    integer, intent(in) :: ISUBCB,ISUBOC,NNDB,NDB,NMZ,NN,ITMIN,IDSAVE,IDREST
    integer, intent(in) :: LN(*),LDN(*)
    real, intent (in)   :: AC1,AC2,DP(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValInt('SUB','ISUBCB    ',ISUBCB)
    call mfLibExp_SingleValInt('SUB','ISUBOC    ',ISUBOC)
    call mfLibExp_SingleValInt('SUB','NNDB      ',NNDB)
    call mfLibExp_SingleValInt('SUB','NDB       ',NDB)
    call mfLibExp_SingleValInt('SUB','NMZ       ',NMZ)
    call mfLibExp_SingleValInt('SUB','NN        ',NN)
    call mfLibExp_SingleValFlt('SUB','AC1       ',AC1)
    call mfLibExp_SingleValFlt('SUB','AC2       ',AC2)
    call mfLibExp_SingleValInt('SUB','ITMIN     ',ITMIN)
    call mfLibExp_SingleValInt('SUB','IDSAVE    ',IDSAVE)
    call mfLibExp_SingleValInt('SUB','IDREST    ',IDREST)
    call mfLibExp_ArrayValInt('SUB','LN        ',LN)
    call mfLibExp_ArrayValInt('SUB','LDN       ',LDN)
    call mfLibExp_ArrayValFlt('SUB','DP        ',DP)
    call mfLibExp_ExpPack('SUB')
  end subroutine exp_SUB

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SUB_Line15 (ISBOCF,ISBOCU)
    implicit none
    integer, intent(in) :: ISBOCF(*),ISBOCU(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_ArrayValInt('SU2','ISBOCF    ',ISBOCF)
    call mfLibExp_ArrayValInt('SU2','ISBOCU    ',ISBOCU)
    call mfLibExp_ExpPack('SU2')
  end subroutine exp_SUB_Line15

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_SUB_Line16 (ISP1,ISP2,JTS1,JTS2,IFL)
    implicit none
    integer, intent(in) :: ISP1,ISP2,JTS1,JTS2,IFL(*)

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValInt('SU3','ISP1      ',ISP1)
    call mfLibExp_SingleValInt('SU3','ISP2      ',ISP2)
    call mfLibExp_SingleValInt('SU3','JTS1      ',JTS1)
    call mfLibExp_SingleValInt('SU3','JTS2      ',JTS2)
    call mfLibExp_ArrayValInt('SU3','IFL       ',IFL)
    call mfLibExp_ExpPack('SU3')
  end subroutine exp_SUB_Line16

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_LGR_Parent (NGRIDS,IUPBHSV,IUPBFSV)
    implicit none
    integer, intent(in) :: NGRIDS,IUPBHSV,IUPBFSV

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValInt('LG1','NGRIDS    ',NGRIDS)
    call mfLibExp_SingleValInt('LG1','IUPBHSV   ',IUPBHSV)
    call mfLibExp_SingleValInt('LG1','IUPBFSV   ',IUPBFSV)
    call mfLibExp_ExpPack('LG1')
  end subroutine exp_LGR_Parent

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_LGR_Child (ISHFLG,IBFLG,IUCBHSV,IUCBFSV,MXLGRITER,&
                            IOUTLGR,RELAXH,RELAXF,HCLOSELGR,FCLOSELGR,&
                            NPLBEG,NPRBEG,NPCBEG,NPLEND,NPREND,NPCEND,&
                            NCPP,NCPPL)
    implicit none
    integer, intent(in) :: ISHFLG,IBFLG,IUCBHSV,IUCBFSV,MXLGRITER,IOUTLGR,&
                           NPLBEG,NPRBEG,NPCBEG,NPLEND,NPREND,NPCEND,&
                           NCPP,NCPPL(*)
    real, intent(in)    :: RELAXH,RELAXF,HCLOSELGR,FCLOSELGR

    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValInt('LG2','ISHFLG    ',ISHFLG)
    call mfLibExp_SingleValInt('LG2','IBFLG     ',IBFLG)
    call mfLibExp_SingleValInt('LG2','IUCBHSV   ',IUCBHSV)
    call mfLibExp_SingleValInt('LG2','IUCBFSV   ',IUCBFSV)
    call mfLibExp_SingleValInt('LG2','MXLGRITER ',MXLGRITER)
    call mfLibExp_SingleValInt('LG2','IOUTLGR   ',IOUTLGR)
    call mfLibExp_SingleValFlt('LG2','RELAXH    ',RELAXH)
    call mfLibExp_SingleValFlt('LG2','RELAXF    ',RELAXF)
    call mfLibExp_SingleValFlt('LG2','HCLOSELGR ',HCLOSELGR)
    call mfLibExp_SingleValFlt('LG2','FCLOSELGR ',FCLOSELGR)
    call mfLibExp_SingleValInt('LG2','NPLBEG    ',NPLBEG)
    call mfLibExp_SingleValInt('LG2','NPRBEG    ',NPRBEG)
    call mfLibExp_SingleValInt('LG2','NPCBEG    ',NPCBEG)
    call mfLibExp_SingleValInt('LG2','NPLEND    ',NPLEND)
    call mfLibExp_SingleValInt('LG2','NPREND    ',NPREND)
    call mfLibExp_SingleValInt('LG2','NPCEND    ',NPCEND)
    call mfLibExp_SingleValInt('LG2','NCPP      ',NCPP)
    call mfLibExp_ArrayValInt('LG2','NCPPL     ',NCPPL)
    call mfLibExp_ExpPack('LG2')
  end subroutine exp_LGR_Child

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_LGR ()
    implicit none
    integer :: flag
    if (NOT(ed_getExportData())) return
    call mfLibExp_SingleValInt('LGR','ISHFLG    ',flag)
    call mfLibExp_ExpPack('LGR')
  end subroutine exp_LGR

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine exp_Finished ()
    implicit none
    integer :: VAL
    VAL = 0
    call exp_Parameters()
    call mfLibExp_SingleValInt('STP', 'NAME     ', VAL);
    call mfLibExp_ExpPack('STP')
  end subroutine exp_Finished


end module module_exportData

!-----------------------------------------------------------------------------
! BRIEF:  
!-----------------------------------------------------------------------------
subroutine g_exp_GeoDB (a_arg, a_fileName)
  use module_exportData
  implicit none
  character(*), intent(in) :: a_arg, a_fileName
  
  call exp_GeoDB(a_arg, a_fileName)
end subroutine g_exp_GeoDB

!-----------------------------------------------------------------------------
! BRIEF:  
!-----------------------------------------------------------------------------
subroutine g_exp_GmsH5 (a_arg, a_fileName)
  use module_exportData
  implicit none
  character(*), intent(in) :: a_arg, a_fileName
  
  call exp_GmsH5(a_arg, a_fileName)
end subroutine g_exp_GmsH5

!-----------------------------------------------------------------------------
! BRIEF:  
!-----------------------------------------------------------------------------
subroutine g_exp_Tables (a_arg)
  use module_exportData
  implicit none
  character(*), intent(in) :: a_arg
  
  call exp_Tables(a_arg)
end subroutine g_exp_Tables

module mfLib
  implicit none

  INTERFACE
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE mfLib_CheckMemory()
    END SUBROUTINE mfLib_CheckMemory
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE mfLib_U2DREL(SUCCESS, IPRN, II, JJ, A, CNTRL,NAME)
      INTEGER SUCCESS [REFERENCE]
      INTEGER IPRN [REFERENCE]
      INTEGER II [REFERENCE]
      INTEGER JJ [REFERENCE]
      DIMENSION A (*)
      CHARACTER*200 CNTRL [REFERENCE]
      CHARACTER*24 NAME [REFERENCE]
    END SUBROUTINE mfLib_U2DREL
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE mfLib_U2DREL8(SUCCESS, IPRN, II, JJ, A, CNTRL,NAME)
      INTEGER SUCCESS [REFERENCE]
      INTEGER IPRN [REFERENCE]
      INTEGER II [REFERENCE]
      INTEGER JJ [REFERENCE]
      REAL*8 A (*)
      CHARACTER*200 CNTRL [REFERENCE]
      CHARACTER*24 NAME [REFERENCE]
    END SUBROUTINE mfLib_U2DREL8
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE mfLib_ULSTRD(SUCCESS,NLIST,LDIM,IAL,NAUX,CAUX,NCOL,NROW,RLIST,LINE)
      INTEGER SUCCESS [REFERENCE]
      INTEGER NLIST [REFERENCE]
      INTEGER LDIM [REFERENCE]
      INTEGER IAL [REFERENCE]
      INTEGER NAUX [REFERENCE]
      CHARACTER CAUX (*)
      INTEGER NCOL [REFERENCE]
      INTEGER NROW [REFERENCE]
      DIMENSION RLIST (*)
      CHARACTER*200 LINE
    END SUBROUTINE mfLib_ULSTRD
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE mfLib_U2DINT(SUCCESS,IPRN, II, JJ, IA, CNTRL)
      INTEGER SUCCESS [REFERENCE]
      INTEGER IPRN [REFERENCE]
      INTEGER II [REFERENCE]
      INTEGER JJ [REFERENCE]
      DIMENSION IA (*)
      CHARACTER*200 CNTRL [REFERENCE]
    END SUBROUTINE mfLib_U2DINT
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE mfLib_SQLITE_U2DINT(SUCCESS,IPRN, II, JJ, IA, CNTRL)
      INTEGER SUCCESS [REFERENCE]
      INTEGER IPRN [REFERENCE]
      INTEGER II [REFERENCE]
      INTEGER JJ [REFERENCE]
      DIMENSION IA (*)
      CHARACTER*200 CNTRL [REFERENCE]
    END SUBROUTINE mfLib_SQLITE_U2DINT
!     ------------------------------------------------------------------
!      Declare the C function
  SUBROUTINE mfLib_SFR2Reach(ii, NROW, NCOL, krch, irch, jrch, jseg, ireach, Strm, NStrmD, line)
      INTEGER ii [REFERENCE]
      INTEGER NROW [REFERENCE]
      INTEGER NCOL [REFERENCE]
      INTEGER krch [REFERENCE]
      INTEGER irch [REFERENCE]
      INTEGER jrch [REFERENCE]
      INTEGER jseg [REFERENCE]
      INTEGER ireach [REFERENCE]
      DIMENSION Strm (*)
      INTEGER NStrmD [VALUE]
      CHARACTER*200 line [REFERENCE]
    END SUBROUTINE mfLib_SFR2Reach
!     ------------------------------------------------------------------
!      Declare the C function
  SUBROUTINE mfLib_Sfr(Nlst, Kper, Iseg, Iotsg, Idivar, Seg, Xsec, Qstage, line)
      INTEGER Nlst [REFERENCE]
      INTEGER Kper [REFERENCE]
      DIMENSION Iseg (*)
      DIMENSION Iotsg (*)
      DIMENSION Idivar (*)
      DIMENSION Seg (*)
      DIMENSION Xsec (*)
      DIMENSION Qstage (*)
      CHARACTER*200 line [REFERENCE]
    END SUBROUTINE mfLib_Sfr
!     ------------------------------------------------------------------
!      Declare the C function
  SUBROUTINE mfLib_ReadSTR(SUCCESS,NSTREM,NSS,NTRIB,NCOL,NROW,STRM,ISTRM,ITRBAR,IDVIAR,LINE)
      INTEGER SUCCESS [REFERENCE]
      INTEGER NSTREM [REFERENCE]
      INTEGER NSS [REFERENCE]
      INTEGER NTRIB [REFERENCE]
      INTEGER NCOL [REFERENCE]
      INTEGER NROW [REFERENCE]
      DIMENSION STRM (*)
      DIMENSION ISTRM (*)
      DIMENSION ITRBAR (*)
      DIMENSION IDVIAR (*)
      CHARACTER*200 LINE
    END SUBROUTINE mfLib_ReadSTR
!     ------------------------------------------------------------------
!      Declare the C function
  SUBROUTINE mfLib_ReadMNW(SUCCESS,ITMP,WELL2,MNWSITE,MNWFLGS,LINE)
      INTEGER SUCCESS [REFERENCE]
      INTEGER ITMP [REFERENCE]
      DOUBLE PRECISION WELL2 [REFERENCE]
      CHARACTER*32 MNWSITE [REFERENCE]
      DOUBLE PRECISION MNWFLGS
      CHARACTER*256 LINE [REFERENCE]
      DIMENSION WELL2 (*)
      DIMENSION MNWSITE (*)
      DIMENSION MNWFLGS (*)
    END SUBROUTINE mfLib_ReadMNW
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE mfLib_SetParFName(FNAME)
      CHARACTER*200 FNAME [REFERENCE]
    END SUBROUTINE mfLib_SetParFName
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE mfLib_SetSenFName(FNAME)
      CHARACTER*200 FNAME [REFERENCE]
    END SUBROUTINE mfLib_SetSenFName
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE mfLib_FillInParType(NPVAL,PARNAM,PARTYP)
      INTEGER       NPVAL  [REFERENCE]
      CHARACTER     PARNAM (*)
      CHARACTER     PARTYP (*)
    END SUBROUTINE mfLib_FillInParType
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE MFLIB_READMNW2SP(FLAG,MNW2,MNWMAX,NMNWVL,NAUX,LnDesc)
      INTEGER           FLAG   [REFERENCE]
      DOUBLE PRECISION  MNW2 (*)
      INTEGER           MNWMAX [REFERENCE]
      INTEGER           NMNWVL [REFERENCE]
      INTEGER           NAUX   [REFERENCE]
      CHARACTER         LnDesc (*)
    END SUBROUTINE MFLIB_READMNW2SP
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE mfLib_SetSfrSegSize(SZ)
      INTEGER          SZ   [REFERENCE]
    END SUBROUTINE mfLib_SetSfrSegSize
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE MFLIB_SAMG_REL_LIC()
    END SUBROUTINE MFLIB_SAMG_REL_LIC
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE MFLIB_SAMGUSG(A,RHS,HNEW,IA,JA,NNA,NNU,KPER,KSTP,ncyc,NCYC_DONE,EPSSAMG,IBOUND,SAMGLOG,IERR,aqLicense)
      DOUBLE PRECISION  A         (*)
      DOUBLE PRECISION  RHS       (*)
      DOUBLE PRECISION  HNEW      (*)
      INTEGER           IA        [REFERENCE]
      INTEGER           JA        [REFERENCE]
      INTEGER           NNA       [REFERENCE]
      INTEGER           NNU       [REFERENCE]
      INTEGER           KPER      [REFERENCE]
      INTEGER           KSTP      [REFERENCE]
      INTEGER           ncyc      [REFERENCE]
      INTEGER           NCYC_DONE [REFERENCE]
      DOUBLE PRECISION  EPSSAMG   [REFERENCE]
      INTEGER           IBOUND    (*)
      INTEGER           SAMGLOG   [REFERENCE]
      INTEGER           IERR      [REFERENCE]
      INTEGER           aqLicense [REFERENCE]
    END SUBROUTINE MFLIB_SAMGUSG
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE MFLIB_LMG1ALSAMG(ISUM,ISUMI,LCA,LCIA,LCJA,LCU1,LCFRHS,LCIG,ISIZ1,ISIZ2,ISIZ3,ISIZ4,ICG,NCOL,NROW,NLAY,samg_logio,stor1,stor2,stor3,samg_logfile)
      INTEGER           ISUM [REFERENCE]
      INTEGER           ISUMI [REFERENCE]
      INTEGER           LCA [REFERENCE]
      INTEGER           LCIA [REFERENCE]
      INTEGER           LCJA [REFERENCE]
      INTEGER           LCU1 [REFERENCE]
      INTEGER           LCFRHS [REFERENCE]
      INTEGER           LCIG [REFERENCE]
      INTEGER           ISIZ1 [REFERENCE]
      INTEGER           ISIZ2 [REFERENCE]
      INTEGER           ISIZ3 [REFERENCE]
      INTEGER           ISIZ4 [REFERENCE]
      INTEGER           ICG [REFERENCE]
      INTEGER           NCOL [REFERENCE]
      INTEGER           NROW [REFERENCE]
      INTEGER           NLAY [REFERENCE]
      INTEGER           samg_logio [REFERENCE]
      REAL              stor1 [REFERENCE]
      REAL              stor2 [REFERENCE]
      REAL              stor3 [REFERENCE]
      character*300     samg_logfile [REFERENCE]
    END SUBROUTINE MFLIB_LMG1ALSAMG
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE MFLIB_LMG1RPSAMG(MXITER,MXCYC,rcloselmg,damplmg,damplmgt,ioutamg,ICG,IADAMPlmg,DUPlmg,DLOWlmg,HCLOSE,CONTROLlmg,samg_logio,SAMG_LOGFILE)
      INTEGER           MXITER [REFERENCE]
      INTEGER           MXCYC [REFERENCE]
      REAL              rcloselmg [REFERENCE]
      REAL              damplmg [REFERENCE]
      REAL              damplmgt [REFERENCE]
      INTEGER           ioutamg [REFERENCE]
      INTEGER           ICG [REFERENCE]
      INTEGER           IADAMPlmg [REFERENCE]
      REAL              DUPlmg [REFERENCE]
      REAL              DLOWlmg [REFERENCE]
      REAL              HCLOSE [REFERENCE]
      INTEGER           CONTROLlmg [REFERENCE]
      INTEGER           samg_logio [REFERENCE]
      character*300     SAMG_LOGFILE [REFERENCE]
    END SUBROUTINE MFLIB_LMG1RPSAMG
!     ------------------------------------------------------------------
!      Declare the C function
    SUBROUTINE MFLIB_LMG1APSAMG(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,A,IA,JA,U,FRHS,IG,ISIZ1,ISIZ2,ISIZ3,ISIZ4,KITER,BCLOSE,DAMP,ICNVG,KSTP,KPER,MXITER,MXCYC,NCOL,NROW,NLAY,NODES,HNOFLO,IOUTAMG,ICG,IADAMP,DUP,DLOW,samg_logio,IHCOFADD,start_res,end_res,iter_done,setup_done,iLicense,samg_logfile)
      DOUBLE PRECISION  HNEW (*)
      INTEGER           IBOUND (*)
      REAL              CR (*)
      REAL              CC (*)
      REAL              CV (*)
      REAL              HCOF (*)
      REAL              RHS (*)
      DOUBLE PRECISION  A (*)
      INTEGER           IA (*)
      INTEGER           JA (*)
      DOUBLE PRECISION  U (*)
      DOUBLE PRECISION  FRHS (*)
      INTEGER           IG (*)
      INTEGER           ISIZ1 [REFERENCE]
      INTEGER           ISIZ2 [REFERENCE]
      INTEGER           ISIZ3 [REFERENCE]
      INTEGER           ISIZ4 [REFERENCE]
      INTEGER           KITER [REFERENCE]
      REAL              BCLOSE [REFERENCE]
      REAL              DAMP [REFERENCE]
      INTEGER           ICNVG [REFERENCE]
      INTEGER           KSTP [REFERENCE]
      INTEGER           KPER [REFERENCE]
      INTEGER           MXITER [REFERENCE]
      INTEGER           MXCYC [REFERENCE]
      INTEGER           NCOL [REFERENCE]
      INTEGER           NROW [REFERENCE]
      INTEGER           NLAY [REFERENCE]
      INTEGER           NODES [REFERENCE]
      REAL              HNOFLO [REFERENCE]
      INTEGER           IOUTAMG [REFERENCE]
      INTEGER           ICG [REFERENCE]
      INTEGER           IADAMP [REFERENCE]
      REAL              DUP [REFERENCE]
      REAL              DLOW [REFERENCE]
      INTEGER           samg_logio [REFERENCE]
      INTEGER           IHCOFADD [REFERENCE]
      REAL              start_res [REFERENCE]
      REAL              end_res [REFERENCE]
      INTEGER           iter_done [REFERENCE]
      INTEGER           setup_done [REFERENCE]
      INTEGER           iLicense [REFERENCE]
      character*300     samg_logfile [REFERENCE]
    END SUBROUTINE MFLIB_LMG1APSAMG

!     leave this below all the ! declarations
  END INTERFACE

      private
      save

      public :: mfLibF_DebugCheckMemory &
               ,mfLibF90_U2DREL &
               ,mfLibF90_U2DREL8 &
               ,mfLibF_U2DINT &
               ,mfLibF_SQLITE_U2DINT &
               ,mfLibF_ULSTRD &
               ,mfLibF_SfrReach &
               ,mfLibF_Sfr &
               ,mfLibF90_STR &
               ,mfLibF_SetParFName &
               ,mfLibF_SetSenFName &
               ,mfLibF_MNW &
               ,mfLibF_FillInParType &
               ,mfLibF_MNW2 &
               ,mfLibF_MNW2WELLID &
               ,mfLibF_SetSfrSegSize &
               ,mfLibF_SAMG_REL_LIC &
               ,mfLibF_SAMG_USG &
               ,mfLibF_LMG1ALsamg &
               ,mfLibF_LMG1RPsamg &
               ,mfLibF_LMG1APsamg

      contains
!-----------------------------------------------------------------------
      subroutine mfLibF_MNW2WELLID (iread,MNW2,MNWMAX,NMNWVL,MNWID)
        implicit none
        DOUBLE PRECISION, intent(in) :: MNW2
        integer, intent(in)          :: iread, NMNWVL, MNWMAX
        integer, intent(out)         :: MNWID

        integer                      :: i, id
        dimension mnw2(NMNWVL,MNWMAX)

        id = MNWID+1
        if (iread.eq.1) id = 1
        MNWID = 0
        i = id
        do while (i<=MNWMAX)
          if (NOT(MNW2(1,i).eq.0)) then
            MNWID = i
            i = MNWMAX+1
          end if
          i = i + 1
        end do
      end subroutine mfLibF_MNW2WELLID

!-----------------------------------------------------------------------
      subroutine mfLibF_MNW2 (FLAG,MNW2,MNWMAX,NMNWVL,NAUX,LnDesc)
        implicit none
        DOUBLE PRECISION, intent(out) :: MNW2
        integer, intent(in)           :: NMNWVL, MNWMAX, NAUX
        integer, intent(out)          :: FLAG
        character(200), intent(in)    :: LnDesc
        dimension mnw2(NMNWVL,MNWMAX)

        call MFLIB_READMNW2SP(FLAG,MNW2,MNWMAX,NMNWVL,NAUX,LnDesc)
      end subroutine mfLibF_MNW2

!-----------------------------------------------------------------------
      subroutine mfLibF_DebugCheckMemory () ! for debugging
        !call mfLib_CheckMemory()
      end subroutine mfLibF_DebugCheckMemory

!-----------------------------------------------------------------------
      subroutine mfLibF90_U2DREL(a_success,a_IPRN,a_II,a_JJ,a_K,a_IOUT,a_A,a_ANAME,a_CNTRL)

        implicit none
        integer, intent(out)       :: a_success, a_IPRN
        integer, intent(in)        :: a_II, a_JJ, a_K, a_IOUT
        real, intent(out)          :: a_A(a_JJ, a_II)
        character*24, intent(in)   :: a_ANAME
        character(*), intent(in)   :: a_CNTRL

        integer        :: i, j

        a_success = 0
        a_IPRN = 0

        do j=1, a_JJ
          do i=1, a_II
            a_A(j,i) = 0
          end do
        end do

        call mfLib_U2DREL(a_success, a_IPRN, a_II, a_JJ, a_A, a_CNTRL,a_ANAME)
        call mfLibF_DebugCheckMemory
      end subroutine mfLibF90_U2DREL

!-----------------------------------------------------------------------
      subroutine mfLibF90_U2DREL8(a_success,a_IPRN,a_II,a_JJ,a_K,a_IOUT,a_A,a_ANAME,a_CNTRL)

        implicit none
        integer, intent(out)          :: a_success, a_IPRN
        integer, intent(in)           :: a_II, a_JJ, a_K, a_IOUT
        double precision, intent(out) :: a_A(a_JJ, a_II)
        character*24, intent(in)      :: a_ANAME
        character(*), intent(in)      :: a_CNTRL

        integer        :: i, j

        a_success = 0
        a_IPRN = 0

        do j=1, a_JJ
          do i=1, a_II
            a_A(j,i) = 0
          end do
        end do

        call mfLib_U2DREL8(a_success, a_IPRN, a_II, a_JJ, a_A, a_CNTRL,a_ANAME)
        call mfLibF_DebugCheckMemory
      end subroutine mfLibF90_U2DREL8
!-----------------------------------------------------------------------
      subroutine mfLibF_U2DINT(a_success,a_IPRN,a_II,a_JJ,a_A,a_CNTRL)

        implicit none
        integer, intent(out)       :: a_success, a_IPRN
        integer, intent(in)        :: a_II, a_JJ
        integer, intent(out)       :: a_A(a_JJ, a_II)
        character*200, intent(in)  :: a_CNTRL

        integer  :: i, j

        a_success = 0
        a_IPRN = 0

        do j=1, a_JJ
          do i=1, a_II
            a_A(j,i) = 0
          end do
        end do

        call mfLib_U2DINT(a_success, a_IPRN, a_II, a_JJ, a_A, a_CNTRL)
        call mfLibF_DebugCheckMemory
      end subroutine mfLibF_U2DINT
!-----------------------------------------------------------------------
      subroutine mfLibF_SQLITE_U2DINT(a_success,a_IPRN,a_II,a_JJ,a_A,a_CNTRL)

        implicit none
        integer, intent(out)       :: a_success, a_IPRN
        integer, intent(in)        :: a_II, a_JJ
        integer, intent(out)       :: a_A(a_JJ, a_II)
        character*200, intent(in)  :: a_CNTRL

        integer  :: i, j

        a_success = 0
        a_IPRN = 0

        do j=1, a_JJ
          do i=1, a_II
            a_A(j,i) = 0
          end do
        end do

        call mfLib_SQLITE_U2DINT(a_success, a_IPRN, a_II, a_JJ, a_A, a_CNTRL)
        call mfLibF_DebugCheckMemory
      end subroutine mfLibF_SQLITE_U2DINT
!-----------------------------------------------------------------------
      subroutine mfLibF_ULSTRD(a_success,a_NLIST,a_LDIM,a_IAL,a_NAUX,a_CAUX,a_NCOL,a_NROW,a_MXLIST,a_RLIST,a_LINE)

        implicit none
        integer, intent(out)         ::a_success
        integer, intent(in)          ::a_NLIST,a_LDIM,a_IAL,a_NAUX,a_NCOL,a_NROW,a_MXLIST
        real, intent(out)            ::a_RLIST(a_LDIM,a_MXLIST)
        character*200, intent(in)    ::a_LINE
        character*16, intent(in)     ::a_CAUX(5)

        a_success = 0
        call mfLib_ULSTRD(a_success,a_NLIST,a_LDIM,a_IAL,a_NAUX,a_CAUX,a_NCOL,a_NROW,a_RLIST,a_LINE)

        call mfLibF_DebugCheckMemory
      end subroutine mfLibF_ULSTRD
!-----------------------------------------------------------------------
      subroutine mfLibF_SfrReach (ii, NROW, NCOL, Nstrm, krch, irch, jrch, jseg, ireach, Strm, NStrmD, line)
       implicit none
       integer, intent(in)       :: ii, NROW, NCOL, Nstrm, NStrmD
       integer, intent(out)      :: krch, irch, jrch, jseg, ireach
       real, intent(out)         :: Strm(24, Nstrm)
       character*200, intent(in) :: line

       call mfLib_SFR2Reach(ii, NROW, NCOL, krch, irch, jrch,jseg, ireach, Strm, NStrmD, line)

        call mfLibF_DebugCheckMemory
      end subroutine mfLibF_SfrReach
!-----------------------------------------------------------------------
      subroutine mfLibF_Sfr (Nlst, Kper, Iseg, Iotsg, Idivar, Seg, Xsec, Qstage, line)
        implicit none
        integer, intent(in)       :: Nlst, Kper
        integer, intent(out)      :: Iseg(*), Iotsg(*), Idivar(*)
        Real, intent(out)         :: Seg(*), Xsec(*), Qstage(*)
        character*200, intent(in) :: line

        call mfLib_Sfr(Nlst, Kper, Iseg, Iotsg, Idivar, Seg, Xsec, Qstage, line)
        call mfLibF_DebugCheckMemory
      end subroutine mfLibF_Sfr
!-----------------------------------------------------------------------
      SUBROUTINE mfLibF90_STR(STRM,ISTRM,NSTREM,MXSTRM,IN,IOUT,ITRBAR,NSS,NTRIB,IDIVAR,NCOL,NROW,NLAY)

      implicit none
      integer, intent(in)   ::NSTREM,MXSTRM,IN,IOUT,NSS,NTRIB,NCOL,NROW,NLAY
      integer, intent(out)  ::ISTRM(5,MXSTRM),ITRBAR(NSS,NTRIB),IDIVAR(NSS)
      real, intent(out)     ::STRM(11,MXSTRM)
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*200 LINE
      INTEGER  :: SUCCESS
      INTEGER  :: II,IK,JK,I,J,K
!     ------------------------------------------------------------------
      READ(IN,'(A)') LINE
!     ------------------------------------------------------------------
!     Read the stream data
!     ------------------------------------------------------------------
      CALL mfLib_ReadSTR(SUCCESS,NSTREM,NSS,NTRIB,NCOL,NROW,STRM,ISTRM,ITRBAR,IDIVAR,LINE)
      IF(SUCCESS.EQ.0) CALL USTOP('ERROR READING STREAM DATA.')
        call mfLibF_DebugCheckMemory
      END SUBROUTINE mfLibF90_STR
!-----------------------------------------------------------------------
      SUBROUTINE mfLibF_MNW(ITMP,WELL2,MNWSITE,MNWFLGS,LINE)

      IMPLICIT NONE
      INTEGER, INTENT(IN)   ::ITMP
      DOUBLE PRECISION, INTENT(OUT) ::WELL2(*), MNWFLGS(*)
      CHARACTER*32, INTENT(OUT) ::MNWSITE(*)
      CHARACTER*256 LINE
      INTEGER SUCCESS
      CALL mfLib_ReadMNW(SUCCESS,ITMP,WELL2,MNWSITE,MNWFLGS,LINE)
      IF(SUCCESS.EQ.0) CALL USTOP('ERROR READING MNW DATA.')
      call mfLibF_DebugCheckMemory
      END SUBROUTINE mfLibF_MNW
!-----------------------------------------------------------------------
      subroutine mfLibF_FillInParType (PARNAM,PARTYP,NPVAL)
        implicit none
        INTEGER, intent (in)            :: NPVAL
        CHARACTER(LEN=10),intent (in)   :: PARNAM(*)
        CHARACTER(LEN=4),intent (out)   :: PARTYP(*)

        call mfLib_FillInParType(NPVAL,PARNAM,PARTYP)
      end subroutine mfLibF_FillInParType
!-----------------------------------------------------------------------
      subroutine mfLibF_SetParFName (FNAME)
        implicit none
        CHARACTER(LEN=200), intent(in) :: FNAME
        
        call mfLib_SetParFName(FNAME)
      end subroutine mfLibF_SetParFName
!-----------------------------------------------------------------------
      subroutine mfLibF_SetSenFName (FNAME)
        implicit none
        CHARACTER(LEN=200), intent(in) :: FNAME
        
        call mfLib_SetSenFName(FNAME)
      end subroutine mfLibF_SetSenFName
!-----------------------------------------------------------------------
      subroutine mfLibF_SetSfrSegSize (SZ)
        implicit none
        INTEGER, intent (in)            :: SZ

        call mfLib_SetSfrSegSize(SZ)
      end subroutine mfLibF_SetSfrSegSize
!-----------------------------------------------------------------------
      subroutine mfLibF_SAMG_REL_LIC ()
        implicit none

        call MFLIB_SAMG_REL_LIC()
      end subroutine mfLibF_SAMG_REL_LIC
!-----------------------------------------------------------------------
      subroutine mfLibF_SAMG_USG (A,RHS,HNEW,IA,JA,NNA,NNU,KPER,KSTP,ncyc,&
                                  NCYC_DONE,EPSSAMG,IBOUND,SAMGLOG,IERR,&
                                  aqLicense)
        implicit none
        DOUBLE PRECISION, intent(inout) :: A(*),RHS(*),HNEW(*),EPSSAMG
        INTEGER, intent(in)             :: IA,JA,NNA,NNU,KPER,KSTP,ncyc
        INTEGER, intent(in)             :: NCYC_DONE,IBOUND(*),SAMGLOG,IERR
        INTEGER, intent(in)             :: aqLicense

        call MFLIB_SAMGUSG(A,RHS,HNEW,IA,JA,NNA,NNU,KPER,KSTP,ncyc,NCYC_DONE,&
                           EPSSAMG,IBOUND,SAMGLOG,IERR,aqLicense)
      end subroutine mfLibF_SAMG_USG
!-----------------------------------------------------------------------
      subroutine mfLibF_LMG1ALsamg (ISUM,ISUMI,LCA,LCIA,LCJA,LCU1,LCFRHS,LCIG,&
                                    ISIZ1,ISIZ2,ISIZ3,ISIZ4,ICG,NCOL,NROW,NLAY,&
                                    samg_logio,stor1,stor2,stor3,samg_logfile)
        implicit none
        INTEGER :: ISUM,ISUMI,LCA,LCIA,LCJA,LCU1,LCFRHS,LCIG,ISIZ1,ISIZ2,ISIZ3
        INTEGER :: ISIZ4,ICG,NCOL,NROW,NLAY,samg_logio
        REAL    :: stor1,stor2,stor3
        CHARACTER*300 :: samg_logfile

        call MFLIB_LMG1ALSAMG(ISUM,ISUMI,LCA,LCIA,LCJA,LCU1,LCFRHS,LCIG,ISIZ1,&
                              ISIZ2,ISIZ3,ISIZ4,ICG,NCOL,NROW,NLAY,samg_logio,&
                              stor1,stor2,stor3,samg_logfile)
      end subroutine mfLibF_LMG1ALsamg
!-----------------------------------------------------------------------
      subroutine mfLibF_LMG1RPsamg (MXITER,MXCYC,rcloselmg,damplmg,damplmgt,ioutamg,ICG,IADAMPlmg,DUPlmg,DLOWlmg,HCLOSE,CONTROLlmg,samg_logio,SAMG_LOGFILE)
        implicit none
        INTEGER :: MXITER,MXCYC,ioutamg,ICG,IADAMPlmg,CONTROLlmg,samg_logio
        REAL    :: rcloselmg,damplmg,damplmgt,DUPlmg,DLOWlmg,HCLOSE
        CHARACTER*300 :: SAMG_LOGFILE

        call MFLIB_LMG1RPSAMG(MXITER,MXCYC,rcloselmg,damplmg,damplmgt,ioutamg,ICG,IADAMPlmg,DUPlmg,DLOWlmg,HCLOSE,CONTROLlmg,samg_logio,SAMG_LOGFILE)
      end subroutine mfLibF_LMG1RPsamg
      
!-----------------------------------------------------------------------
      subroutine mfLibF_LMG1APsamg (&
        HNEW,IBOUND,CR,CC,CV,HCOF,RHS,A,IA,JA,U,FRHS,IG,ISIZ1,ISIZ2,ISIZ3,&
        ISIZ4,KITER,BCLOSE,DAMP,ICNVG,KSTP,KPER,MXITER,MXCYC,NCOL,NROW,NLAY,&
        NODES,HNOFLO,IOUTAMG,ICG,IADAMP,DUP,DLOW,samg_logio,IHCOFADD,&
        start_res,end_res,iter_done,setup_done,iLicense,samg_logfile)

        implicit none
        DOUBLE PRECISION :: HNEW(*),A(*),U(*),FRHS(*)
        INTEGER          :: IBOUND(*),IA(*),JA(*),IG(*),ISIZ1,ISIZ2,ISIZ3,ISIZ4,&
                            KITER,ICNVG,KSTP,KPER,MXITER,MXCYC,NCOL,NROW,NLAY,&
                            NODES,IOUTAMG,ICG,IADAMP,samg_logio,IHCOFADD,&
                            iter_done,setup_done,iLicense
        REAL             :: CR(*),CC(*),CV(*),HCOF(*),RHS(*),BCLOSE,DAMP,&
                            HNOFLO,DUP,DLOW,start_res,end_res
        CHARACTER*300    :: samg_logfile
        
        call MFLIB_LMG1APSAMG(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,A,IA,JA,U,FRHS,IG,&
                              ISIZ1,ISIZ2,ISIZ3,ISIZ4,KITER,BCLOSE,DAMP,ICNVG,&
                              KSTP,KPER,MXITER,MXCYC,NCOL,NROW,NLAY,NODES,&
                              HNOFLO,IOUTAMG,ICG,IADAMP,DUP,DLOW,samg_logio,&
                              IHCOFADD,start_res,end_res,iter_done,setup_done,&
                              iLicense,samg_logfile)
      end subroutine mfLibF_LMG1APsamg


end module mfLib

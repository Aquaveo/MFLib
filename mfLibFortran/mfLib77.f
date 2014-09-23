      module mfLib77
      
      implicit none

      private
      save

      public :: mfLibF77_U2DREL,
     +          mfLibF77_STR,
     +          mfLibF77_STRU

      contains

!-----------------------------------------------------------------------
      subroutine mfLibF77_U2DREL(a_success,
     +                           a_IPRN,
     +                           a_II,
     +                           a_JJ,
     +                           a_K,
     +                           a_IOUT,
     +                           a_A,
     +                           a_ANAME,
     +                           a_CNTRL)

        use mfLib

        implicit none
        integer, intent(out)       :: a_success, a_IPRN
        integer, intent(in)        :: a_II, a_JJ, a_K, a_IOUT
        real, intent(out)          :: a_A(a_JJ, a_II)
        character*24, intent(in)   :: a_ANAME
        character(*), intent(in)   :: a_CNTRL

        call mfLibF90_U2DREL(a_success, a_IPRN, a_II, a_JJ, a_K, a_IOUT,
     +                       a_A, a_ANAME, a_CNTRL)

        if(a_success.EQ.0) CALL USTOP('ERROR READING ARRAY.')
        if(a_IPRN.GE.0) then
          if(a_K.GT.0) then
            write(a_IOUT,94) a_ANAME,a_K
   94       format(1X,///11X,A,' FOR LAYER',I4,/
     1       1X,'READING DATA FROM HDF5 FILE')
          else if(a_K.EQ.0) then
            write(a_IOUT,95) a_ANAME
   95       format(1X,///11X,A,/
     1       1X,'READING DATA FROM HDF5 FILE')
          else
            write(a_IOUT,96) a_ANAME
   96       format(1X,///11X,A,' FOR CROSS SECTION',/
     1       1X,'READING DATA FROM HDF5 FILE')
          end if
          call ULAPRW(a_A,a_ANAME,0,0,a_JJ,a_II,0,a_IPRN,a_IOUT)
        endif
      end subroutine mfLibF77_U2DREL
!-----------------------------------------------------------------------
      SUBROUTINE mfLibF77_STR(STRM,ISTRM,NSTREM,MXSTRM,IN,IOUT,ITRBAR,
     +       NDIV,NSS,NTRIB,IDIVAR,ICALC,IPTFLG,NCOL,NROW,NLAY,NPSTR,
     +       ISTRPB,IRDFLG)

      use mfLib
      implicit none
      integer, intent(in)   ::NSTREM,MXSTRM,IN,IOUT,NDIV,NSS,NTRIB,
     +                        ICALC,IPTFLG,NCOL,NROW,NLAY,NPSTR,
     +                        ISTRPB,IRDFLG
      integer, intent(out)  ::ISTRM(5,MXSTRM),ITRBAR(NSS,NTRIB),
     +                        IDIVAR(NSS)
      real, intent(out)     ::STRM(11,MXSTRM)
      CHARACTER*16          :: STRAUX(20)
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER  :: II,IK,JK,I,J,K
C     ------------------------------------------------------------------
C     Read the stream data
C     ------------------------------------------------------------------
      call mfLibF90_Str(STRM,ISTRM,NSTREM,MXSTRM,IN,IOUT,ITRBAR,
     +       NSS,NTRIB,IDIVAR,NCOL,NROW,NLAY,0,STRAUX)

C     ------------------------------------------------------------------
C     put zeros in these arrays if they are not being used
C     ------------------------------------------------------------------
      IF(NTRIB.LE.0) THEN
        DO IK=1,NSS
          DO JK=1,NTRIB
            ITRBAR(IK,JK)=0
          END DO
        END DO
      ENDIF
      IF(NDIV.LE.0) THEN
        DO IK=1,NSS
          IDIVAR(IK)=0
        END DO
      ENDIF

C     ------------------------------------------------------------------
C     Check for errors
C     ------------------------------------------------------------------
      IF(IRDFLG.EQ.0) WRITE(IOUT,4)
    4 FORMAT(/,4X,'LAYER   ROW    COL    SEGMENT   REACH   STREAMFLOW',
     16X,'STREAM    STREAMBED     STREAMBED BOT  STREAMBED TOP',/27X,
     2'NUMBER   NUMBER                   STAGE   CONDUCTANCE',6X,
     3'ELEVATION      ELEVATION',/3X,110('-'))
      DO II=1,NSTREM
c      READ(IN,5)K,I,J,ISTRM(4,II),ISTRM(5,II),STRM(1,II),STRM(2,II),
c     1STRM(3,II),STRM(4,II),STRM(5,II)
      K=ISTRM(1,II)
      I=ISTRM(2,II)
      J=ISTRM(3,II)
    5 FORMAT(5I5,F15.0,4F10.0)
      IF(IRDFLG.EQ.0) WRITE(IOUT,6)K,I,J,ISTRM(4,II),
     1ISTRM(5,II),STRM(1,II),STRM(2,II),STRM(3,II),STRM(4,II),STRM(5,II)
    6 FORMAT(1X,1X,I6,2I7,2I9,7X,G11.4,G12.4,G11.4,4X,2G13.4)

C
C  Check for illegal grid location
      IF(K.LT.1 .OR. K.GT.NLAY) THEN
         WRITE(IOUT,*) ' Layer number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(I.LT.1 .OR. I.GT.NROW) THEN
         WRITE(IOUT,*) ' Row number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(J.LT.1 .OR. J.GT.NCOL) THEN
         WRITE(IOUT,*) ' Column number in list is outside of the grid'
         CALL USTOP(' ')
      END IF

      END DO

C     ------------------------------------------------------------------
C     Print the data to the OUT file
C     ------------------------------------------------------------------

C
C3------PRINT NUMBER OF REACHES IN CURRENT STRESS PERIOD.
      WRITE (IOUT,101) NSTREM
  101 FORMAT(1X,/1X,I6,' STREAM REACHES')
C6----READ AND PRINT DATA IF STREAM STAGE IS CALCULATED.
      IF(ICALC.GT.0) THEN
      IF(IRDFLG.EQ.0) WRITE(IOUT,7)
    7 FORMAT(/,4X,'LAYER',3X,'ROW',4X,'COL   ',' SEGMENT',3X,
     1'REACH',8X,'STREAM',13X,'STREAM',10X,'ROUGH',/27X,'NUMBER',3X,
     2 'NUMBER',8X,'WIDTH',14X,'SLOPE',10X,'COEF.',/3X,110('-'))
      DO 280 II=1,NSTREM
c      READ(IN,8) STRM(6,II),STRM(7,II),STRM(8,II)
    8 FORMAT(3F10.0)
      IF(IRDFLG.EQ.0) WRITE(IOUT,9)ISTRM(1,II),
     &    ISTRM(2,II),ISTRM(3,II),ISTRM(4,II),ISTRM(5,II),
     1    STRM(6,II),STRM(7,II),STRM(8,II)
    9 FORMAT(2X,I6,2I7,2I9,7X,G12.4,4X,G13.4,4X,G12.4)
  280 CONTINUE
      ENDIF

C9-----READ AND PRINT TRIBUTARY SEGMENTS.
      IF(NTRIB.GT.0) THEN
      IF(IRDFLG.EQ.0) WRITE(IOUT,10)NTRIB
   10 FORMAT(/,30X,'MAXIMUM NUMBER OF TRIBUTARY STREAMS IS ',I6,//1X,
     1 20X,'STREAM SEGMENT',15X,'TRIBUTARY STREAM SEGMENT NUMBERS')
      DO 340 IK=1,NSS
c      READ(IN,11) (ITRBAR(IK,JK),JK=1,NTRIB)
   11 FORMAT(10I5)
      IF(IRDFLG.EQ.0) WRITE(IOUT,12) IK,(ITRBAR(IK,JK),JK=1,NTRIB)
   12 FORMAT(19X,I6,20X,10I5)
  340 CONTINUE
      ENDIF
C
C10----READ AND PRINT DIVERSION SEGMENTS NUMBERS.
  343 IF(NDIV.GT.0) THEN
      IF(IRDFLG.EQ.0) WRITE(IOUT,13)
   13 FORMAT(/,10X,'DIVERSION SEGMENT NUMBER',10X,
     1       'UPSTREAM SEGMENT NUMBER')
      DO 345 IK=1,NSS
c      READ(IN,14) IDIVAR(IK)
   14 FORMAT(I10)
      IF(IRDFLG.EQ.0) WRITE(IOUT,15) IK,IDIVAR(IK)
   15 FORMAT(19X,I6,27X,I6)
  345 CONTINUE
      ENDIF
C
C11----SET FLOW OUT OF REACH, FLOW INTO REACH, AND FLOW THROUGH
C      STREAM BED TO ZERO.
  350 DO 360 II =1,NSTREM
      STRM(9,II)=0.0
      STRM(10,II)=0.0
      STRM(11,II)=0.0
  360 CONTINUE

      END SUBROUTINE mfLibF77_STR
!-----------------------------------------------------------------------
      SUBROUTINE mfLibF77_STRU(STRM,ISTRM,NSTREM,MXSTRM,IN,IOUT,ITRBAR,
     +       NDIV,NSS,NTRIB,IDIVAR,ICALC,IPTFLG,NCOL,NROW,NLAY,NPSTR,
     +       ISTRPB,IRDFLG,NAUX,STRAUX)

      use mfLib
      implicit none
      integer, intent(in)   ::NSTREM,MXSTRM,IN,IOUT,NDIV,NSS,NTRIB,
     +                        ICALC,IPTFLG,NCOL,NROW,NLAY,NPSTR,
     +                        ISTRPB,IRDFLG,NAUX
      integer, intent(out)  ::ISTRM(5,MXSTRM),ITRBAR(NSS,NTRIB),
     +                        IDIVAR(NSS)
      real, intent(out)     ::STRM(11+NAUX,MXSTRM)
      character, intent(in) ::STRAUX(*)
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     Read the stream data
C     ------------------------------------------------------------------
      call mfLibF90_Str(STRM,ISTRM,NSTREM,MXSTRM,IN,IOUT,ITRBAR,
     +       NSS,NTRIB,IDIVAR,NCOL,NROW,NLAY,NAUX,STRAUX)

      END SUBROUTINE mfLibF77_STRU

      end module mfLib77

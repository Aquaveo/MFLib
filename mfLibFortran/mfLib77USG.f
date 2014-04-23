      module mfLib77USG
      
      implicit none

      private
      save

      public :: mfLibF77_U2DREL8

      contains

!-----------------------------------------------------------------------
      subroutine mfLibF77_U2DREL8(a_success,
     +                            a_IPRN,
     +                            a_II,
     +                            a_JJ,
     +                            a_K,
     +                            a_IOUT,
     +                            a_A,
     +                            a_ANAME,
     +                            a_CNTRL)

        use mfLib

        implicit none
        integer, intent(out)          :: a_success, a_IPRN
        integer, intent(in)           :: a_II, a_JJ, a_K, a_IOUT
        double precision, intent(out) :: a_A(a_JJ, a_II)
        character*24, intent(in)      :: a_ANAME
        character(*), intent(in)      :: a_CNTRL

        call mfLibF90_U2DREL8(a_success, a_IPRN, a_II, a_JJ, a_K,
     +                        a_IOUT, a_A, a_ANAME, a_CNTRL)

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
          call ULAPRW8(a_A,a_ANAME,0,0,a_JJ,a_II,0,a_IPRN,a_IOUT)
        endif
      end subroutine mfLibF77_U2DREL8

      end module mfLib77USG
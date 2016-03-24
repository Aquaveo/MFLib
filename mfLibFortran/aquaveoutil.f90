!-------------------------------------------------------------------------------
! BRIEF: Contains helpful aquaveo code.
!-------------------------------------------------------------------------------

!///////////////////////////////////////////////////////////////////////////////
!
! MODULE: module_aquaveo_data
!
!///////////////////////////////////////////////////////////////////////////////

module module_aquaveo_data
 !use module_export_GeoDB
  implicit none

  private

  save
  logical ::        m_stayopen
  logical ::        m_wrapper
  logical ::        m_mp3du = .FALSE.
  logical ::        m_export
  logical ::        m_exportDB
  logical ::        m_promptForArray, m_readBinArraySingle
  logical ::        m_strUseHdf, m_sfr2UseHdf, m_mnwUseHdf
  character(7)   :: m_filestat
  character(256) :: m_headfile
  character(256) :: m_inputfile
  character(256) :: m_path
  character(256) :: m_prefix
  character(256) :: m_disfile

  public :: ed_getstayopen, &
            ed_setstayopen, &
            ed_getwrapper, &
            ed_setwrapper, &
            ed_getheadfile, &
            ed_setheadfile, &
            ed_getinputfile, &
            ed_setinputfile, &
            ed_getpath, &
            ed_setpath, &
            ed_getprefix, &
            ed_setprefix, &
            ed_setMp3du, &
            ed_getMp3du, &
            ed_setExportGeoDB, &
            ed_setExportText, &
            ed_setExportGmsH5, &
            ed_setExportTables, &
            ed_getExportData, &
            ed_getExportDB, &
            ed_getfilestat, &
            ed_setfilestat, &
            ed_setPromptForArray, &
            ed_getPromptForArray, &
            ed_getReadBinArraySingle &
           ,ed_SetReadSfrH5 &
           ,ed_SfrUsingH5 &
           ,ed_SetReadStrH5 &
           ,ed_StrUsingH5 &
           ,ed_SetReadMnwH5 &
           ,ed_MnwUsingH5 &
           ,ed_getdisfile &
           ,ed_setdisfile
            

  contains

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_SetPromptForArray (a_prompt, a_array)

    implicit none
    logical, intent(in) :: a_prompt, a_array

    m_promptForArray = a_prompt
    m_readBinArraySingle = a_array

  end subroutine ed_SetPromptForArray
  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure logical function ed_getPromptForArray ()
    implicit none
    ed_getPromptForArray = m_promptForArray
  end function ed_getPromptForArray
  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure logical function ed_getReadBinArraySingle ()
    implicit none
    ed_getReadBinArraySingle = m_readBinArraySingle
  end function ed_getReadBinArraySingle
  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure logical function ed_getstayopen ()
    implicit none

    ed_getstayopen = m_stayopen
  end function ed_getstayopen

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setstayopen (a)
    implicit none
    logical, intent(in) :: a

    m_stayopen = a
  end subroutine ed_setstayopen

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure logical function ed_getwrapper ()
    implicit none

    ed_getwrapper = m_wrapper
  end function ed_getwrapper

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setwrapper (a)
    implicit none
    logical, intent(in) :: a

    m_wrapper = a
  end subroutine ed_setwrapper

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure character(len_trim(m_headfile)) function ed_getheadfile ()
    implicit none

    ed_getheadfile = m_headfile
  end function ed_getheadfile

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setheadfile (a)
    implicit none
    character(*), intent(in) :: a

    m_headfile = a
  end subroutine ed_setheadfile

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure character(len_trim(m_inputfile)) function ed_getinputfile ()
    implicit none

    ed_getinputfile = m_inputfile
  end function ed_getinputfile

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setinputfile (a)
    implicit none
    character(*), intent(in) :: a

    m_inputfile = a
  end subroutine ed_setinputfile

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure character(len_trim(m_path)) function ed_getpath ()
    implicit none

    ed_getpath = m_path
  end function ed_getpath

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setpath (a)
    implicit none
    character(*), intent(in) :: a

    m_path = a
  end subroutine ed_setpath

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure character(len_trim(m_prefix)) function ed_getprefix ()
    implicit none

    ed_getprefix = m_prefix
  end function ed_getprefix

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setprefix (a)
    implicit none
    character(*), intent(in) :: a

    m_prefix = a
  end subroutine ed_setprefix

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setMp3du (a_arg)
    implicit none
    integer, intent(in) :: a_arg

    m_mp3du = .FALSE.
    if (a_arg.ne.0) m_mp3du = .TRUE.
  end subroutine ed_setMp3du

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure logical function ed_getMp3du ()
    implicit none
    
    ed_getMp3du = m_mp3du
  end function ed_getMp3du

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setExportGeoDB (a_arg, a_fName)
    implicit none
    character(*), intent(in) :: a_arg, a_fName

    m_export = .TRUE.
    m_exportDB = .TRUE.
    call g_exp_Export(a_arg, a_fName)
  end subroutine ed_setExportGeoDB

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setExportText (a_arg, a_fName)
    implicit none
    character(*), intent(in) :: a_arg, a_fName

    m_export = .TRUE.
    call g_exp_Export(a_arg, a_fName)
  end subroutine ed_setExportText

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setExportGmsH5 (a_arg, a_fName)
    implicit none
    character(*), intent(in) :: a_arg, a_fName

    m_export = .TRUE.
    call g_exp_Export(a_arg, a_fName)
  end subroutine ed_setExportGmsH5

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setExportTables (a_arg)
    implicit none
    character(*), intent(in) :: a_arg

    call g_exp_Tables(a_arg)
  end subroutine ed_setExportTables

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure logical function ed_getExportData ()
    implicit none
    
    ed_GetExportData = m_export
  end function ed_getExportData

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure logical function ed_getExportDB ()
    implicit none
    
    ed_GetExportDB = m_exportDB
  end function ed_getExportDB

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure character(len_trim(m_filestat)) function ed_getfilestat ()
    implicit none

    ed_getfilestat = m_filestat
  end function ed_getfilestat
  
  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_setfilestat (a)
    implicit none
    character(*), intent(in) :: a

    m_filestat = a
  end subroutine ed_setfilestat

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_SetReadMnwH5 (a_logical)

    implicit none
    logical, intent(in) :: a_logical

    m_mnwUseHdf = a_logical

  end subroutine ed_SetReadMnwH5

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_SetReadStrH5 (a_logical)

    implicit none
    logical, intent(in) :: a_logical

    m_strUseHdf = a_logical

  end subroutine ed_SetReadStrH5

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_StrUsingH5(a_flag)

    implicit none
    integer, intent(out) ::a_flag

    a_flag = 0
    if (m_strUseHdf == .true.) a_flag = 1

  end subroutine ed_StrUsingH5

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  subroutine ed_SetReadSfrH5 (a_logical)

    implicit none
    logical, intent(in) :: a_logical

    m_sfr2UseHdf = a_logical

  end subroutine ed_SetReadSfrH5

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure logical function ed_SfrUsingH5()

    implicit none

    ed_SfrUsingH5 = m_sfr2UseHdf
  end function ed_SfrUsingH5

  !-----------------------------------------------------------------------------
  ! BRIEF:      
  !-----------------------------------------------------------------------------
  pure logical function ed_MnwUsingH5()

    implicit none

    ed_MnwUsingH5 = m_mnwUseHdf
  end function ed_MnwUsingH5
  
  !-------------------------------------------------------------------------------
  ! BRIEF:      
  !-------------------------------------------------------------------------------
  pure character(len_trim(m_disfile)) function ed_getdisfile ()
    implicit none

    ed_getdisfile = m_disfile
  end function ed_getdisfile

  !-------------------------------------------------------------------------------
  ! BRIEF:      
  !-------------------------------------------------------------------------------
  subroutine ed_setdisfile (a)
    implicit none
    character(*), intent(in) :: a

    m_disfile = a
  end subroutine ed_setdisfile


end module module_aquaveo_data

!///////////////////////////////////////////////////////////////////////////////
!
! MODULE: module_aquaveo_functions
!
!///////////////////////////////////////////////////////////////////////////////

module module_aquaveo_functions
  implicit none
  contains

  !-------------------------------------------------------------------------------
  ! BRIEF:      Called right before calling STOP.
  !-------------------------------------------------------------------------------
  subroutine stopfile (a_stayopen)
    implicit none
    logical, intent(in) :: a_stayopen

    ! print the call stack
    !call TRACEBACKQQ()

    if (a_stayopen == .true.) then
      write(*,*) ' '
      pause
    endif
  end subroutine stopfile

  !-------------------------------------------------------------------------------
  ! BRIEF:   Given a file name that may include a path, determine the path.
  ! PARAMS:  a_name - The file name (may include a path, prefix, and suffix).
  !          a_path - The path.
  ! EXAMPLE: a_name = 'C:\path\more path\prefix.suffix'
  !          a_path = 'C:\path\more path\'
  ! PRE:     You must ensure that a_path has enough space.
  !-------------------------------------------------------------------------------
  subroutine getpath (a_name, a_path)
    implicit none
    character(*), intent(in) ::  a_name
    character(*), intent(out) :: a_path

    integer ::                   count

    a_path = a_name

    ! search backwards for the first '\' or '/'
    do count = len(a_path), 1, -1
      if ((a_path(count:count) == '\') .or. (a_path(count:count) == '/')) exit

      a_path(count:count) = ' '
    end do
  end subroutine getpath

  !-------------------------------------------------------------------------------
  ! BRIEF:   Find and return just the prefix part of the filename.
  ! PARAMS:  a_name   - The file name (may include a path, prefix, and suffix).
  !          a_prefix - The prefix.
  ! EXAMPLE: a_name   = 'C:\path\more path\prefix moreprefix.suffix'
  !          a_prefix = 'prefix moreprefix'
  ! PRE:     You must ensure that a_path has enough space.
  !-------------------------------------------------------------------------------
  subroutine getprefix (a_name, a_prefix)
    implicit none
    character(*), intent(in) ::  a_name
    character(*), intent(out) :: a_prefix

    integer ::     begincount
    integer ::     endcount
    integer ::     i
    logical ::     found

    begincount = len_trim(a_name)
    endcount = begincount

    ! Search from the right for the right-most '.'
    found = .false.
    do endcount = endcount, 1, -1
      if (a_name(endcount:endcount) == '.') then
        found = .true.
        exit
      endif
    end do

    ! If we didn't find a '.' then the end is the right-most non-space char
    if (found == .false.) then
      endcount = len_trim(a_name)
    else
      endcount = endcount - 1
    endif

    ! Search from the right for the first '\' or '/'
    do begincount = endcount, 1, -1
      if ((a_name(begincount:begincount) == '\') .or. &
          (a_name(begincount:begincount) == '/') .or. &
          (begincount <= 0)) exit

    end do

    begincount = begincount+1

    a_prefix = a_name(begincount:endcount)

  end subroutine getprefix

  !-------------------------------------------------------------------------------
  ! BRIEF:   Removes the path from fname, if one exists.
  ! PARAMS:  a_name   - The file name (may include a path, prefix, and suffix).
  ! EXAMPLE: Before: a_name = 'C:\path\more path\prefix.suffix'
  !          After:  a_name = 'prefix.suffix'
  !-------------------------------------------------------------------------------
  subroutine removepath (a_fname)
    implicit none
    character(*), intent(inout) :: a_fname

    character(len_trim(a_fname)) :: path

    call getpath(a_fname, path)
    a_fname = a_fname(len_trim(path) + 1:len_trim(a_fname))

  end subroutine removepath

  !-------------------------------------------------------------------------------
  ! BRIEF:   Builds a filename from the global prefix, what, index and ext.
  ! PARAMS:  a_fname   - The file name to be created
  !          a_prefix1 - The first part of the file name prefix
  !          a_prefix1 - The second part of the file name prefix
  !          a_index   - A number from 0 to 999
  !          a_ext     - The file name extension
  ! EXAMPLE: Given:
  !          a_prefix1 - 'prefix 1'
  !          a_prefix2 - 'prefix 2'
  !          a_index   - 7
  !          a_ext     - 'some extention'
  !          Result (a_fname) will be: 'prefix 1_prefix 2007.some extension'
  ! PRE:     You must ensure that a_fname has enough space.
  !-------------------------------------------------------------------------------
  subroutine setname (a_fname, a_prefix1, a_prefix2, a_index, a_ext)
    implicit none
    character(*), intent(inout) :: a_fname
    character(*), intent(in) ::    a_prefix1
    character(*), intent(in) ::    a_prefix2
    integer, intent(in) ::         a_index
    character(*), intent(in) ::    a_ext

    character(5) :: indexstr
    character(len_trim(a_prefix1)+len_trim(a_prefix2)+4+len_trim(a_ext)):: str

    ! Add the '_' between the prefixes if both exist
    if (len_trim(a_prefix1) > 0 .and. len_trim(a_prefix2) > 0) then
      str = a_prefix1(1:len_trim(a_prefix1))//'_'// &
                a_prefix2(1:len_trim(a_prefix2))
    else
      str = a_prefix1(1:len_trim(a_prefix1))// &
                a_prefix2(1:len_trim(a_prefix2))
    endif

    write(indexstr,'(I3.3)') a_index

    ! Add the '.' and extension if it's not blank
    if (len_trim(a_ext) > 0) then
      a_fname = str(1:len_trim(str))//indexstr(1:3)//'.'//a_ext(1:len_trim(a_ext))
    else
      a_fname = str(1:len_trim(str))//indexstr(1:3)
    endif

  end subroutine setname

  !-------------------------------------------------------------------------------
  ! BRIEF:  Removes any leading or trailing quotes (") from string
  ! PARAMS: a_string - The string to remove quotes from
  !-------------------------------------------------------------------------------
  subroutine trimquotes (a_string)
    implicit none
    character(*), intent(inout) :: a_string

    integer :: bg
    integer :: en
    integer :: zero

    ! Skip forward through leading spaces
    do bg = 1, len(a_string), 1
      if (a_string(bg:bg) /= ' ') exit
    end do

    ! Skip forward through leading quotes
    do bg = bg, len(a_string), 1
      if (a_string(bg:bg) /= '"') exit
    end do

    ! Skip forward through leading single quotes
    do bg = bg, len(a_string), 1
      if (a_string(bg:bg) /= '''') exit
    end do

    ! Skip backward through trailing 0
    do en = len(a_string), 1, -1
    zero = ichar(a_string(en:en))
    if (zero /= 0) exit 
    end do

    ! Skip backward through trailing spaces
    do en = en, 1, -1
      if (a_string(en:en) /= ' ') exit
    end do

    ! Skip backward through trailing quotes
    do en = en, 1, -1
      if (a_string(en:en) /= '"') exit
    end do

    ! Skip backward through trailing single quotes
    do en = en, 1, -1
      if (a_string(en:en) /= '''') exit
    end do

    a_string = a_string(bg:en)
  end subroutine trimquotes

  !-------------------------------------------------------------------------------
  ! BRIEF:  Puts the path onto the front of fname.
  !
  !         You need to have already determined path (ex.using getpath) and
  !         send that in as path. You would usually call this before you
  !         open a file.
  ! PARAMS: a_path  - The path to put on the front of a_fname
  !         a_fname - The file name (if it already includes a path, that
  !                   path will be replaced with a_path)
  ! PRE:    You have to make sure a_fname has enough room to fit path.
  !-------------------------------------------------------------------------------
  subroutine setpath (a_path, a_fname)
    implicit none
    character(*), intent(in) ::    a_path
    character(*), intent(inout) :: a_fname

    character(len_trim(a_path)) :: path
    character(len_trim(a_fname)) :: fname
    integer ::      pathlength

    path = a_path
    fname = a_fname
    call trimquotes(path)
    call trimquotes(fname)

    call removepath(fname)
    pathlength = len_trim(path)

    a_fname = path(1:pathlength)//fname

  end subroutine setpath

  !-------------------------------------------------------------------------------
  ! BRIEF:  Puts the extension onto end of fname, replacing any that may be there.
  ! PARAMS: a_fname - The file name (it may or may not already have an extension)
  !         a_ext   - The extension the filename will have when we're done.  This
  !                   should NOT include a '.' (ie 'this' not '.this')
  ! PRE:    You have to make sure a_fname has enough room to fit ext.
  !-------------------------------------------------------------------------------
   subroutine setextension (a_fname, a_ext)
     implicit none
     character(*), intent(inout) :: a_fname
     character(*), intent(in) ::    a_ext

     character(len_trim(a_fname)+len_trim(a_ext)+1) :: name
     integer :: i
     integer :: j
     integer :: k

     i = index(a_fname, '.', .true.)
     j = index(a_fname, '\', .true.)
     k = index(a_fname, '/', .true.)

     if (i == 0 .or. (i < j .or. i < k)) then
       ! There is no extension
       if (a_ext /= '') then
         name = a_fname(1:len_trim(a_fname))//'.'
       else
         name = a_fname(1:len_trim(a_fname))
       endif
       i = len_trim(name)
     else
       name = a_fname(1:len_trim(a_fname))
     endif

     a_fname = name(1:i)//a_ext(1:len_trim(a_ext))

   end subroutine setextension
   
  !-------------------------------------------------------------------------------
  ! BRIEF:  reads a binary array in single precision
  !-------------------------------------------------------------------------------
  subroutine ef_ExtBinArrFlt(A,II,JJ,LOCAT,err,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY)
    implicit none
    integer, intent(in)           :: II, JJ, LOCAT
    double precision, intent(out) :: PERTIM,TOTIM, A(JJ,II)
    integer, intent(out)          :: NCOL,NROW,ILAY,err
    character, intent(out)        :: TEXT(16)

    integer   :: l1, l2
    real*4    :: pertimFlt, totimFlt
    real*4, ALLOCATABLE :: arrFlt(:,:)

    ALLOCATE ( arrFlt(JJ,II) )

    READ(LOCAT) pertimFlt,totimFlt,TEXT,NCOL,NROW,ILAY
    if ((err.EQ.0.and.NCOL.EQ.JJ.and.NROW.EQ.II).or.err.NE.0) then
      err = 0
      PERTIM = REAL(pertimFlt)
      TOTIM = REAL(totimFlt)
      READ(LOCAT) arrFlt
      A = REAL(arrFlt)
    else
      err = 1
    endif
  end subroutine ef_ExtBinArrFlt
  !-------------------------------------------------------------------------------
  ! BRIEF:  reads a binary array in double precision
  !-------------------------------------------------------------------------------
  subroutine ef_ExtBinArrDbl(A,II,JJ,LOCAT,err,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY)
    implicit none
    integer, intent(in)           :: II, JJ, LOCAT
    double precision, intent(out) :: PERTIM,TOTIM, A(JJ,II)
    integer, intent(out)          :: NCOL,NROW,ILAY,err
    character, intent(out)        :: TEXT(16)

    integer   :: l1, l2
    real*8    :: pertimDbl, totimDbl
    real*8, ALLOCATABLE :: arrDbl(:,:)

    ALLOCATE ( arrDbl(JJ,II) )

    READ(LOCAT) pertimDbl,totimDbl,TEXT,NCOL,NROW,ILAY
    if ((err.EQ.0.and.NCOL.EQ.JJ.and.NROW.EQ.II).or.err.NE.0) then
      err = 0
      PERTIM = REAL(pertimDbl)
      TOTIM = REAL(totimDbl)
      READ(LOCAT) arrDbl
      A = REAL(arrDbl)
    else
      err = 1
    end if
  end subroutine ef_ExtBinArrDbl
  !-------------------------------------------------------------------------------
  ! BRIEF:  reads a binary array and allows user input to decide the format
  !         of the array (single or double)
  !-------------------------------------------------------------------------------
  subroutine ef_ReadExternalBinaryArray8 (A,II,JJ,LOCAT,err,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY)
    USE IFLPORT
    use module_aquaveo_data
    
    implicit none
    integer, intent(in)           :: II, JJ, LOCAT
    double precision, intent(out) :: PERTIM,TOTIM, A(JJ,II)
    integer, intent(out)          :: NCOL,NROW,ILAY,err
    character, intent(out)        :: TEXT(16)

    character :: inputChar;
    integer   :: filePos, seekErr
    logical   :: readBinArraySingle

    ! Get the file position
    filePos = FTELL(LOCAT)

    err = 0
    call ef_ExtBinArrFlt(A,II,JJ,LOCAT,err,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY)
    if (err.EQ.1) then
      seekErr = FSEEK(LOCAT, filePos, 0) ! 0 is SEEK_SET
      if (NOT(seekErr.EQ.0)) then
        err = 1
        return
      end if
      err = 0
      call ef_ExtBinArrDbl(A,II,JJ,LOCAT,err,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY)
    endif
    if (err.EQ.0) return

    if (err.EQ.1) then ! prompt for method to read binary array
      seekErr = FSEEK(LOCAT, filePos, 0) ! 0 is SEEK_SET
      if (NOT(seekErr.EQ.0)) return

      inputChar = 'y' !set the default to be single precision
      if (ed_getPromptForArray()) then
        WRITE (*,*) 'NCOL/NROW specified in external binary array '
        WRITE (*,*) 'file does not match NCOL/NROW specified in '
        WRITE (*,*) 'the DIS package. Is the binary array single '
        WRITE (*,*) 'precision? (Y/N)'
        read (*,'(a)') inputChar
        if (inputChar.EQ.'') inputChar = 'y'
        if (inputChar.EQ.'Y') inputChar = 'y'
        if (inputChar.EQ.'y') then
          WRITE (*,*) 'Single precision assumed for binary arrays'
          readBinArraySingle = .true.
        else
          WRITE (*,*) 'Double precision assumed for binary arrays'
          readBinArraySingle = .false.
        endif
        call ed_SetPromptForArray(.false., readBinArraySingle)
      endif
      
      if (ed_getReadBinArraySingle()) then
        call ef_ExtBinArrFlt(A,II,JJ,LOCAT,err,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY)
      else
        call ef_ExtBinArrDbl(A,II,JJ,LOCAT,err,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY)
      endif
      err = 0
    end if
  end subroutine ef_ReadExternalBinaryArray8
  !-------------------------------------------------------------------------------
  ! BRIEF:  reads a binary array and allows user input to decide the format
  !         of the array (single or double)
  !-------------------------------------------------------------------------------
  subroutine ef_ReadExternalBinaryArray (A,II,JJ,LOCAT,err,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY)
    USE IFLPORT
    use module_aquaveo_data
    
    implicit none
    integer, intent(in)    :: II, JJ, LOCAT
    real, intent(out)      :: PERTIM,TOTIM, A(JJ,II)
    integer, intent(out)   :: NCOL,NROW,ILAY,err
    character, intent(out) :: TEXT(16)
    
    double precision PERTIMDbl, TOTIMDbl
    double precision, ALLOCATABLE :: ADbl(:,:)

    ALLOCATE ( ADbl(JJ,II) )
    
    call ef_ReadExternalBinaryArray8(ADbl,II,JJ,LOCAT,err,PERTIMDbl,TOTIMDbl,TEXT,NCOL,NROW,ILAY)
    if (err.eq.0) then
      PERTIM = REAL(PERTIMDbl)
      TOTIM = REAL(TOTIMDbl)
      A = REAL(ADbl)
    end if
  end subroutine ef_ReadExternalBinaryArray

end module module_aquaveo_functions


!-------------------------------------------------------------------------------
! BRIEF: Gives an error when a is not true
!-------------------------------------------------------------------------------
subroutine assert (a)
  implicit none
  logical, intent(in) :: a

  ! This if statement lets me set a breakpoint
  if (a == .false.) then
    write(*,*) 'ERROR!'
  end if

!  assert = a
end subroutine assert

!-------------------------------------------------------------------------------
! BRIEF: Tests the aquaveo subroutines.
!-------------------------------------------------------------------------------
subroutine testaquaveoutils
  implicit none
  
  call testaquaveodata()
  call testaquaveofunctions()
end subroutine testaquaveoutils

!-------------------------------------------------------------------------------
! BRIEF: Tests the module_aquaveo_data subroutines.
!-------------------------------------------------------------------------------
subroutine testaquaveodata
  use module_aquaveo_data
  implicit none

  !------------------------------
  ! test 1

  call ed_setstayopen(.true.)
  call ed_setheadfile('the head file')
  call ed_setinputfile('the input file')
  call ed_setpath('the path')
  call ed_setprefix('the prefix')

  call assert(ed_getstayopen() == .true.)
  call assert(ed_getheadfile() == 'the head file')
  call assert(ed_getinputfile() == 'the input file')
  call assert(ed_getpath() == 'the path')
  call assert(ed_getprefix() == 'the prefix')

  !------------------------------
  ! test 2

  call ed_setstayopen(.false.)
  call ed_setheadfile('')
  call ed_setinputfile('')
  call ed_setpath('')
  call ed_setprefix('')

  call assert(ed_getstayopen() == .false.)
  call assert(ed_getheadfile() == '')
  call assert(ed_getinputfile() == '')
  call assert(ed_getpath() == '')
  call assert(ed_getprefix() == '')

end subroutine testaquaveodata

!-------------------------------------------------------------------------------
! BRIEF: Tests the module_aquaveo_functions subroutines
!-------------------------------------------------------------------------------
subroutine testaquaveofunctions
  use module_aquaveo_functions
  implicit none

  character(256) :: path
  character(256) :: prefix
  character(256) :: name
  character(256) :: prefix1
  character(256) :: prefix2
  character(256) :: ext
  integer ::        length
  integer ::        index

  !------------------------------
  ! test getpath

  name = '" some sort of string "'
  call trimquotes(name)
  call assert(name == ' some sort of string')

  name = 'some sort of string '
  call trimquotes(name)
  call assert(name == 'some sort of string')

  name = '"some sort of string '
  call trimquotes(name)
  call assert(name == 'some sort of string')

  name = 'some sort of string "'
  call trimquotes(name)
  call assert(name == 'some sort of string')

  !------------------------------
  ! test getpath

  call getpath('//path/more path/prefix moreprefix.suffix', path)
  call assert(path == '//path/more path/')

  call getpath('C:\path\more path\prefix moreprefix.suffix', path)
  call assert(path == 'C:\path\more path\')

  call getpath('prefix moreprefix.suffix', path)
  call assert(path == '')

  call getpath('', path)
  call assert(path == '')

  !------------------------------
  ! test getprefix 

  call getprefix('C:\path\more path\prefix moreprefix.suffix', prefix)
  call assert(prefix == 'prefix moreprefix')

  call getprefix('C:\path\more path\prefix moreprefix', prefix)
  call assert(prefix == 'prefix moreprefix')

  call getprefix('//path/more path/prefix moreprefix', prefix)
  call assert(prefix == 'prefix moreprefix')

  call getprefix('prefix moreprefix', prefix)
  call assert(prefix == 'prefix moreprefix')

  call getprefix('', prefix)
  call assert(prefix == '')

  !------------------------------
  ! test removepath

  name = 'C:\path\more path\prefix moreprefix.suffix'
  call removepath(name)
  call assert(name == 'prefix moreprefix.suffix')

  name = '//path/more path/prefix moreprefix.suffix'
  call removepath(name)
  call assert(name == 'prefix moreprefix.suffix')

  name = 'prefix moreprefix.suffix'
  call removepath(name)
  call assert(name == 'prefix moreprefix.suffix')

  name = ''
  call removepath(name)
  call assert(name == '')

  !------------------------------
  ! test setextension

  name = ''
  ext = ''
  call setextension(name, ext)
  call assert(name .eq. '')

  name = 'the filename'
  ext = ''
  call setextension(name, ext)
  call assert(name .eq. 'the filename')

  name = ''
  ext = 'the new extension'
  call setextension(name, ext)
  call assert(name .eq. '.the new extension')

  name = 'the filename'
  ext = 'the new extension'
  call setextension(name, ext)
  call assert(name .eq. 'the filename.the new extension')

  name = 'the filename.the extension'
  ext = 'the new extension'
  call setextension(name, ext)
  call assert(name .eq. 'the filename.the new extension')

  ! with paths '\'
  name = 'C:\1.2\more path\'
  ext = ''
  call setextension(name, ext)
  call assert(name .eq. 'C:\1.2\more path\')

  name = 'C:\1.2\more path\the filename'
  ext = ''
  call setextension(name, ext)
  call assert(name .eq. 'C:\1.2\more path\the filename')

  name = 'C:\1.2\more path\'
  ext = 'the new extension'
  call setextension(name, ext)
  call assert(name .eq. 'C:\1.2\more path\.the new extension')

  name = 'C:\1.2\more path\the filename'
  ext = 'the new extension'
  call setextension(name, ext)
  call assert(name .eq. 'C:\1.2\more path\the filename.the new extension')

  name = 'C:\1.2\more path\the filename.the extension'
  ext = 'the new extension'
  call setextension(name, ext)
  call assert(name .eq. 'C:\1.2\more path\the filename.the new extension')

  ! with paths '/'
  name = '/1.2/more path/'
  ext = ''
  call setextension(name, ext)
  call assert(name .eq. '/1.2/more path/')

  name = '/1.2/more path/the filename'
  ext = ''
  call setextension(name, ext)
  call assert(name .eq. '/1.2/more path/the filename')

  name = '/1.2/more path/'
  ext = 'the new extension'
  call setextension(name, ext)
  call assert(name .eq. '/1.2/more path/.the new extension')

  name = '/1.2/more path/the filename'
  ext = 'the new extension'
  call setextension(name, ext)
  call assert(name .eq. '/1.2/more path/the filename.the new extension')

  name = '/1.2/more path/the filename.the extension'
  ext = 'the new extension'
  call setextension(name, ext)
  call assert(name .eq. '/1.2/more path/the filename.the new extension')

  !------------------------------
  ! test setpath

  path = ''
  name = ''
  call setpath(path, name)
  call assert(name == '')

  path = ''
  name = 'D:\path\other path\prefix moreprefix.suffix'
  call setpath(path, name)
  call assert(name == 'prefix moreprefix.suffix')

  path = 'D:\path\other path\'
  name = 'prefix moreprefix.suffix'
  call setpath(path, name)
  call assert(name == 'D:\path\other path\prefix moreprefix.suffix')

  path = 'C:\path\more path\'
  name = 'D:\path\other path\prefix moreprefix.suffix'
  call setpath(path, name)
  call assert(name == 'C:\path\more path\prefix moreprefix.suffix')

  path = '/path/more path/'
  name = '/path/other path/prefix moreprefix.suffix'
  call setpath(path, name)
  call assert(name == '/path/more path/prefix moreprefix.suffix')

  path = '/path/more path/'
  name = 'D:\path\other path\prefix moreprefix.suffix'
  call setpath(path, name)
  call assert(name == '/path/more path/prefix moreprefix.suffix')

  !------------------------------
  ! test setname

  prefix1 = 'prefix 1'
  prefix2 = 'prefix 2'
  index = 7
  ext = 'the extension'
  call setname(name, prefix1, prefix2, index, ext)
  call assert(name == 'prefix 1_prefix 2007.the extension')

  prefix1 = ''
  prefix2 = 'prefix 2'
  index = 7
  ext = 'the extension'
  call setname(name, prefix1, prefix2, index, ext)
  call assert(name == 'prefix 2007.the extension')

  prefix1 = 'prefix 1'
  prefix2 = ''
  index = 7
  ext = 'the extension'
  call setname(name, prefix1, prefix2, index, ext)
  call assert(name == 'prefix 1007.the extension')

  prefix1 = 'prefix 1'
  prefix2 = ''
  index = 7
  ext = ''
  call setname(name, prefix1, prefix2, index, ext)
  call assert(name == 'prefix 1007')

  prefix1 = ''
  prefix2 = ''
  index = 7
  ext = ''
  call setname(name, prefix1, prefix2, index, ext)
  call assert(name == '007')

end subroutine testaquaveofunctions

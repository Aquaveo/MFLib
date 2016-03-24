!-------------------------------------------------------------------------------
! BRIEF: Contains helpful aquaveo code.
!-------------------------------------------------------------------------------

!///////////////////////////////////////////////////////////////////////////////
!
! MODULE: module_aquaveo
!
!///////////////////////////////////////////////////////////////////////////////

module module_aquaveo
  !use dfport ! aquaveo
  use module_aquaveo_data
  use module_aquaveo_functions
  implicit none

  private :: iGetNameFile

  save
  public :: readcommandline, getNameFile, getNameOrLgrControlFile, printWrapperInfo

  contains

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine iGetNameFile (a_fname, a_flag)
    character(*), intent(out) :: a_fname
    integer, intent(in)       :: a_flag
    character(256)            :: fname
    character(256)            :: path
    character(256)            :: fltype
    integer                   :: isup
    integer                   :: ierr
    integer                   :: inunit

    call readcommandline(fname)

    isup = 95
    ! If the command line was empty, get name file from user.
    ! Keep trying to open the name file until they give us a good one.
    do
      if (fname(1:1) == ' ') then
        if (a_flag.eq.1) then
          write(*,*) ' Enter the name of the NAME or LGR CONTROL FILE:'
        else
          write(*,*) ' Enter the name of the NAME FILE:'
        end if
        read(*,'(a)') fname
        call trimquotes(fname)
      endif

      open(unit=isup, file=fname, status='old', action='read', iostat=ierr)
      if (ierr == 0) exit
      ! try adding .nam to the file and try to open it
      call setextension(fname, 'nam')
      open(unit=isup, file=fname, status='old', action='read', iostat=ierr)
      if (ierr == 0) exit

      ! set the fname to be an empty string and go back to the beginning of
      ! the loop
      fname = ' '
    end do

    ! get the path to the file
    call getpath(fname, path)

    ! if this is a GMS MODFLOW super file then get the name file by
    ! reading this file
    read(isup, '(a)') fltype
    if ((fltype(1:7).ne.'MF2KSUP').and.(fltype(1:8).ne.'MF2K5SUP').and. &
        (fltype(1:8).ne.'MFNWTSUP').and.(fltype(1:8).ne.'MFLGRSUP').and. &
        (fltype(1:8).ne.'MFUSGSUP')) then
      write(*,*) ' This file is not a GMS MODFLOW superfile. Assuming NAM file '
    else
      ! read the name file name from the superfile
      do
        603 format(a5,i4,a)
        read(isup, '(a)') fltype
        if (fltype(1:4).eq.'NAME') then
          backspace(isup)
          read(isup,603) fltype, inunit, fname
          call trimquotes(fname)
          call setpath(fname, path)
          exit
        endif
      end do
    endif


    close(isup)
    a_fname = fname
  end subroutine iGetNameFile

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine getNameOrLgrControlFile (a_fname)
    character(*), intent(out) :: a_fname
    call iGetNameFile(a_fname, 1)
  end subroutine getNameOrLgrControlFile

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine getNameFile (a_fname)
    character(*), intent(out) :: a_fname
    call iGetNameFile(a_fname, 0)
  end subroutine getNameFile

  !-----------------------------------------------------------------------------
  ! BRIEF:  
  !-----------------------------------------------------------------------------
  subroutine readcommandline (a_fname)
    implicit none
    character(*), intent(out) :: a_fname
    character(2048)           :: arg1, arg2, arg3, arg4
    character*10              :: stayopen, wrapper
    integer                   :: ierr, isup, flag

    call ed_setstayopen(.FALSE.)
    call ed_setwrapper(.FALSE.)
    call ed_setfilestat('UNKNOWN')
    
    a_fname = ''
    if (iargc() > 0) then
      call getarg(1, a_fname)
    endif

    arg1 = ''
    stayopen = '0'
    if (iargc() > 1) then ! stay open
      call getarg(2, arg1)
      if (arg1.eq.'0') then
        call ed_setstayopen(.FALSE.)
      else if (arg1.eq.'1') then
        call ed_setstayopen(.TRUE.)
      else if (arg1(1:1).eq.'-') then
        flag = 0
        if (arg1.eq.'-exportText') flag=3
        if (arg1.eq.'-exportText_ArraysInFolder') flag=3
        if (arg1.eq.'-exportText_ArraysInternal') flag=3
        if (arg1.eq.'-exportTextAI') flag=3
        if (arg1.eq.'-exportGeoDB') flag=1
        if (arg1.eq.'-exportGeoDBFree') flag=1
        if (arg1.eq.'-exportSQLite') flag=1
        if (arg1.eq.'-exportGmsH5') flag=2
        if (arg1.eq.'-exportGmsCompressedH5') flag=2
        if (flag.gt.0.and.flag.lt.4) then
          call ed_SetPromptForArray(.true., .true.)
          call getarg(3, arg2)
          if (flag.eq.1) then
            call ed_setExportGeoDB(arg1,arg2)
          else if (flag.eq.2) then
            call ed_setExportGmsH5(arg1,arg2)
          else if (flag.eq.3) then
            call ed_setExportText(arg1,arg2)
          endif
          if (iargc() > 4) then
            call getarg(4, arg3)
            if (arg3.eq.'-exportTables') then
              call getarg(5, arg4)
              call ed_setExportTables(arg4)
            endif
          endif
          call ed_setfilestat('SCRATCH')
        endif
      endif
    endif

    wrapper = '0'
    if (iargc() > 2) then ! wrapper
      call getarg(3, wrapper)
      if (wrapper.ne.'0') then
        call ed_setwrapper(.TRUE.)
      else
        call ed_setwrapper(.FALSE.)
      endif
    endif


  end subroutine readcommandline 

  !-----------------------------------------------------------------------------
  ! BRIEF:  prints the error information if you are running modflow with the
  !         model wrapper from inside of GMS
  !-----------------------------------------------------------------------------
  subroutine printWrapperInfo (iter, err, per, step)
    implicit none
    integer          :: iter, per, step
    double precision :: err
    character(256)   :: fmt = "('MODELDATA',1X,I5,1X,G12.4,1X,I3,1X,I4)"

    if (ed_getwrapper()) then
      write(*, fmt)  iter,err,per,step
    endif

  end subroutine printWrapperInfo
end module module_aquaveo


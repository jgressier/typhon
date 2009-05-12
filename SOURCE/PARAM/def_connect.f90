!------------------------------------------------------------------------------!
! Procedure : def_connect  
! 
! Fonction 
!   Definition of connections (classical, periodicity...)
!
!------------------------------------------------------------------------------!
subroutine def_connect(block, defsolver)
 
use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER

implicit none

! -- INPUTS --
type(rpmblock), target :: block

! -- OUTPUTS --
type(mnu_solver)                             :: defsolver

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nconnect          ! nombre de conditions aux limites
integer                  :: ib, nkey, idef
integer                  :: izr            ! indice de parcours du tableau de raccords
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire
logical                  :: same_name

! ----- BODY -----

call print_info(5,"* Definition of  Connections")

! -- Recherche du BLOCK:BOCO

pblock => block
call seekrpmblock(pblock, "CONNECTION", 0, pcour, nconnect)

defsolver%nconnect = nconnect
allocate(defsolver%connect(nconnect))

do ib = 1, nconnect
  
  call seekrpmblock(pblock, "CONNECTION", ib, pcour, nkey)

  ! -- Family names --

  call rpmgetkeyvalstr(pcour, "FAMILY1", str)
  defsolver%connect(ib)%fam1 = str

  call rpmgetkeyvalstr(pcour, "FAMILY2", str)
  defsolver%connect(ib)%fam2 = str

  ! -- Type of connection --

  call rpmgetkeyvalstr(pcour, "TYPE", str)

  defsolver%connect(ib)%type = inull

  if (samestring(str, "PERIODIC")) defsolver%connect(ib)%type = bccon_periodic

  select case(defsolver%connect(ib)%type)

  case(bccon_periodic)
    call rpmgetkeyvalstr(pcour, "PERIODICITY", str)
    defsolver%connect(ib)%link = str
    call print_info(10,"  . periodic connection "//trim(defsolver%connect(ib)%link)//" for "//&
                       trim(defsolver%connect(ib)%fam1)//"/"//trim(defsolver%connect(ib)%fam2))
    ! --- look for periodicity definition ---
    idef      = 0
    same_name = .false.
    print*,defsolver%defmesh%nperiodicity
    do while ((.not.same_name).and.(idef+1 <= defsolver%defmesh%nperiodicity))
      idef = idef + 1
      same_name = samestring(str, defsolver%defmesh%periodicity(idef)%name)
    enddo 
    if (same_name) then
      defsolver%connect(ib)%ilink = idef
    else
      call erreur("Bad parameter", "impossible to find PERIODICITY name "//trim(str))
    endif

  case default
    call erreur("parameter parsing (def_connect)","unknown type of connection")
  endselect

enddo ! -- loop on connections --

endsubroutine def_connect
!------------------------------------------------------------------------------!
! Changes History
!
! July  2008: creation (periodicity)     
!------------------------------------------------------------------------------!



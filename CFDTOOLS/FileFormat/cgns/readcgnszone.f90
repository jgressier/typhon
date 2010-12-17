!------------------------------------------------------------------------------!
! Procedure : readcgnszone                Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Lecture d'une zone d'un fichier CGNS
!
!------------------------------------------------------------------------------!

subroutine readcgnszone(unit, ib, iz, zone) 

use CGNS_STRUCT   ! Definition des structures CGNS
use IOCFD        ! Sorties standard TYPHON

implicit none 

! -- Entrees --
integer             :: unit       ! numero d'unite pour la lecture
integer             :: ib, iz     ! numero de base et de zone

! -- Sorties --
type(st_cgns_zone)  :: zone       ! structure CGNS : zone

! -- Variables internes --
integer       :: size(3,3)        ! tableau d'informations de la zone
integer       :: ier, igeo        ! code d'erreur
integer       :: i, ibc           ! indice courant
integer       :: nmax_elem        ! nombre total d'elements
character(len=100)     :: str_w   ! nom fantome

! -- BODY --
   
call cg_base_read_f(unit, ib, zone%nom, zone%imesh, igeo, ier)

!! BUG : test desactive car ier /= 0 meme si tout est correct
!if (ier /= 0) call erreur("Lecture CGNS","Probleme a la lecture de la base")

! --- Lecture du type de zone ---
          
call cg_zone_type_f(unit, ib, iz, zone%type, ier)
if (ier /= 0) call cfd_error("(CGNS) cannot read CGNS zone type")

select case(zone%type)
  case(Structured,Unstructured)
    write(str_w,'(a,i3,a,a)') "- Zone",iz,": type",ZoneTypeName(zone%type)
    call cfd_print(str_w)
  case default
    call cfd_error("(CGNS) unknown CGNS zone type")
endselect

! --- Lecture des informations de la zone ---

call cg_zone_read_f(unit, ib, iz, zone%nom, size, ier)
if (ier /= 0)   call cfd_error("(CGNS) cannot read CGNS zone information")

select case(zone%type)

  case(Structured)     ! cas STRUCTURE
    ! size(1:3, 1:3) contient             (cf CGNS Mid-level Library)
    !     (1:3, 1) le nombre de sommets ni,nj,nk
    !     (1:3, 2) le nombre de cellules
    !     (1:3, 3) ?
    zone%mesh%ni = size(1,1)
    zone%mesh%nj = size(2,1)
    zone%mesh%nk = size(3,1)

  case(Unstructured)   ! cas NON STRUCTURE
    ! size(1:3, 1) contient             (cf CGNS Mid-level Library)
    !     (1, 1) le nombre de sommets
    !     (2, 1) le nombre de cellules
    !     (3, 1) le nombre de sommets aux limites
    ! ce sont en fait les trois premiers entiers du tableau sous forme lineaire
    zone%mesh%ni = size(1,1)
    zone%mesh%nj = 1
    zone%mesh%nk = 1
    nmax_elem    = size(2,1)

  case default
    call cfd_error("(CGNS) unknown CGNS zone type (b)")
endselect

! --- Allocation et Lecture des sommets du maillage ---

allocate(zone%mesh%vertex(zone%mesh%ni, zone%mesh%nj, zone%mesh%nk)) 
call readcgnsvtex(unit, ib, iz, zone%mesh)
   
! --- Lecture des connectivites  ---

select case(zone%type)

case(Structured)
  call cfd_error("(CGNS) structured multiblock mesh not implemented")

case(Unstructured)
  call readcgns_ustconnect(unit, ib, iz, zone, nmax_elem)

case default
    call cfd_error("(CGNS) unknown CGNS zone type (c)")
endselect

! --- Lecture des conditions aux limites ---

call cg_nbocos_f(unit, ib, iz, zone%nboco, ier)
if (ier /= 0) call cfd_error("(CGNS) cannot read number of CGNS boundary conditions")

write(str_w,'(a,i3,a)') ". reading",zone%nboco," CGNS boundary conditions"
call cfd_print(str_w)

allocate(zone%boco(zone%nboco))

do ibc = 1, zone%nboco

  select case(zone%type)

  case(Structured)
    call cfd_error("(CGNS) structured multiblock mesh not implemented")

  case(Unstructured)
    call readcgns_ustboco(unit, ib, iz, ibc, zone%boco(ibc))

  case default
    call cfd_error("(CGNS) unknown CGNS zone type (d)")

  endselect

enddo

! --- fermeture du fichier ---

call cg_close_f(unit, ier)

!-------------------------
endsubroutine readcgnszone

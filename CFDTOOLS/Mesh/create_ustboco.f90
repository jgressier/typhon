!------------------------------------------------------------------------------!
! Procedure : create_ustboco
!  
! Fonction
!   Creation des conditions aux limites par conversion CGNS->TYPHON
!   Test de coherence entre les faces CGNS et les faces creees dans TYPHON
!
! Defauts/Limitations/Divers :
!   ATTENTION : les faces sont supposees ordonnees (faces limites en fin de tab)
!
!------------------------------------------------------------------------------!
subroutine create_ustboco(umesh) 

use USTMESH       ! Definition des structures maillage non structure
use IOCFD  

implicit none 

! -- INPUTS --

! -- INPUTS/OUTPUTS --
type(st_ustmesh)    :: umesh           ! unstructured mesh

! -- Internal variables --
integer             :: nface_int       ! nombre de faces internes
integer             :: nface_lim       ! nombre de faces limites
integer             :: ib, if, nf, iib

! -- BODY --

! -- Creation des conditions aux limites --

do ib = 1, umesh%nboco

  umesh%boco(ib)%family = uppercase(umesh%boco(ib)%family)
  call cfd_print("  . linking boundary condition marks"//strof(ib,3)//"/"//trim(strof(umesh%nboco))// &
                    ": "//trim(umesh%boco(ib)%family))

  select case(umesh%boco(ib)%ilocation)
  case(iloc_vtex)
    call cfd_print('      vertex tagging method')
    call seek_bcface_vtex(umesh%boco(ib), umesh)
  case(iloc_elemcell)
    call cfd_print('      cell tagging method')
    call cfd_error('tagging method not yet implemented')
    !call seek_bcface_vtex(ib, umesh%boco(ib), umesh, listface)
  case(iloc_elemface)
    call cfd_print('      face element tagging method')
    call seek_bcface_face(umesh%boco(ib), umesh)
  case(iloc_face)
    call cfd_print('      genuine face tagging method (nothing to do)')
  case default
    call cfd_error("boundary condition links: unknown tagging method ("//trim(strof(umesh%boco(ib)%ilocation))//")")
  endselect

  if (umesh%boco(ib)%nface == 0) &
  call cfd_print("    > no marked face has been found, skipping boundary condition mark section...")

enddo

! --- Check empty boco marks ---

nf  = 0
iib = 0

do ib = 1, umesh%nboco
  if (umesh%boco(ib)%nface /= 0) then
    iib = iib + 1
    nf  = nf + umesh%boco(ib)%nface
    if (iib < ib) umesh%boco(iib) = umesh%boco(ib)   ! if staggered, then transfer
  else
    call delete_ustboco(umesh%boco(ib))
  endif
enddo

umesh%nboco = iib

nf = 0
do ib = 1, umesh%nboco
  nf = nf + umesh%boco(ib)%nface
enddo

! --- check bounding faces == tagged faces ---

if (nf /= umesh%nface_lim) &
  call cfd_error("number of tagged faces does not correspond to number of boundaring faces")

!-------------------------
endsubroutine create_ustboco
!------------------------------------------------------------------------------!
! Changes history
!
! fev  2003: creation
! june 2004: construction des connectivites BOCO-> faces, generalisation
!            procedure intrinseque transferee dans USTMESH
! Nov  2007: check empty boco marks
! Dec  2010: TYPHON(cgns2typhon_ustboco)->CFDTOOLS(create_ustboco)
!------------------------------------------------------------------------------!

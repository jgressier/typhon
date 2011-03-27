!------------------------------------------------------------------------------!
! Procedure : calc_ns_timestep   
!
! Fonction
!   Compute local time step for Euler/NS solver
!
!------------------------------------------------------------------------------!
subroutine calc_ns_timestep(cfl, fluid, umesh, field, dtloc, ncell)

use OUTPUT
use PACKET
use MENU_NUM
use DEFFIELD
use USTMESH
use EQNS

implicit none

! -- INPUTS --
real(krp)         :: cfl           ! CFL number
type(st_espece)   :: fluid         ! donnees du fluide
type(st_ustmesh)  :: umesh         ! donnees geometriques
type(st_field)    :: field         ! donnees champs
integer           :: ncell         ! nombre de cellules internes (taille de dtloc)

! -- OUTPUTS --
real(krp), dimension(1:ncell) :: dtloc    ! tableau de pas de temps local

! -- Private DATA --
integer   :: if, ic, nbadcell, ib
real(krp) :: gg1
real(krp) :: a2(cell_buffer), rv2(cell_buffer), irho(cell_buffer)
integer, pointer      :: ifsta(:), ifend(:)     ! starting and ending index
integer, pointer      :: icsta(:), icend(:)     ! starting and ending index
integer               :: fbuf, nfblock          ! buffer size for face
integer               :: cbuf, ncblock          ! buffer size 

! ------------------------------ BODY ------------------------------

! Euler timestep is 

! -- Calcul de somme S_i --
! pour faire la somme des surfaces des faces, on boucle d'abord sur les faces
! internes pour les contributions aux deux cellules voisines, puis on boucle
! sur les faces limites pour uniquement ajouter la contribution aux cellules 
! internes

! initialisation avant somme des faces

!$OMP PARALLEL DO
do ic = 1, ncell
  dtloc(ic) = 0._krp
enddo
!$OMP END PARALLEL DO

! somme des surfaces de faces internes sur chaque cellule (boucle sur faces)

!!$OMP PARALLEL DO private(ic)
do if = 1, umesh%nface_int
  ic  = umesh%facecell%fils(if,1)
  !!$OMP ATOMIC
  dtloc(ic) = dtloc(ic) + umesh%mesh%iface(if,1,1)%surface 
  ic  = umesh%facecell%fils(if,2)
  !!$OMP ATOMIC
  dtloc(ic) = dtloc(ic) + umesh%mesh%iface(if,1,1)%surface
enddo
!!$OMP END PARALLEL DO

! somme des surfaces de faces limites sur chaque cellule (boucle sur faces)

!!$OMP PARALLEL DO private(ic)
do if = umesh%nface_int+1, umesh%nface
  ic  = umesh%facecell%fils(if,1)
  !!$OMP ATOMIC
  dtloc(ic) = dtloc(ic) + umesh%mesh%iface(if,1,1)%surface
enddo
!!$OMP END PARALLEL DO

! -- Calcul de V / somme_i S_i et prise en compte du nombre de CFL --

call new_buf_index(ncell, cell_buffer, ncblock, icsta, icend)

!$OMP PARALLEL DO
do ic = 1, ncell
  dtloc(ic) =  cfl * 2._krp * umesh%mesh%volume(ic,1,1) / dtloc(ic)
enddo
!$OMP END PARALLEL DO

gg1 = fluid%gamma*(fluid%gamma -1._krp)

!$OMP PARALLEL DO private(ic, rv2, a2, irho, cbuf, nbadcell, str_w) shared(dtloc)
do ib = 1, ncblock
  cbuf = icend(ib)-icsta(ib)+1
  irho(1:cbuf) = 1._krp/field%etatcons%tabscal(1)%scal(icsta(ib):icend(ib))
  rv2 (1:cbuf) = sqrabs(field%etatcons%tabvect(1)%vect(icsta(ib):icend(ib)))
  a2  (1:cbuf) = (field%etatcons%tabscal(2)%scal(icsta(ib):icend(ib))-.5_krp*rv2(1:cbuf)*irho(1:cbuf))*gg1*irho(1:cbuf)
  
  nbadcell = count(.not.(a2(1:cbuf) > 0._krp))
  if (nbadcell >= 1) then    ! should get NaN as well
    do ic = 1, cbuf
      if (.not.(a2(ic) > 0._krp)) then
        write(str_w,'(a,i8,a,3e10.2,a)') " <cell",icsta(ib)+ic-1,",", umesh%mesh%centre(icsta(ib)+ic-1,1,1),">"
        call print_info(20, str_w)
      endif
    enddo
    write(str_w,'(a,3e10.2,a)') "at least "//strof(nbadcell)//" negative or NaN internal energy (",minval(a2(1:cbuf)),"m²/s²"
    call print_info(20, str_w)
    call error_stop(trim(str_w))
  endif

  dtloc(icsta(ib):icend(ib)) = dtloc(icsta(ib):icend(ib)) / (sqrt(rv2(1:cbuf))*irho(1:cbuf)+sqrt(a2(1:cbuf)))
enddo
!$OMP END PARALLEL DO

deallocate(icsta, icend)

! dans le cas de pas de temps global, le pas de temps minimum est calcule et impose
! dans la routine appelante

endsubroutine calc_ns_timestep
!------------------------------------------------------------------------------!
! Changes history
!
! July 2004 : creation, calcul par CFL
! Aug  2005 : use direct CFL number (computed before)
!------------------------------------------------------------------------------!


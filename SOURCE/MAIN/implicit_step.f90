!!------------------------------------------------------------------------------!
! Procedure : implicit_step               Auteur : J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf historique)
!   Integration implicit de domaine
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine implicit_step(dt, typtemps, defsolver, defspat, deftime, &
                         umesh, field, coupling, ncp)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use SPARSE_MAT
use MENU_ZONECOUPLING

implicit none

! -- Declaration des entrees --
real(krp)        :: dt         ! pas de temps CFL
character        :: typtemps   ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver  ! type d'equation a resoudre
type(mnu_spat)   :: defspat    ! parametres d'integration spatiale
type(mnu_time)   :: deftime    ! parametres d'integration spatiale
type(st_ustmesh) :: umesh      ! domaine non structure a integrer
integer          :: ncp        ! nombre de couplages de la zone

! -- Declaration des entrees/sorties --
type(st_field)   :: field            ! champ des valeurs et residus
type(mnu_zonecoupling), dimension(1:ncp) &
                 :: coupling ! donnees de couplage

! -- Declaration des variables internes --
type(st_dlu)          :: mat
type(st_genericfield) :: flux             ! tableaux des flux
real(krp), dimension(:), allocatable &
                      :: jacL, jacR       ! tableaux de jacobiennes des flux
integer(kip)          :: if, ic1, ic2, ic, info

! -- Debut de la procedure --

allocate(jacL(umesh%nface))
allocate(jacR(umesh%nface))

!--------------------------------------------------
! phase explicite : calcul du second membre
!--------------------------------------------------

! -- allocation des flux et termes sources --

call new(flux, umesh%nface, field%nscal, field%nvect, 0)

select case(defsolver%typ_solver)
case(solKDIF)
  call integration_kdif_ust(dt, defsolver, defspat, umesh, field, flux, .true., jacL, jacR)
case default
  call erreur("incoherence interne (implicit_step)", "solveur inconnu")
endselect

! -- flux surfaciques -> flux de surfaces et calcul des residus  --

call flux_to_res(dt, umesh, flux, field%residu, .true., jacL, jacR)


!--------------------------------------------------
! phase implicite 
!--------------------------------------------------

! DEV : on ne devrait allouer que les faces internes

call new(mat, umesh%ncell_int, umesh%nface)  ! allocation et initialisation
mat%couple%fils(1:mat%ncouple, 1:2) = umesh%facecell%fils(1:mat%ncouple, 1:2) 

! -- suppression de l'influence des cellules limites --
! l'eventuelle dependance de la cellule gauche via la cellule limite
! doit deja etre dans jacL

do if = 1, mat%ncouple
  if (mat%couple%fils(if,2) > mat%dim) jacR(if) = 0._krp
enddo

! construction de la matrice

do if = 1, mat%ncouple
  ic1 = mat%couple%fils(if,1)    
  ic2 = mat%couple%fils(if,2)    
  ! bilan cellule a gauche de la face
  mat%diag(ic1) = mat%diag(ic1) + jacL(if)
  mat%upper(if) = + jacR(if)    ! ic1 cell is supposed to the lowest index
  ! bilan cellule a droite de la face
  if (ic2 <= mat%dim) then
    mat%diag(ic2) = mat%diag(ic2) - jacR(if)
    mat%lower(if) = - jacL(if)  ! ic2 cell is supposed to the highest index
  endif
enddo

do ic = 1, mat%dim
  mat%diag(ic) = mat%diag(ic) + umesh%mesh%volume(ic,1,1) / dt
  !mat%diag(ic) = umesh%mesh%volume(ic,1,1) / dt
enddo

deallocate(jacL, jacR)

! resolution

select case(deftime%implicite%methode)
case(alg_lu)
  call dlu_lu(mat, field%residu%tabscal(1)%scal, field%residu%tabscal(1)%scal)

case(alg_jac)
  call dlu_jacobi(deftime%implicite, mat, field%residu%tabscal(1)%scal, &
                  field%residu%tabscal(1)%scal, info)
  if (info < 0) call print_warning("methode d'inversion JACOBI non convergee")

case(alg_gs)
  call erreur("developpement","Methode Gauss-Seidel non implementee")

case(alg_sor)
  call erreur("developpement","Methode SOR non implementee")
  
case default
  call erreur("incoherence","methode d'inversion inconnue")
endselect

call delete(mat)

!--------------------------------------------------

!select case(typtemps)
! case(instationnaire) ! corrections de flux seulement en instationnaire

! ! Calcul de l'"energie" a l'interface, en vue de la correction de flux, pour 
! ! le couplage avec echanges espaces
! !DVT : flux%tabscal(1) !
! if (ncp>0) then
!   call accumulfluxcorr(dt, defsolver, umesh%nboco, umesh%boco, &
!                        umesh%nface, flux%tabscal(1)%scal, ncp, &
!                        coupling)
! endif

!endselect

if (ncp > 0) call erreur("Developpement","couplage interdit en implicite")

call delete(flux)


endsubroutine implicit_step

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2004 : creation de la procedure
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : integration_zone            Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  : (cf historique)
!   Integration de tous les domaines d'une zone sur un pas de temps correspondant 
!   a une iteration
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integration_zone(dt, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE

implicit none

! -- Declaration des entrees --
real(krp)     :: dt              ! pas de temps propre a la zone
type(st_zone) :: zone            ! zone a integrer

! -- Declaration des sorties --
! retour des residus a travers le champ field de la structure zone

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid
integer                :: if

! -- Debut de la procedure --

! -- Preparation du calcul --

pgrid => zone%grid
do while (associated(pgrid))
  call calc_varprim(zone%defsolver, pgrid%info%field_loc)     ! calcul des var. primitives
  pgrid => pgrid%next
enddo

! -- calcul des conditions aux limites pour tous les domaines --

call conditions_limites(zone)
    
! on ne calcule les gradients que dans les cas necessaires

if (zone%defspat%calc_grad) then
  pgrid => zone%grid
  do while (associated(pgrid))
    call calc_gradient(zone%defsolver, pgrid,                 &
                       pgrid%info%field_loc%etatprim, pgrid%info%field_loc%gradient)
    call calc_gradient_limite(zone%defsolver, pgrid%umesh, pgrid%info%field_loc%gradient)
    pgrid => pgrid%next
  enddo
endif

! -- integration des domaines --

pgrid => zone%grid
do while (associated(pgrid))
  ! DEV : changer les structures de couplages dans MGRID
  call integration_grid(dt, zone%info%typ_temps,                    &
                        zone%defsolver, zone%defspat, zone%deftime, &
                        pgrid, zone%coupling, zone%ncoupling)

  ! Desallocation des eventuelles listes chainees de champ generique utilisees
  if (pgrid%nbocofield .ne. 0) then
    call delete_chainedgfield(pgrid%bocofield)
    pgrid%nbocofield=0
  endif

  pgrid => pgrid%next
enddo



!-----------------------------
endsubroutine integration_zone

!------------------------------------------------------------------------------!
! Historique des modifications
!
! août 2002 : creation de la procedure
! juil 2003 : modification arguments integration_ustdomaine
! oct  2003  : modification arguments integration_ustdomaine : ajout typ_temps
! oct  2003 : insertion des procedures de calcul var. primitives et gradients
!             (calcul des conditions aux limites avant calcul de gradients)
!             ajout du calcul des gradients aux limites (set_gradient_limite)
! avr  2004 : traitement des listes chainees de structures MGRID
!             changement d'appel integration_ustdomaine -> integration_grid
! juin 2004 : desallocation des listes chainees de champ generique en fin de procedure
! oct  2004 : field chained list
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : readcgns_strconnect                Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Lecture des de la connectivite de tous les elements
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine readcgns_strconnect(unit, ib, iz, zone, nmax_elem)                 

use CGNSLIB       ! definition des mots-clefs
use CGNS_STRUCT   ! Definition des structures CGNS
use OUTPUT        ! Sorties standard TYPHON

implicit none

! -- Entrees --
integer             :: unit       ! numero d'unite pour la lecture
integer             :: ib, iz     ! numero de base et de zone
integer             :: nmax_elem  ! nombre total d'elements

! -- Sorties --
type(st_cgns_zone)  :: zone       ! connectivite cellule->sommets
                                  !              faces  ->sommets
                                  ! dans la structure zone

! -- Variables internes --                                        
integer             :: ier        ! code erreur
integer             :: ifam, nfam ! indice de famille et nombre total de familles
integer             :: ideb, ifin ! indice des cellules repertoriees dans la section
integer             :: itype      ! type d'element
integer             :: nbd, ip    ! entiers non utilises en sortie

! -- Debut de procedure

write(str_w,'(a,i8,a)') "lecture des connectivites du maillage non structure :",nmax_elem,"elements"
call print_info(5, adjustl(str_w))

! --- Lecture du nombre de sections ---
! (les cellules sont regroupees par section selon leur type)

call cg_nsections_f(unit, ib, iz, nfam, ier)
if (ier /= 0)   call erreur("Lecture CGNS","Probleme a la lecture du nombre de sections")

! --- Lecture des types de section et affectation aux familles ---




!------------------------------
endsubroutine readcgns_strconnect

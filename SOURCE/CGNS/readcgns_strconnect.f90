!------------------------------------------------------------------------------!
! Procedure : readcgns_strconnect                Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Lecture des de la connectivité de tous les élements
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine readcgns_strconnect(unit, ib, iz, zone, nmax_elem)                 

use CGNSLIB       ! définition des mots-clefs
use CGNS_STRUCT   ! Définition des structures CGNS
use OUTPUT        ! Sorties standard TYPHON

implicit none

! -- Entrées --
integer             :: unit       ! numéro d'unité pour la lecture
integer             :: ib, iz     ! numéro de base et de zone
integer             :: nmax_elem  ! nombre total d'éléments

! -- Sorties --
type(st_cgns_zone)  :: zone       ! connectivité cellule->sommets
                                  !              faces  ->sommets
                                  ! dans la structure zone

! -- Variables internes --                                        
integer             :: ier        ! code erreur
integer             :: ifam, nfam ! indice de famille et nombre total de familles
integer             :: ideb, ifin ! indice des cellules répertoriées dans la section
integer             :: itype      ! type d'élément
integer             :: nbd, ip    ! entiers non utilisés en sortie

! -- Début de procédure

write(str_w,'(a,i8,a)') "lecture des connectivités du maillage non structuré :",nmax_elem,"éléments"
call print_info(5, adjustl(str_w))

! --- Lecture du nombre de sections ---
! (les cellules sont regroupées par section selon leur type)

call cg_nsections_f(unit, ib, iz, nfam, ier)
if (ier /= 0)   call erreur("Lecture CGNS","Problème à la lecture du nombre de sections")

! --- Lecture des types de section et affectation aux familles ---




!------------------------------
endsubroutine readcgns_strconnect

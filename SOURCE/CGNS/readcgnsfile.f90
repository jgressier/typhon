!------------------------------------------------------------------------------!
! Procedure : readcgnsfile                Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Lecture d'un fichier CGNS
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine readcgnsfile(unit, nom, world) 

use CGNSLIB       ! definition des mots-clefs
use CGNS_STRUCT   ! Definition des structures CGNS
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrees --
integer             :: unit       ! numero d'unite pour la lecture
character(len=*)    :: nom

! -- Sorties --
type(st_cgns_world) :: world      ! structure des donnees CGNS

! -- Variables internes --
integer       :: ier              ! code d'erreur
integer       :: i                ! indice courant

! -- Debut de procedure
   
! --- Lecture du nom de fichier ---
   
! --- Ouverture du fichier ---

call print_info(5, "* LECTURE DU MAILLAGE CGNS : "//trim(nom))

call cg_open_f(trim(nom), MODE_READ, unit, ier)
call print_info(8, "Ouverture du fichier "//trim(nom))

if (ier /= 0) call erreur("Lecture CGNS","Probleme a l'ouverture du fichier CGNS")
   
! --- Lecture du nombre de bases ---

call cg_nbases_f(unit, world%nbase, ier)

write(str_w,'(a,i2,a,a)') ".",world%nbase,"base(s) dans le fichier ",trim(nom)
call print_info(8, adjustl(str_w))

if (ier /= 0) call erreur("Lecture CGNS","Probleme a la lecture du nombre de bases")

! --- Allocation et Lecture des bases ---

allocate(world%base(world%nbase))

do i = 1, world%nbase
  call readcgnsbase(unit, i, world%base(i))
enddo


! --- fermeture du fichier ---

call cg_close_f(unit, ier)
call print_info(8, "Fermeture du fichier "//trim(nom))



!-------------------------
endsubroutine readcgnsfile

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

use CGNSLIB       ! définition des mots-clefs
use CGNS_STRUCT   ! Définition des structures CGNS
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrées --
integer             :: unit       ! numéro d'unité pour la lecture
character(len=*)    :: nom

! -- Sorties --
type(st_cgns_world) :: world      ! structure des données CGNS

! -- Variables internes --
integer       :: ier              ! code d'erreur
integer       :: i                ! indice courant

! -- Début de procédure
   
! --- Lecture du nom de fichier ---
   
! --- Ouverture du fichier ---

call print_info(5, "* LECTURE DU MAILLAGE CGNS : "//trim(nom))

call cg_open_f(trim(nom), MODE_READ, unit, ier)
call print_info(8, "Ouverture du fichier "//trim(nom))

if (ier /= 0) call erreur("Lecture CGNS","Problème à l'ouverture du fichier CGNS")
   
! --- Lecture du nombre de bases ---

call cg_nbases_f(unit, world%nbase, ier)

write(str_w,'(a,i2,a,a)') ".",world%nbase,"base(s) dans le fichier ",trim(nom)
call print_info(8, adjustl(str_w))

if (ier /= 0) call erreur("Lecture CGNS","Problème à la lecture du nombre de bases")

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

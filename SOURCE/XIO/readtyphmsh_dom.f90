!------------------------------------------------------------------------------!
! Procedure : readtyphmsh_dom             Auteur : J. Gressier
!                                         Date   : Fevrier 2004
! Fonction                                Modif  : (cf historique)
!   Lecture d'un fichier de maillage TYPHMSH - lecture d'une domaine
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine readtyphmsh_dom(unit, umesh, typ_geo)

use DEFZONE       ! structure ZONE
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrées --
integer      :: unit       ! numéro d'unité pour la lecture
character    :: typ_geo    ! type de géométrie

! -- Sorties --
type(st_ustmesh) :: umesh      ! structure maillage non structuré

! -- Variables internes --
integer               :: ier             ! code d'erreur
integer               :: i               ! indice courant
character(len=60)     :: typ_dom, str    ! chaînes
character(len=strlen) :: nom             ! chaînes

! -- Début de procédure
   

! -- type de domaine --

umesh%mesh%info%geom = typ_geo

!! DEV : lecture de fichier avec commentaires

read(unit,*) typ_dom

if (samestring(typ_dom,"CLOSEDCRV")) then

  read(unit,*) nom            ! nom du domaine
  read(unit,*) umesh%nface    ! nombre de faces 

  ! dans ce type de maillage, les éléments sont directements les faces
  ! et on établit une connectivité entres faces (cellface)

  umesh%ncell = 0              ! pas de cellule dans le maillage
  umesh%nvtex = umesh%nface+1  ! pas de cellule dans le maillage

  ! allocation des vertex uniquement
  call new(umesh%mesh, 0, 0, umesh%nvtex)

  call print_info(10,"| lecture du maillage de points")

  ! lecture des points
  do i = 1, umesh%nvtex
    read(unit,*) umesh%mesh%vertex(i,1,1)%x, umesh%mesh%vertex(i,1,1)%y
    umesh%mesh%vertex%z = 0._krp
  enddo

  ! connectivité face->vtex
  call new(umesh%facevtex, umesh%nface, 2)
  do i = 1, umesh%nface
    umesh%facevtex%fils(i,1) = i
    umesh%facevtex%fils(i,2) = i+1
  enddo
  
  ! connectivité face->cell (connectivité des faces, deux à deux)
  call new(umesh%facecell, umesh%nface, 2)
  do i = 2, umesh%nvtex-1   ! boucle sur les sommets "internes"
    umesh%facecell%fils(i-1,2) = i
    umesh%facecell%fils(i,  1) = i
  enddo
  ! connectivité particulière de la première et la dernière face
  umesh%facecell%fils(1,1) = umesh%facecell%fils(1,2)  ! échange
  umesh%facecell%fils(1,2)           = 0               ! cellule limite
  umesh%facecell%fils(umesh%nface,2) = 0               ! cellule limite

  umesh%nface_int = umesh%nface - 2
  umesh%nface_lim = 2
  umesh%ncell_int = 0
  umesh%ncell_lim = 0

  ! -- Définition des listes de conditions limites --

  call print_info(10,"| Création des familles de conditions limites")

  call createboco(umesh, 2)

  ! profil

  call new(umesh%boco(1), nom, umesh%nface)
  do i = 1, umesh%nface
    umesh%boco(1)%iface(i) = i
  enddo

  ! kutta

  nom = trim(nom)//"_KT"
  call new(umesh%boco(2), nom, umesh%nface_lim)
  umesh%boco(2)%iface(1) = 0
  umesh%boco(2)%iface(2) = 0
    
else
  call erreur("lecture TYPHMSH","type de domaine inconnu")
endif


!-------------------------
endsubroutine readtyphmsh_dom

!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2004 : création de la procédure
!------------------------------------------------------------------------------!

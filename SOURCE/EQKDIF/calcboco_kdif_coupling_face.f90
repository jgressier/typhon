!------------------------------------------------------------------------------!
! Procedure : calcboco_kdif_coupling_face Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  : 
!   Conditions aux limites de couplage, methode du flux de face, 
!   en diffusion thermique
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_kdif_coupling_face(ustboco, ustdom, champ, condrac)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrees --
type(st_ustboco) :: ustboco           ! lieu d'application des conditions aux limites
type(st_ustmesh) :: ustdom            ! maillage non structure
type(st_genericfield) :: condrac ! stockage des conditions limites de couplage

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des etats

! -- Declaration des variables internes --
integer          :: ifb, if           ! index de liste, face limite
integer          :: ighostface        ! index de cellule fictive

! -- Debut de la procedure --
  do ifb = 1, ustboco%nface 
    if = ustboco%iface(ifb)
    ighostface = ustdom%facecell%fils(if,2) 
    champ%etatprim%tabscal(1)%scal(ighostface) = condrac%tabscal(1)%scal(ifb)
   !champ%etatprim%tabscal(2)%scal(ighostface) = condrac%tabscal(2)%scal(ifb)
  enddo


endsubroutine calcboco_kdif_coupling_face

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): creation de la procedure
!------------------------------------------------------------------------------!

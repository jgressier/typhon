!------------------------------------------------------------------------------!
! Procedure : setboco_kdif_isoth_nunif    Auteur : J. Gressier/E. Radenac
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites non uniformes pour la conduction de la 
!   chaleur
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_kdif_isoth_nunif(ustboco, ustdom, champ, temp)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD 

implicit none

! -- Declaration des entrées --
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: ustdom           ! maillage non structuré
real(krp), dimension(ustboco%nface) &
                 :: temp             ! température condition limite

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des états

! -- Declaration des variables internes --
integer          :: ifb, if, ip      ! index de liste, index de face limite et paramètres
integer          :: ighost    ! index de cellule intérieure, et de cellule fictive

! -- Debut de la procedure --

do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  ighost = ustdom%facecell%fils(if,2)
  do ip = 1, champ%nscal
    champ%etatprim%tabscal(ip)%scal(ighost) = temp(ifb)
  enddo
enddo

endsubroutine setboco_kdif_isoth_nunif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov 2003             : création de la procédure
!------------------------------------------------------------------------------!

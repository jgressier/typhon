!------------------------------------------------------------------------------!
! Procedure : setboco_kdif_isoth          Auteur : J. Gressier/E. Radenac
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites non uniformes pour la conduction de la 
!   chaleur
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_kdif_isoth(unif, ustboco, ustdom, champ, bckdif)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD 

implicit none

! -- Declaration des entrees --
integer            :: unif             ! uniform or not
type(st_ustboco)   :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh)   :: ustdom           ! maillage non structure
type(st_boco_kdif) :: bckdif           ! parameters and temperature (field or constant)

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des etats

! -- Declaration des variables internes --
integer    :: ifb, if, ip      ! index de liste, index de face limite et parametres
integer    :: ighost           ! index de cellule interieure, et de cellule fictive

! -- Debut de la procedure --

if (unif == uniform) then

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    ighost = ustdom%facecell%fils(if,2)
    do ip = 1, champ%nscal
      champ%etatprim%tabscal(ip)%scal(ighost) = bckdif%temp_wall
    enddo
  enddo

else

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    ighost = ustdom%facecell%fils(if,2)
    do ip = 1, champ%nscal
      champ%etatprim%tabscal(ip)%scal(ighost) = bckdif%temp(ifb)
    enddo
  enddo

endif

endsubroutine setboco_kdif_isoth

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2003 : creation de la procedure
! july 2004 : merge of uniform and non-uniform boco settings
!------------------------------------------------------------------------------!

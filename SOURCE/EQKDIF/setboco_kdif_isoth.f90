!------------------------------------------------------------------------------!
! Procedure : setboco_kdif_isoth          Auteur : J. Gressier/E. Radenac
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites non uniformes pour la conduction de la 
!   chaleur
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_kdif_isoth(curtime, unif, ustboco, umesh, champ, bckdif)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD 
use FCT_EVAL
use FCT_ENV

implicit none

! -- INPUTS --
real(krp)          :: curtime          ! current time
integer            :: unif             ! uniform or not
type(st_ustboco)   :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh)   :: umesh            ! unstructured mesh
type(st_boco_kdif) :: bckdif           ! parameters and temperature (field or constant)

! -- OUTPUTS --
type(st_field)   :: champ            ! champ des etats

! -- Internal variables --
integer    :: nface
integer    :: ifb, if, ip      ! index de liste, index de face limite et parametres
integer    :: ighost           ! index de cellule interieure, et de cellule fictive
real(krp)  :: tloc

! -- BODY --

if (unif == uniform) then

  call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    ighost = umesh%facecell%fils(if,2)
    call fct_env_set_real(blank_env, "x", umesh%mesh%iface(if,1,1)%centre%x)
    call fct_env_set_real(blank_env, "y", umesh%mesh%iface(if,1,1)%centre%y)
    call fct_env_set_real(blank_env, "z", umesh%mesh%iface(if,1,1)%centre%z)
    call fct_env_set_real(blank_env, "t", curtime)
    call fct_eval_real(blank_env, bckdif%wall_temp, tloc)
    do ip = 1, champ%nscal
      champ%etatprim%tabscal(ip)%scal(ighost) = tloc
    enddo
  enddo

else

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    ighost = umesh%facecell%fils(if,2)
    do ip = 1, champ%nscal
      champ%etatprim%tabscal(ip)%scal(ighost) = bckdif%temp(ifb)
    enddo
  enddo

endif

endsubroutine setboco_kdif_isoth

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2003 : creation de la procedure
! july 2004 : merge of uniform and non-uniform boco settings
! Mar  2008 : use of FCT function
!------------------------------------------------------------------------------!

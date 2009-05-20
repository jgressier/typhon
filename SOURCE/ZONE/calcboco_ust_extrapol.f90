!------------------------------------------------------------------------------!
! Procedure : calcboco_ust_extrapol       Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin  2003 (cf Historique)
!   Integration d'une zone sur un ecart de temps donne,
!   d'une representation physique uniquement
!
! Defauts/Limitations/Divers :
!   ATTENTION : le calcul des conditions aux limites doit se faire sur les
!     variables primitives
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust_extrapol(defboco, ustboco, umesh, champ)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrees --
type(mnu_boco)   :: defboco          ! parametres de conditions aux limites
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: umesh            ! unstructured mesh

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des etats

! -- Declaration des variables internes --
integer          :: ifb, if, ip      ! index de liste, index de face limite, et parametre
integer          :: icell, ighost    ! index de cellule interieure, et de cellule fictive

! -- Debut de la procedure --

select case(defboco%order_extrap)

case(extrap_quantity)   

  ! --- extrapolation d'ordre 1 ---

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    icell  = umesh%facecell%fils(if,1)
    ighost = umesh%facecell%fils(if,2)
    do ip = 1, champ%nscal
      champ%etatprim%tabscal(ip)%scal(ighost) = champ%etatprim%tabscal(ip)%scal(icell) 
    enddo
    do ip = 1, champ%nvect
      champ%etatprim%tabvect(ip)%vect(ighost) = champ%etatprim%tabvect(ip)%vect(icell) 
    enddo
  enddo
  
case(extrap_gradient)
  call erreur("Developpement","Extrapolation d'ordre 2 non implementee")

endselect


endsubroutine calcboco_ust_extrapol

!------------------------------------------------------------------------------!
! Chnge History
!
! apr  2003 : creation
! june 2003 : update of type of fields
!------------------------------------------------------------------------------!

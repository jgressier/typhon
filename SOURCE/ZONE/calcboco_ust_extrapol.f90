!------------------------------------------------------------------------------!
! Procedure : calcboco_ust_extrapol       Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin  2003 (cf Historique)
!   Intégration d'une zone sur un écart de temps donné,
!   d'une représentation physique uniquement
!
! Defauts/Limitations/Divers :
!   ATTENTION : le calcul des conditions aux limites doit se faire sur les
!     variables primitives
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust_extrapol(defboco, ustboco, ustdom, champ)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrées --
type(mnu_boco)   :: defboco          ! paramètres de conditions aux limites
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: ustdom           ! maillage non structuré

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des états

! -- Declaration des variables internes --
integer          :: ifb, if, ip      ! index de liste, index de face limite, et paramètre
integer          :: icell, ighost    ! index de cellule intérieure, et de cellule fictive

! -- Debut de la procedure --

select case(defboco%order_extrap)

case(extrap_quantity)   

  ! --- extrapolation d'ordre 1 ---

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    icell  = ustdom%facecell%fils(if,1)
    ighost = ustdom%facecell%fils(if,2)
    do ip = 1, champ%nscal
      champ%etatprim%tabscal(ip)%scal(ighost) = champ%etatprim%tabscal(ip)%scal(icell) 
    enddo
    do ip = 1, champ%nvect
      champ%etatprim%tabvect(ip)%vect(ighost) = champ%etatprim%tabvect(ip)%vect(icell) 
    enddo
  enddo
  
case(extrap_gradient)
  call erreur("Développement","Extrapolation d'ordre 2 non implémentée")

endselect


endsubroutine calcboco_ust_extrapol

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 (v0.0.1b) : création de la procédure
! juin  2003           : mise à jour des types de champs
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : corr_varprim_kdif           Auteur : E. Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  :
!   Calcul des variables primitives aux frontières de couplage,
!   tenant compte des corrections de flux, code thermique.
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine corr_varprim_kdif(field, domaine, defkdif, dif_enflux, nb)

use TYPHMAKE
use OUTPUT
use VARCOM
use USTMESH
use MENU_KDIF
use DEFFIELD
use MENU_ZONECOUPLING

implicit none

! -- Declaration des entrées --
type(st_ustmesh)      :: domaine          ! domaine non structuré à intégrer
type(mnu_kdif)        :: defkdif          ! propriétés du matériau
type(st_genericfield) :: dif_enflux       ! énergie à ajouter, pour correction de flux
integer               :: nb               ! index de la condition aux limites

! -- Declaration des entrées/sorties --
type(st_field)   :: field            ! champ des valeurs et résidus

! -- Declaration des variables internes --
integer               :: i, if               ! index de face
integer               :: ic1, ic2         ! index de cellules
integer               :: ip               ! index de variables

! -- Debut de la procedure --

do i=1, domaine%boco(nb)%nface

  if = domaine%boco(nb)%iface(i)

  ic1 = domaine%facecell%fils(if,1)
  ic2 = domaine%facecell%fils(if,2)

  ! -- calcul des résidus --
  do ip = 1, field%nscal
    field%residu%tabscal(ip)%scal(ic1) = - dif_enflux%tabscal(2)%scal(i)
    field%residu%tabscal(ip)%scal(ic2) = dif_enflux%tabscal(2)%scal(i)
  enddo
  do ip = 1, field%nvect
    field%residu%tabvect(ip)%vect(ic1) = - dif_enflux%tabvect(2)%vect(i)
    field%residu%tabvect(ip)%vect(ic2) = dif_enflux%tabvect(2)%vect(i)
  enddo

  do ip = 1, field%nscal
    field%residu%tabscal(ip)%scal(ic1) =  field%residu%tabscal(ip)%scal(ic1) &
                                            / domaine%mesh%volume(ic1,1,1)
  enddo
  do ip = 1, field%nvect
    field%residu%tabvect(ip)%vect(ic1) =  field%residu%tabvect(ip)%vect(ic1)  &
                                            / domaine%mesh%volume(ic1,1,1)
  enddo

  ! -- calcul des variables conservatives et primitives --
  do ip = 1, field%nscal
    field%etatcons%tabscal(ip)%scal(ic1) = field%etatcons%tabscal(ip)%scal(ic1) &
                                          + field%residu%tabscal(ip)%scal(ic1)
    field%etatprim%tabscal(ip)%scal(ic1) = field%etatcons%tabscal(ip)%scal(ic1) &
                                          / defkdif%materiau%Cp
  enddo

  do ip= 1, field%nvect
    field%etatcons%tabvect(ip)%vect(ic1) = field%etatcons%tabvect(ip)%vect(ic1) &
                                          + field%residu%tabvect(ip)%vect(ic1)
    field%etatprim%tabvect(ip)%vect(ic1) = field%etatcons%tabvect(ip)%vect(ic1) &
                                          / defkdif%materiau%Cp
  enddo

enddo

endsubroutine corr_varprim_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!

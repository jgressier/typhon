!------------------------------------------------------------------------------!
! Procedure : setboco_kdif_hconv          Auteur : J. Gressier/E. Radenac
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites non uniformes pour la conduction de la 
!   chaleur
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_kdif_hconv(unif, ustboco, ustdom, champ, flux, defsolver, bckdif)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_BOCO
use USTMESH
use DEFFIELD 

implicit none

! -- Declaration des entrées --
integer            :: unif             ! uniform or not
type(st_ustboco)   :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh)   :: ustdom           ! maillage non structuré
type(mnu_solver)   :: defsolver        ! type d'équation à résoudre
type(st_boco_kdif) :: bckdif           ! parameters and fluxes (field or constant)

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des états
real(krp), dimension(ustboco%nface) &
                 :: flux             ! flux

! -- Declaration des variables internes --
integer          :: ifb, if, ip      ! index de liste, index de face limite et paramètres
integer          :: ic, ighost    ! index de cellule intérieure, et de cellule fictive
type(v3d)        :: cgface, cg, normale ! centre de face, de cellule, normale face
real(krp)        :: d             ! distance cellule - face limite
real(krp)        :: conduct       ! conductivité

! -- Debut de la procedure --

if (unif == uniform) then
!-----------------------------------------
! cas uniforme
!-----------------------------------------
do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  ic     = ustdom%facecell%fils(if,1)
  ighost = ustdom%facecell%fils(if,2)

  ! Calcul "distance pondérée" centre de cellule - centre face
  cgface = ustdom%mesh%iface(if,1,1)%centre
  cg     = ustdom%mesh%centre(ic,1,1)
  normale= ustdom%mesh%iface(if,1,1)%normale
  d    = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))

  ! Calcul conductivité
  conduct = valeur_loi(defsolver%defkdif%materiau%Kd, champ%etatprim%tabscal(1)%scal(ic))

  ! Calcul approché de la température du point fictif pour calcul des gradients
  do ip = 1, champ%nscal
    champ%etatprim%tabscal(ip)%scal(ighost) = & 
      ( (conduct/d) * champ%etatprim%tabscal(ip)%scal(ic) + bckdif%h_conv*bckdif%temp_conv ) &
      / (conduct/d+bckdif%h_conv)
  enddo

  ! Calcul du flux
  flux(ifb) = bckdif%h_conv*(champ%etatprim%tabscal(1)%scal(ighost) - bckdif%temp_conv)

enddo

else

!-----------------------------------------
! cas non uniforme
!-----------------------------------------
do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  ic     = ustdom%facecell%fils(if,1)
  ighost = ustdom%facecell%fils(if,2)

  ! Calcul "distance pondérée" centre de cellule - centre face
  cgface = ustdom%mesh%iface(if,1,1)%centre
  cg     = ustdom%mesh%centre(ic,1,1)
  normale= ustdom%mesh%iface(if,1,1)%normale
  d    = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))

  ! Calcul conductivité
  conduct = valeur_loi(defsolver%defkdif%materiau%Kd, champ%etatprim%tabscal(1)%scal(ic))

  ! Calcul approché de la température du point fictif pour calcul des gradients
  do ip = 1, champ%nscal
    champ%etatprim%tabscal(ip)%scal(ighost) = ( (conduct/d) * &
      champ%etatprim%tabscal(ip)%scal(ic) + bckdif%tconv_nunif(ifb)*bckdif%h_nunif(ifb) ) &
      / (conduct/d+bckdif%tconv_nunif(ifb))
  enddo

  ! Calcul du flux
  flux(ifb) = bckdif%tconv_nunif(ifb)*(champ%etatprim%tabscal(1)%scal(ighost) &
                                       - bckdif%h_nunif(ifb))
enddo

endif

endsubroutine setboco_kdif_hconv

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2004 : création de la procédure
! july 2004 : merge of uniform and non-uniform boco settings
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! Procedure : calc_fourier                Auteur : E. Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  : 
!   Calcul du nombre de Fourier d'une zone : on le calcule sur toutes
!   les cellules et on prend le maximum
! Defauts/Limitations/Divers : maillage régulier
!
!------------------------------------------------------------------------------!
subroutine calc_fourier(fourier, dt, ust_mesh, material, etatprim)

use TYPHMAKE
use OUTPUT
use DEFFIELD
use GEO3D
use MATER_LOI
use MATERIAU
use USTMESH

implicit none

! -- Declaration des entrées --
real(krp)         :: dt                 ! pas de temps de la zone
type(st_ustmesh)  :: ust_mesh
type(st_materiau) :: material
type(st_scafield) :: etatprim

! -- Declaration des entrées/sorties --
real(krp)   :: fourier

! -- Declaration des variables internes --
integer     :: i, ic1, ic2
real(krp)   :: f, dist, conduct
type(v3d)   :: dcg

! -- Debut de la procedure --

! Initialisation du nombre de Fourier à 0
fourier = 0

! On itère sur l'ensemble des faces du domaine
! Sur chaque face, on calcule le nb de Fourier de chacune des deux cellules
! "attachées" à la face.
! On prend le maximum. 
do i = 1, ust_mesh%nface

! Numéro des deux cellules associées à la face i

ic1 = ust_mesh%facecell%fils(i,1)
ic2 = ust_mesh%facecell%fils(i,2)

! Le calcul s'effectue sur des cellules de volume non nul (pas sur les 
! "ghostcells")
! Calcul sur la première cellule
if(ust_mesh%mesh%volume(ic1,1,1) > 0) then
  
  conduct = valeur_loi(material%Kd,etatprim%scal(ic1))
  dcg = ust_mesh%mesh%centre(ic1,1,1) - ust_mesh%mesh%iface(i,1,1)%centre
  dist = 2*abs(dcg)

  f = conduct * dt/(material%Cp*dist**2)

  if (f>fourier) then
    fourier = f
  endif
endif

! Calcul sur la deuxième cellule
if(ust_mesh%mesh%volume(ic2,1,1) > 0) then

  conduct = valeur_loi(material%Kd,etatprim%scal(ic2))
  dcg = ust_mesh%mesh%centre(ic1,1,1) - ust_mesh%mesh%iface(i,1,1)%centre
  dist = 2*abs(dcg)

  f = conduct * dt/(material%Cp*dist**2)

  if (f>fourier) then
    fourier = f
  endif
endif

enddo


!Autre possibilité :
!dcg = lzone%ust_mesh%mesh%centre(ic2,1,1) - lzone%ust_mesh%mesh%centre(ic1,1,1)
!dist = abs(dcg)
!f = conduct * lzone%ust_mesh%mesh%iface(i,1,1)%surface &
!         * dt/ (lzone%defsolver%defkdif%materiau%Cp * lzone%ust_mesh%mesh%volume(ic1,1,1) &
!         *dist)

endsubroutine calc_fourier

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet  2003(v0.0.1b) : création de la procédure
!------------------------------------------------------------------------------!

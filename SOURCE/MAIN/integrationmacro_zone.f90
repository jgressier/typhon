!------------------------------------------------------------------------------!
! Procedure : integrationmacro_zone       Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : Juillet 2003 (cf Historique)
!   Intégration d'une zone sur un écart de temps donné,
!   d'une représentation physique uniquement
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integrationmacro_zone(mdt, lzone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use DEFFIELD
use GEO3D
use MATER_LOI

implicit none

! -- Declaration des entrées --
real(krp)     :: mdt              ! pas de temps macro (sens physique)
type(st_zone) :: lzone            ! zone à intégrer

! -- Declaration des sorties --

! -- Declaration des variables internes --
real(krp)   :: local_t            ! temps local (0 à mdt)
real(krp)   :: dt                 ! pas de temps de la zone
integer     :: if                 ! index de champ

!DVT : Nb de Fourier
integer     :: i, ic1, ic2
real(krp)   :: fourier, f, dist, conduct
type(v3d)   :: dcg
! -- Debut de la procedure --
local_t = 0._krp

!---------------------------------------------------------------------------------------------------------------------
! DVT : Nb de Fourier de la zone pour un maillage régulier
!---------------------------------------------------------------------------------------------------------------------
fourier = 0

do i = 1, lzone%ust_mesh%nface

ic1 = lzone%ust_mesh%facecell%fils(i,1)
ic2 = lzone%ust_mesh%facecell%fils(i,2)
!dcg = lzone%ust_mesh%mesh%centre(ic2,1,1) - lzone%ust_mesh%mesh%centre(ic1,1,1)
!dist = abs(dcg)

if(lzone%ust_mesh%mesh%volume(ic1,1,1) > 0) then
  conduct = valeur_loi(lzone%defsolver%defkdif%materiau%Kd,lzone%field(1)%etatprim%tabscal(1)%scal(ic1))
  dcg = lzone%ust_mesh%mesh%centre(ic1,1,1) - lzone%ust_mesh%mesh%iface(i,1,1)%centre
  dist = 2*abs(dcg)
  !f = conduct * lzone%ust_mesh%mesh%iface(i,1,1)%surface &
  !         * mdt/ (lzone%defsolver%defkdif%materiau%Cp * lzone%ust_mesh%mesh%volume(ic1,1,1) &
  !         *dist)
  f = conduct * mdt/(lzone%defsolver%defkdif%materiau%Cp*dist**2)
  if (f>fourier) then
    fourier = f
  endif
endif

if(lzone%ust_mesh%mesh%volume(ic2,1,1) > 0) then
  conduct = valeur_loi(lzone%defsolver%defkdif%materiau%Kd,lzone%field(1)%etatprim%tabscal(1)%scal(ic2))
  dcg = lzone%ust_mesh%mesh%centre(ic1,1,1) - lzone%ust_mesh%mesh%iface(i,1,1)%centre
  dist = 2*abs(dcg)
  !f = conduct * lzone%ust_mesh%mesh%iface(i,1,1)%surface &
  !         * mdt/ (lzone%defsolver%defkdif%materiau%Cp * lzone%ust_mesh%mesh%volume(ic1,1,1) &
  !         *dist)
  f = conduct * mdt/(lzone%defsolver%defkdif%materiau%Cp*dist**2)
  if (f>fourier) then
    fourier = f
  endif
endif

enddo

write(str_w,'(a,i,a,g10.4)') "* FOURIER zone ", lzone%id, " : ", fourier
call print_info(6, str_w)   
                           
!-----------------------------------------------------------------------------------------------------------------------

! allocation des champs de résidus
!print*, "DEBUG INTEGRATIONMACRO_ZONE"
do if = 1, lzone%ndom
  call alloc_res(lzone%field(if))
enddo

do while (local_t < mdt)
  
  ! On peut ici coder différentes méthodes d'intégration (RK, temps dual...)

  write(str_w,'(a,i5,a,g10.4)') "  zone",lzone%id," à t local =",local_t
  call print_info(7,str_w)

  !call calc_zonetimestep(local_t, lzone, dt)
  dt = mdt

  call integration_zone(dt, lzone)

  local_t = local_t + dt

  do if = 1, lzone%ndom
    print*,'!! DEBUG update dom =',if
    call update_champ(lzone%field(if))                   ! màj    des var. conservatives
!    call calc_varprim(lzone%defsolver, lzone%field(if), &
!                      lzone%ust_mesh%ncell_int)  ! calcul des var. primitives
!DVT
    call calc_varprim(lzone%defsolver, lzone%field(if))   ! calcul des var. primitives
  enddo

enddo

call capteurs(lzone)

do if = 1, lzone%ndom
  call dealloc_res(lzone%field(if))
enddo
!print*, "DEBUG : fin dealloc"
endsubroutine integrationmacro_zone

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil  2002 (v0.0.1b) : création de la procédure
! juin  2003           : champs multiples
! juillet 2003         : calcul du nombre de Fourier de la zone
!------------------------------------------------------------------------------!

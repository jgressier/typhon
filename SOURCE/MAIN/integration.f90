!------------------------------------------------------------------------------!
! Procedure : integration                 Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : (cf historique)
!   Integration totale jusqu'au critere d'arret du calcul
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
 
subroutine integration(lworld)

use TYPHMAKE
use STRING
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrees/sorties --
type(st_world) :: lworld

! -- Declaration des entrees --

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid
real(krp)              :: cpu_start, cpu_end
integer, dimension(:), allocatable &
                       :: exchcycle ! indices des cycles d'echange pour les differents couplages de zones
integer                :: ir, izone, if, ib, ic, ierr
integer                :: iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2

! -- BODY --

! initialisation

lworld%info%icycle          = 0
lworld%info%curtps          = 0._krp
lworld%info%residu_ref      = 1._krp
lworld%info%cur_res         = lworld%info%residu_ref
lworld%info%fin_integration = .false.

! Allocation du tableau des indices de cycle d'echange pour les calculs couples
allocate(exchcycle(lworld%prj%ncoupling))
exchcycle(:) = 1 ! initialisation a 1 : 1er echange au 1er cycle, a partir des conditions initiales

! allocation des champs de residus et gradients

do izone = 1, lworld%prj%nzone
  pgrid => lworld%zone(izone)%grid
  do while (associated(pgrid))
    call alloc_res(pgrid%info%field_loc)
    !! DEV : l'allocation ne doit se faire que dans certains cas
    call alloc_grad(pgrid%info%field_loc)
    pgrid => pgrid%next
  enddo
enddo

call cpu_time(cpu_start)

!--------------------------------------------------------
! INTEGRATION
!--------------------------------------------------------
do while (.not. lworld%info%fin_integration)

  lworld%info%icycle = lworld%info%icycle + 1

  ! -- ecriture d'informations en debut de cycle --

  select case(lworld%prj%typ_temps)
  case(stationnaire)
    str_w = "* CYCLE "//strof(lworld%info%icycle)
    !write(str_w,'(a,a)') "* CYCLE ",strof(lworld%info%icycle,3)
  case(instationnaire)
    write(str_w,'(a,i5,a,g10.4)') "* CYCLE", lworld%info%icycle, &
                                  " : t = ",  lworld%info%curtps
  case(periodique)
    write(str_w,'(a,i5)') "* CYCLE", lworld%info%icycle
  endselect

  call print_info(6,str_w)

  ! -- integration d'un cycle --

  call integration_cycle(lworld, exchcycle, lworld%prj%ncoupling)  


  ! -- Actualisation des conditions aux limites au raccord
  do ic = 1, lworld%prj%ncoupling
    call calcul_raccord(lworld, ic, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)
    call update_couplingboco(lworld%zone(iz1), lworld%zone(iz2), nbc1, nbc2, &
                             ncoupl1, lworld%coupling(ic)%boco)
  enddo

  ! -- ecriture d'informations en fin de cycle --

  select case(lworld%prj%typ_temps)

  case(stationnaire)
    write(str_w,'(a,g10.4)') "  Residu de cycle = ", log10(lworld%info%cur_res/lworld%info%residu_ref)
    if (lworld%info%cur_res/lworld%info%residu_ref <= lworld%prj%residumax) then
      lworld%info%fin_integration = .true.
    endif
    if (lworld%info%icycle == lworld%prj%ncycle) then
      lworld%info%fin_integration = .true.
      write(uf_stdout,'(a)') " Nombre de cycles maximal atteint"
      write(uf_log,'(a,g10.4)') "Nombre de cycles maximal atteint, RESIDU = ",&
                           log10(lworld%info%cur_res/lworld%info%residu_ref)
    endif
    call print_info(6,str_w)

  case(instationnaire)
    lworld%info%curtps = lworld%info%curtps + lworld%prj%dtbase
    if (lworld%info%icycle == lworld%prj%ncycle) lworld%info%fin_integration = .true.

  case(periodique)
    lworld%info%fin_integration = .true.

  endselect

  call output_result(lworld, end_cycle)

  open(unit=2001, file="typhon_stop", status="old", iostat=ierr)
  if (ierr == 0) then
    lworld%info%fin_integration = .true.
    call print_info(9, "INTERRUPTING INTEGRATION...")
    close(2001)
  endif

enddo

call cpu_time(cpu_end)

write(str_w, "(a,e13.4)") "CPU   integration time: ", cpu_end-cpu_start 
call print_info(10, str_w)
write(str_w, "(a,e13.4)") "CPU average cycle time: ", (cpu_end-cpu_start)/lworld%info%icycle
call print_info(10, str_w)

! Mise a jour des variables primitives
do izone = 1, lworld%prj%nzone
  call calc_varprim(lworld%zone(izone)%defsolver, lworld%zone(izone)%grid%info%field_loc)
enddo

! Mise a jour des conditions aux limites, notamment de couplage pour l'affichage des donnees :
if (lworld%prj%ncoupling > 0) then
  do ir = 1, lworld%prj%ncoupling
      call calcul_raccord(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)
      call echange_zonedata(lworld,ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2) 
  enddo
endif

do izone = 1, lworld%prj%nzone
 call conditions_limites(lworld%zone(izone))
enddo


!-----------------------------------------------------------------------------------------------------------------------
! DVT : Fermeture du fichier de comparaison des flux a l'interface
!-----------------------------------------------------------------------------------------------------------------------
!if (lworld%prj%ncoupling > 0) then
  close(uf_compflux)
!endif
!-----------------------------------------------------------------------------------------------------------------------

! Desallocation du tableau d'indice de cycle d'echange pour le calcul couple :
deallocate(exchcycle)

do izone = 1, lworld%prj%nzone
 select case(lworld%zone(izone)%defsolver%typ_solver)    ! DEV : en attendant homogeneisation
 case(solKDIF)                                           ! de l'acces des champs dans 
   call dealloc_res(lworld%zone(izone)%grid%info%field_loc)       ! les structures MGRID
   call dealloc_grad(lworld%zone(izone)%grid%info%field_loc)
 case(solVORTEX)
 endselect
enddo

endsubroutine integration

!------------------------------------------------------------------------------!
! Changes history
!
! juil 2002 : creation de la procedure
! juin 2003 : instant d'echange excht
!             mise a jour des CL pour le fichier de sortie
! sept 2003 : gestion du calcul par residus (optionnel) + reorganisation
! oct  2003 : remplacement d'instant d'echange excht par indice de cycle d'echange
!              exchcycle
! avr  2004 : integration des structures MGRID pour tous les solveurs
! oct  2004 : field chained list
! Nov  2005 : add ending cycle output
!------------------------------------------------------------------------------!

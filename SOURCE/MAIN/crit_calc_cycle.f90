!------------------------------------------------------------------------------!
! Procedure : crit_calc_cycle             Auteur : J. Gressier / E. Radenac
!                                         Date   : Octobre 2004
! Fonction                                Modif  : (cf historique)
!   Critère de calcul de cycle
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
 
subroutine crit_calc_cycle(lworld, ncoupling, critere, itc)

use TYPHMAKE
use STRING
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrées/sorties --
logical        :: critere

! -- Declaration des entrées --
type(st_world) :: lworld
integer        :: ncoupling
integer        :: itc        ! intégration du cycle

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid
real(krp)              :: maxDEF, DEF, TLM1, TLM, TLN, dif, maxdif
type(st_scafield)      :: EF1, EF2
integer                :: ir, if, ifb, ighost, ip
integer                :: iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2

! -- Debut de la procedure --

if (itc == 1) then 
  critere = .false.
else
  maxdif = 0
  ip = 1
  do ir = 1, ncoupling
    call calcul_raccord(lworld,ir,iz1,iz2,ncoupl1,ncoupl2,nbc1,nbc2)
    pgrid=>lworld%zone(iz1)%grid

    do if = 1, pgrid%umesh%boco(nbc1)%nface

      TLM1 = lworld%zone(iz1)%defsolver%boco(pgrid%umesh%boco(nbc1)%idefboco)%boco_kdif%temp(if)
      
      ifb = pgrid%umesh%boco(nbc1)%iface(if)
      ighost = pgrid%umesh%facecell%fils(ifb,2)
      TLM  = pgrid%info%field_loc%etatprim%tabscal(ip)%scal(ighost)
!      TLN  = pgrid%info%field_cyclestart%etatprim%tabscal(ip)%scal(ighost)

      dif = abs(TLM1 - TLM)
      if (dif.gt.maxdif) then
        maxdif = dif
      endif

    enddo
  enddo

  print*, "debug CRITERE", maxdif
  if (maxdif .le. lworld%prj%eps_cvloc) then
    critere = .true.
  else
    critere = .false.
  endif

endif

!if (itc == 1) then 
!  critere = .false.
!else
!  maxDEF = 0
!  if (ncoupling > 0) then
!    do ir = 1, ncoupling
!      call calcul_raccord(lworld,ir,iz1,iz2,ncoupl1,ncoupl2,nbc1,nbc2)
!      pgrid=>lworld%zone(iz1)%grid
!      EF2 = lworld%zone(iz2)%coupling(ncoupl2)%zcoupling%etatcons%tabscal(1)
!      EF1 = lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(1)
!      do if = 1, pgrid%umesh%boco(nbc1)%nface
!        DEF = &
!         EF2%scal(lworld%zone(iz2)%coupling(ncoupl2)%zcoupling%connface(if)) +&
!         EF1%scal(if)
!        DEF = abs( DEF )
!        if (DEF.gt.maxDEF) then
!          maxDEF = DEF
!        endif
!      enddo
!    enddo
!  endif
!  print*, "debug CRITERE", maxDEF
!  if (maxDEF .le. lworld%prj%eps_cvloc) then
!    critere = .true.
!  else
!    critere = .false.
!  endif
!endif

!if (itc==10) then
!  critere = .true.
!endif

endsubroutine crit_calc_cycle

!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct 2004 : création de la procédure
!------------------------------------------------------------------------------!

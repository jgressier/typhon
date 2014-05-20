!------------------------------------------------------------------------------
! Procedure : correction                  Auteur : E. Radenac
!                                         Date   : Fevrier 2004
!> @brief Application de la correction cumulee de bilan de flux
!
! Defauts/Limitations/Divers : 
!------------------------------------------------------------------------------
subroutine correction(zone1, zone2, nfacelim, corcoef, nbc1, nbc2, ncoupl1, &
                      ncoupl2, part_cor1, part_cor2, typ_cor1, typ_cor2, &
                      fincycle, dtexch)
 
use OUTPUT
use VARCOM
use DEFZONE
use DEFFIELD
use GEO3D
use TYPHMAKE

implicit none

! -- Declaration des entrees --
integer                    :: nfacelim            ! nombre de faces limites
real(krp), dimension(nfacelim) &
                           :: corcoef   ! coefficient de correction de flux
integer                    :: nbc1, nbc2 ! indice des conditions aux limites 
integer                    :: ncoupl1, ncoupl2 ! numero (identite) du raccord
real(krp)                  :: part_cor1, part_cor2 ! part de la correction a 
                                                ! apporter, dans les deux zones
integer                    :: typ_cor1, typ_cor2 ! type de correction
logical                    :: fincycle
real(krp)                  :: dtexch             ! pas de temps entre 
                                                 ! deux echanges

! -- Declaration des sorties --
type(st_zone)              :: zone1, zone2

! -- Declaration des variables internes --
integer                        :: ifield, i, if

! -- Debut de la procedure --

! Supplement de flux pour ech. espaces : calcul de la difference a appliquer

call calcdifflux(zone1%coupling(ncoupl1)%zcoupling%etatcons%tabscal, &
                  zone2%coupling(ncoupl2)%zcoupling%etatcons%tabscal, &
                  nfacelim, zone1%coupling(ncoupl1)%zcoupling%solvercoupling, &
                  corcoef, zone2%coupling(ncoupl2)%zcoupling%connface )

! Calcul des variables primitives avec correction de flux
! PROVISOIRE : A AMELIORER
! si couplage KDIF / KDIF correction dans les deux domaines avec repartition donnee
! si couplage KDIF / NS correction dans le solide seulement
select case (zone1%coupling(ncoupl1)%zcoupling%solvercoupling)
case(kdif_kdif)

if ( (typ_cor1.ne.bocoT) .and. (typ_cor1.ne.repart_reg) .and. &
     (typ_cor1.ne.repart_geo) .and. (typ_cor1.ne.distributed) )then

  call corr_varprim(zone1%gridlist%first%info%field_loc, &
                    zone1%gridlist%first%umesh, &
                    zone1%defsolver, &
                    zone1%coupling(ncoupl1)%zcoupling%etatcons, nbc1, &
                    part_cor1, typ_cor1, fincycle)

  call corr_varprim(zone2%gridlist%first%info%field_loc, &
                    zone2%gridlist%first%umesh, &
                    zone2%defsolver, &
                    zone2%coupling(ncoupl2)%zcoupling%etatcons, nbc2, &
                    part_cor2, typ_cor2, fincycle)
  endif

case(kdif_ns)
  if ( (typ_cor1.ne.bocoT) .and. (typ_cor1.ne.repart_reg) .and. &
         (typ_cor1.ne.repart_geo) ) then
    if (zone1%defsolver%typ_solver == solKDIF) then
      call corr_varprim(zone1%gridlist%first%info%field_loc, &
                     zone1%gridlist%first%umesh, &
                     zone1%defsolver, &
                     zone1%coupling(ncoupl1)%zcoupling%etatcons, nbc1, &
                     part_cor1, typ_cor1, fincycle)
    else
      call corr_varprim(zone2%gridlist%first%info%field_loc, &
                     zone2%gridlist%first%umesh, &
                     zone2%defsolver, &
                     zone2%coupling(ncoupl2)%zcoupling%etatcons, nbc2, &
                     part_cor2, typ_cor2, fincycle)

    endif
  endif

endselect

if (typ_cor1.eq.distributed) then

  select case(zone1%defsolver%typ_solver)

  case(solKDIF)
    do i=1,nfacelim
      if = zone1%gridlist%first%umesh%boco(nbc1)%iface(i)
      zone1%defsolver%boco(zone1%gridlist%first%umesh%boco(nbc1)%idefboco)%boco_kdif%flux_nunif(if) = part_cor1 * zone1%coupling(ncoupl1)%zcoupling%etatcons%tabscal(2)%scal(i) / (zone1%gridlist%first%umesh%mesh%face_surf(if) * dtexch)
      zone1%coupling(ncoupl1)%zcoupling%etatcons%tabscal(3)%scal(i) = 0
    enddo

  case(solNS)

  case default
    call erreur("Incoherence interne (correction)","type de solveur inconnu")

  endselect

  select case(zone2%defsolver%typ_solver)

  case(solKDIF)
    do i=1,nfacelim
      if = zone1%gridlist%first%umesh%boco(nbc1)%iface(i)
      zone2%defsolver%boco(zone1%gridlist%first%umesh%boco(nbc1)%idefboco)%boco_kdif%flux_nunif(if) = part_cor2 * zone2%coupling(ncoupl2)%zcoupling%etatcons%tabscal(2)%scal(i) / (zone2%gridlist%first%umesh%mesh%face_surf(if) * dtexch)
      zone2%coupling(ncoupl2)%zcoupling%etatcons%tabscal(3)%scal(i) = 0
    enddo

  case(solNS)

  case default
    call erreur("Incoherence interne (correction)","type de solveur inconnu")

  endselect

endif

endsubroutine correction

!------------------------------------------------------------------------------
! Historique des modifications
!
! fev  2004 : creation de la procedure
! avr  2004 : changement de structure (MGRID) : limite a une grille
! oct  2004 : field chained list
!------------------------------------------------------------------------------

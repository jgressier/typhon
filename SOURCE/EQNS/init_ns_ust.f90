!------------------------------------------------------------------------------!
! Procedure : init_ns_ust                 Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Initialization according parameters
!
! Defauts/Limitations/Divers :
!   CAUTION : only initialization of primitive variables
!
!------------------------------------------------------------------------------!
subroutine init_ns_ust(defns, initns, champ, mesh, init_type, initfile, ncell)

use TYPHMAKE
use DEFFIELD
use MENU_NS
use MENU_INIT

implicit none

! -- Declaration des entrees --
type(mnu_ns)     :: defns
type(st_init_ns) :: initns
type(st_mesh)    :: mesh
integer(kpp)     :: init_type
character(len=strlen) :: initfile
integer(kip)     :: ncell

! -- Declaration des sorties --
type(st_field) :: champ

! -- Declaration des variables internes --
integer         :: ip, ic
type(st_nsetat) :: nspri
character(len=50):: charac
real(krp)       :: x,y,z, temp

! -- Debut de la procedure --

select case(init_type)

case(init_def)
  !print*,'init ns ',ncell
  nspri = pi_ti_mach_dir2nspri(defns%properties(1),initns%ptot, initns%ttot, &
                                                 initns%mach, initns%direction) 
  champ%etatprim%tabscal(1)%scal(1:ncell) = nspri%density
  champ%etatprim%tabscal(2)%scal(1:ncell) = nspri%pressure
  champ%etatprim%tabvect(1)%vect(1:ncell) = nspri%velocity

  !!if (champ%allocgrad) champ%gradient(:,:,:,:,:) = 0._krp

case(init_udf)
  print*,"   UDF initialization"
  call udf_ns_init(defns, ncell, mesh%centre(1:ncell, 1, 1), champ%etatprim)
  !print*,champ%etatprim%tabscal(1)%scal(10)
  !print*,champ%etatprim%tabscal(2)%scal(10)
  !print*,champ%etatprim%tabvect(1)%vect(10)

case(init_file)
  print*, initfile
  open(unit=1004, file = initfile, form="formatted")
  read(1004,'(a)') charac
  read(1004,'(a)') charac
  do ic=1, ncell
    read(1004,'(8e18.8)') x, y, z, champ%etatprim%tabvect(1)%vect(ic)%x, &
                 champ%etatprim%tabvect(1)%vect(ic)%y, &
                 champ%etatprim%tabvect(1)%vect(ic)%z, &
                 champ%etatprim%tabscal(2)%scal(ic), temp
    champ%etatprim%tabscal(1)%scal(ic) = champ%etatprim%tabscal(2)%scal(ic) / &
                  ( temp * defns%properties(1)%r_const )
  enddo
  close(1004)

case default
  call erreur("internal error", "unknown initialization method (init_ns_ust)")

endselect

endsubroutine init_ns_ust

!------------------------------------------------------------------------------!
! Modification history
!
! july 2004 : creation & calculation of uniform primitive variables
!------------------------------------------------------------------------------!



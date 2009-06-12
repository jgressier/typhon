!------------------------------------------------------------------------------!
! MODULE : MATFREE
!
! Fonction   
!   Library for matrix-free matrix products on field structures
!
!------------------------------------------------------------------------------!

module MATFREE

use DEFFIELD
use GENFIELD
use MODINFO
use MENU_SOLVER
use MENU_NUM
use MGRID
use MENU_ZONECOUPLING

implicit none

! -- Module global variables ------------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure ST_FIELDINFO : container for field information
!------------------------------------------------------------------------------!

type st_fieldinfo
  type(st_infozone) :: dt              ! 
  type(st_infozone) :: info            ! zone information structure
  type(mnu_solver)  :: defsolver       ! solver parameters
  type(st_gridlist) :: gridlist        ! list of grids
  integer           :: ncoupling       ! number of couplings of zone
  type(mnu_zonecoupling), dimension(:), pointer &
                    :: coupling ! coupling data
endtype st_fieldinfo


! -- INTERFACES -------------------------------------------------------------


! -- Functions and Operators ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! calcrhs_free : unpacks filed_info
!------------------------------------------------------------------------------!
subroutine calcrhs_free(field_info)
implicit none
! -- parameters --
type(st_fieldinfo) :: field_info
! -- internal --

! -- body --

call calc_rhs(field_info%dt, field_info%info, field_info%defsolver, &
              field_info%gridlist, &
              field_info%coupling, field_info%ncoupling)

endsubroutine calcrhs_free


!------------------------------------------------------------------------------!
! yeqax_free : y = A.x
!------------------------------------------------------------------------------!
subroutine yeqax_free(y, x, refres, field, field_info)
implicit none
! -- parameters --
type(st_genericfield) , intent(out) :: y
type(st_genericfield) , intent(in)  :: x,refres
type(st_field)        , intent(in)  :: field
type(st_fieldinfo)    , intent(in)  :: field_info
! -- internal
real(krp) :: coef

! -- body --

coef = 1.0_krp ! compute(coef)

call field_ref2cons(field)
call xeqxpay(field%etatcons, coef, x)
call calcrhs_free(field_info)
call transfer_gfield(field%residu, y)
call xeqxpay(y, -1.0_krp, refres)
call scale(y, 1.0_krp/coef)

endsubroutine yeqax_free


!------------------------------------------------------------------------------!
! yeqmaxpz_free : y = - A.x + z
!------------------------------------------------------------------------------!
subroutine yeqmaxpz_free(y, x, z, refres, field, field_info)
implicit none
! -- parameters --
type(st_genericfield) , intent(out) :: y
type(st_genericfield) , intent(in)  :: x,z,refres
type(st_field)        , intent(in)  :: field
type(st_fieldinfo)    , intent(in)  :: field_info
! -- internal

! -- body --

call yeqax_free(y, x, refres, field, field_info)
call scale(y, -1.0_krp)
call xeqxpay(y, 1.0_krp, z)

endsubroutine yeqmaxpz_free


!------------------------------------------------------------------------------!
! gmres_free : @@@@@@@@@@@@@@@@
!------------------------------------------------------------------------------!
subroutine gmres_free(def_impli, sol, info)!@@@@@@@@@@@@@@@@)
implicit none
! -- parameters --
!type(st_genericfield) , intent(out) :: y
!type(st_genericfield) , intent(in)  :: x,z,refres
!type(st_field)        , intent(in)  :: field
!type(st_fieldinfo)    , intent(in)  :: field_info
! -- internal
! -- Inputs --
type(mnu_imp) :: def_impli
! -- Inputs/Outputs --
type(st_genericfield) , intent(inout) :: sol
! -- Outputs --
integer(kip)  :: info
! -- Internal variables --
type(st_genericfield)                :: r1, p1
type(st_genericfield), dimension(:), &
                       allocatable   :: w1, v1
real(krp), dimension(:), &
                       allocatable   :: ss, yy, qc, qs
real(krp), dimension(:,:),&
                       allocatable   :: hh
integer(kip)                         :: nit
real(krp)                            :: errgmres, ref
real(krp)                            :: beta, fact, tmp, normp1
integer(kip)                         :: ik, jk, nk, k
integer(kip)                         :: nkrylov

! -- Body --

nkrylov = 10_kip

!GG!    ! initialisation
!GG!    
!GG!    nit    = 0
!GG!    errgmres = huge(errgmres)    ! maximal real number in machine representation (to ensure 1st iteration)
!GG!    
!GG!    call new(p1, sol)
!GG!    call new(r1, sol)
!GG!    
!GG!    allocate(qc(nkrylov))
!GG!    allocate(qs(nkrylov))
!GG!    allocate(ss(nkrylov))
!GG!    allocate(yy(nkrylov))
!GG!    allocate(hh(nkrylov+1,nkrylov));
!GG!    
!GG!    ! -- initialization --
!GG!    
!GG!    call transfer_gfield(p1, sol)  ! save RHS
!GG!    
!GG!    ! -- initial guess --
!GG!    
!GG!    ! sol = p1   is the initial guess
!GG!    
!GG!    ref = l1norm(sol)
!GG!    
!GG!    normp1 = l2norm(p1)
!GG!    
!GG!    do while ((errgmres >= ref*def_impli%maxres).and.(nit <= def_impli%max_it*2))
!GG!    
!GG!      ! r0 = M^(-1) . ( b - A.x0 )
!GG!      call yeqmaxpz_free(r1, sol, p1, refres, field, field_info)
!GG!                                                    ! R1 = RHS - MAT.SOL
!GG!    
!GG!      beta = l2norm(r1)                             ! beta = ||r0||_2
!GG!      fact = 1.0_krp/beta
!GG!    
!GG!      call transfer_gfield(v1(1), r1)
!GG!      call scale(v1(1), fact)                       ! v1 = r0/|r0|
!GG!    
!GG!      ss(1:nkrylov) = 0.0_krp
!GG!      ss(1) = beta                                  ! s(1) = |r0|
!GG!    
!GG!      hh(1:nkrylov+1,1:nkrylov) = 0.0_krp
!GG!    
!GG!      do jk = 1,nkrylov
!GG!    
!GG!        call yeqax_free (w1(jk), v1(jk), refres, field, field_info) 
!GG!                                                    ! wj = M^(-1).A.vj
!GG!    
!GG!        do ik = 1,jk
!GG!          hh(ik,jk) = dot_prod(w1(jk),v1(ik))       ! hij = (wj,vi)
!GG!          call xeqxpay(w1,-hh(ik,jk),v1)            ! wj = wj - hij*vi
!GG!        enddo
!GG!    
!GG!        hh(jk+1,jk) = l2norm(w1(jk))                ! h(j+1,j) = ||wj||_2
!GG!    
!GG!        if ( hh(jk+1,jk) > 1.0E-14*normp1 ) then
!GG!    
!GG!          fact = 1.0_krp/hh(jk+1,jk)
!GG!          if ( jk < nkrylov ) then
!GG!            call transfer_gfield(v1(jk+1), w1(jk))
!GG!            call scale(v1(k+1), fact)               ! v(j+1) = wj/h(j+1,j)
!GG!          endif
!GG!    
!GG!          do ik = 1,jk-1                            ! apply J(1)...J(kj-1) on hh
!GG!            tmp = hh(ik,jk)
!GG!            hh(ik  ,jk) =  qc(ik)*tmp + qs(ik)*hh(ik+1,jk)
!GG!            hh(ik+1,jk) = -qs(ik)*tmp + qc(ik)*hh(ik+1,jk)
!GG!          enddo
!GG!    
!GG!          if ( hh(jk,jk) == 0 ) exit                ! Exit if hh(jk,jk)==0
!GG!    
!GG!          tmp = sqrt(hh(jk,jk)**2+hh(jk+1,jk)**2)   ! build J(jk)
!GG!          qc(jk) = hh(jk  ,jk)/tmp
!GG!          qs(jk) = hh(jk+1,jk)/tmp
!GG!    
!GG!          hh(jk  ,jk) = tmp
!GG!          hh(jk+1,jk) = 0.0_krp
!GG!    
!GG!          ss(jk+1) = -qs(jk)*ss(jk)                 ! apply J(jk) on ss
!GG!          ss(jk  ) =  qc(jk)*ss(jk)
!GG!    
!GG!          if ( abs(ss(jk+1)) < def_impli%maxres*abs(ss(1)) ) exit
!GG!    
!GG!        else
!GG!    
!GG!          exit
!GG!    
!GG!        endif
!GG!    
!GG!      enddo
!GG!    
!GG!      nk = jk-1
!GG!    
!GG!      ! Solve H.y = s
!GG!      do jk = nk,1,-1
!GG!        tmp = ss(jk)
!GG!        do ik = jk+1,nk
!GG!          tmp = tmp - hh(jk,ik)*yy(ik)
!GG!        enddo
!GG!        yy(jk) = tmp/hh(jk,jk)
!GG!        call xeqxpay(sol,yy(jk),v1(jk))             ! sol = sol + Vm.ym
!GG!      enddo
!GG!    
!GG!      call yeqmaxpz_free(r1, sol, p1, refres, field, field_info)
!GG!                                                    ! R1 = RHS - MAT.SOL
!GG!      ss(nk+1) =  l2norm(r1)
!GG!    
!GG!      errgmres = ss(nk+1)/normp1
!GG!    
!GG!      nit     = nit + 1
!GG!    
!GG!    enddo
!GG!    
!GG!    if (nit <= def_impli%max_it) then
!GG!      info = nit - 1
!GG!    else
!GG!      info = -1
!GG!    endif
!GG!    
!GG!    deallocate(r1, p1, qc, qs, ss, yy, hh, w1, v1)

endsubroutine gmres_free


endmodule MATFREE


!------------------------------------------------------------------------------!
! Changes history
!
! Jun 2009 : creation
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! MODULE : LAPACK                         Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cf historique)
!   Interfaces pour les routines LAPACK
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module LAPACK
  
!------------------------------------------------------------------------------!
! Liste des routines interfacées
!------------------------------------------------------------------------------!
! GEGV
! GESV
! GESVX
! GETRF
! POTRF
! POTRS
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! GEGV : Calcul de valeurs propres generalisees à partir d'une matrice générale
!------------------------------------------------------------------------------!
  interface lapack_gegv    
    
    subroutine sgegv(jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai, &
                     beta, vl, ldvl, vr, ldvr, work, lwork, info )
      character jobvl, jobvr
      integer   info, lda, ldb, ldvl, ldvr, lwork, n
      real      a(lda,*), alphai(*), alphar(*), b(ldb,*), &
                beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
    endsubroutine
    
    subroutine dgegv(jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai, &
                     beta, vl, ldvl, vr, ldvr, work, lwork, info )
      character        jobvl, jobvr
      integer          info, lda, ldb, ldvl, ldvr, lwork, n
      double precision a(lda,*), alphai(*), alphar(*), b(ldb,*), &
                       beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
    endsubroutine
    
    subroutine cgegv(jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,  &
                     vl, ldvl, vr, ldvr, work, lwork, rwork, info )
      character jobvl, jobvr
      integer   lda, ldb, n, ldvl, ldvr, lwork, info
      real      rwork(:)
      complex   a(lda,*), b(ldb,*)
      complex   alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
    endsubroutine
    
    subroutine zgegv(jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,  &
                     vl, ldvl, vr, ldvr, work, lwork, rwork, info)
      character  jobvl, jobvr
      integer    lda, ldb, n, ldvl, ldvr, lwork, info
      real(8)    rwork(*)
      complex(8) a(lda,*), b(ldb,*)
      complex(8) alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
    endsubroutine
  
  endinterface


!------------------------------------------------------------------------------!
! GESV : Résolution d'un systeme lineaire A.X=B
!   Decomposition LU de A, solution X dans B
!------------------------------------------------------------------------------!
  interface lapack_gesv    
    ! Resolution d'un systeme lineaire A.X=B
    ! Decomposition LU de A, solution X dans B
    
    subroutine sgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer info, lda, ldb, n, nrhs, ipiv(*)
      real    a(lda,*), b(ldb,*)
    endsubroutine sgesv

    subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer info, lda, ldb, n, nrhs, ipiv(*)
      real(8) a(lda,*), b(ldb,*)
    endsubroutine dgesv

    subroutine cgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer info, lda, ldb, n, nrhs, ipiv(*)
      complex a(lda,*), b(ldb,*)
    endsubroutine cgesv

    subroutine zgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer    info, lda, ldb, n, nrhs, ipiv(*)
      complex(8) a(lda,*), b(ldb,*)
    endsubroutine zgesv

  endinterface

  
!------------------------------------------------------------------------------!
! GESVX : Résolution (expert) d'un systeme lineaire A.X=B
!   Decomposition LU de A, solution X dans B
!   Estimation des erreurs commises et calcul du nombre de condition
!------------------------------------------------------------------------------!
  interface lapack_gesvx
    
    subroutine sgesvx(fact, trans, n, nrhs, a, lda, af, ldaf, ipiv, equed, &
                      r, c, b, ldb, x, ldx, rcond, ferr, berr, work,       &
                      irwork, info)
      character fact, trans, equed
      integer   info, lda, ldaf, ldb, ldx, n, nrhs, ipiv(*)
      real(4)   rcond, berr(*), c(*), ferr(*), r(*) 
      integer   irwork(*)
      real(4)   a(lda,*), af(ldaf,*), b(ldb,*), work(*), x(ldx,*)
    endsubroutine sgesvx

    subroutine dgesvx(fact, trans, n, nrhs, a, lda, af, ldaf, ipiv, equed, &
                      r, c, b, ldb, x, ldx, rcond, ferr, berr, work,       &
                      irwork, info)
      character fact, trans, equed
      integer   info, lda, ldaf, ldb, ldx, n, nrhs, ipiv(*)
      real(8)   rcond, berr(*), c(*), ferr(*), r(*) 
      integer   irwork(*)
      real(8)   a(lda,*), af(ldaf,*), b(ldb,*), work(*), x(ldx,*)
    endsubroutine dgesvx

    subroutine cgesvx(fact, trans, n, nrhs, a, lda, af, ldaf, ipiv, equed, &
                      r, c, b, ldb, x, ldx, rcond, ferr, berr, work,       &
                      irwork, info)
      character  fact, trans, equed
      integer    info, lda, ldaf, ldb, ldx, n, nrhs, ipiv(*)
      real(4)    rcond, berr(*), c(*), ferr(*), r(*) 
      real(4)    irwork(*)
      complex(4) a(lda,*), af(ldaf,*), b(ldb,*), work(*), x(ldx,*)
    endsubroutine cgesvx

    subroutine zgesvx(fact, trans, n, nrhs, a, lda, af, ldaf, ipiv, equed, &
                      r, c, b, ldb, x, ldx, rcond, ferr, berr, work,       &
                      irwork, info)
      character  fact, trans, equed
      integer    info, lda, ldaf, ldb, ldx, n, nrhs, ipiv(*)
      real(8)    rcond, berr(*), c(*), ferr(*), r(*) 
      real(8)    irwork(*)
      complex(8) a(lda,*), af(ldaf,*), b(ldb,*), work(*), x(ldx,*)
    endsubroutine zgesvx

  endinterface


!------------------------------------------------------------------------------!
! GETRF : Décomposition LU d'une matrice générale
!------------------------------------------------------------------------------!
  interface lapack_getrf   

    subroutine sgetrf(m, n, a, lda, piv, info)
      integer :: lda, m, n, info, piv(*)
      real    :: a(lda,*)
    endsubroutine sgetrf

    subroutine dgetrf(m, n, a, lda, piv, info)
      integer :: lda, m, n, info, piv(*)
      real(8) :: a(lda,*)
    endsubroutine dgetrf

    subroutine cgetrf(m, n, a, lda, piv, info)
      integer :: lda, m, n, info, piv(*)
      complex :: a(lda,*)
    endsubroutine cgetrf

    subroutine zgetrf(m, n, a, lda, piv, info)
      integer    :: lda, m, n, info, piv(*)
      complex(8) :: a(lda,*)
    endsubroutine zgetrf

  endinterface


!------------------------------------------------------------------------------!
! POTRF : Décomposition de Choleski d'une matrice symétrique définie positive
!   A est écrasée par la matrice U ou L de la décomposition Ut.U ou L.Lt
!   si uplo='U', seul triang. sup. de A est utilisée en entrée
!   si uplo='L', seul triang. inf. de A est utilisée en entrée
!------------------------------------------------------------------------------!
  interface lapack_potrf   

    subroutine spotrf(uplo, n, a, lda, info)
      character :: uplo                
      integer   :: lda, n, info
      real      :: a(lda,*)    
    endsubroutine spotrf

    subroutine dpotrf(uplo, n, a, lda, info)
      character :: uplo
      integer   :: lda, n, info
      real(8)   :: a(lda,*)
    endsubroutine dpotrf

    subroutine cpotrf(uplo, n, a, lda, info)
      character :: uplo
      integer   :: lda, n, info
      complex   :: a(lda,*)
    endsubroutine cpotrf

    subroutine zpotrf(uplo, n, a, lda, info)
      character  :: uplo
      integer    :: lda, n, info
      complex(8) :: a(lda,*)
    endsubroutine zpotrf

  endinterface


!------------------------------------------------------------------------------!
! POTRS : Résolution de A.X = B (A matrice symétrique ou hermitienne)
!   A est la décomposition de Choleski Ut.U ou L.Lt, calculée par POTRF
!   si uplo='U', seul triang. sup. de A est utilisée en entrée
!   si uplo='L', seul triang. inf. de A est utilisée en entrée
!------------------------------------------------------------------------------!
  interface lapack_potrs   

    subroutine spotrs(uplo, n, nrhs, a, lda, b, ldb, info)
      character :: uplo
      integer   :: info, lda, ldb, n, nrhs
      real      :: a(lda,*), b(ldb,*)
    endsubroutine spotrs

    subroutine dpotrs(uplo, n, nrhs, a, lda, b, ldb, info)
      character :: uplo
      integer   :: info, lda, ldb, n, nrhs
      real(8)   :: a(lda,*), b(ldb,*)
    endsubroutine dpotrs

    subroutine cpotrs(uplo, n, nrhs, a, lda, b, ldb, info)
      character :: uplo
      integer   :: info, lda, ldb, n, nrhs
      complex   :: a(lda,*), b(ldb,*)
    endsubroutine cpotrs

    subroutine zpotrs(uplo, n, nrhs, a, lda, b, ldb, info)
      character  :: uplo
      integer    :: info, lda, ldb, n, nrhs
      complex(8) :: a(lda,*), b(ldb,*)
    endsubroutine zpotrs

  endinterface


!
!  interface lapack_gesvd   ! Calcul de valeurs singulieres 
!  endinterface
!

endmodule LAPACK

!------------------------------------------------------------------------------!
! Historique des modifications
!
! sept 2003 : création du module, interfaces GExxx et POxxx
!------------------------------------------------------------------------------!

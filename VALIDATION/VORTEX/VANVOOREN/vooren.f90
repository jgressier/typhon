program vooren

implicit none

integer :: npts     ! nombre de points de discrétisation du profil
real(8) :: corde    ! corde du profil
real(8) :: epsi     ! paramètre d'épaisseur
real(8) :: deltaBF  ! écart angulaire du bord de fuite
real(8) :: alpha    ! incidence pour le calcul de Cp

real(8) :: va, vk   ! paramètre de génération (cf Katz & Plotkin)

integer :: i
real(8) :: pi, theta, r1, r2, th1, th2, pA, pB, D0, D1, D2
real(8) :: dth, cth, sth, ck1, ck2, sk1, sk2, pp, kr, alp

real(8), dimension(:), allocatable :: x, y, u, v, p

! -- début du programme --

print*,"------------------------------------------------"
print*,"Génération de profil VAN DE VOOREN"
print*,"------------------------------------------------"

! -- lecture des paramètres --

write(*,'("       corde du profil : ")', advance="no") ; read(*,*) corde
write(*,'("    épaisseur relative : ")', advance="no") ; read(*,*) epsi
write(*,'("angle de bord de fuite : ")', advance="no") ; read(*,*) deltaBF
write(*,'("      nombre de points : ")', advance="no") ; read(*,*) npts
write(*,'("             incidence : ")', advance="no") ; read(*,*) alpha


! -- préparation du calcul  --

print*
print*," * génération de la géométrie et calcul aérodynamique"

pi  = acos(-1._8)
vk  = 2._8 - deltaBF/180._8
va  = corde * (1+epsi)**(vk-1._8) / 2._8**vk
alp = alpha*pi/180._8

allocate(x(0:npts))
allocate(y(0:npts))
allocate(u(0:npts))
allocate(v(0:npts))
allocate(p(0:npts))

! -- calcul  --

dth = 2._8*pi/real(npts,8)

do i = 0, npts

  theta = i*dth
  cth   = cos(theta)
  sth   = sin(theta)
  r1    = va*sqrt((cth-1._8)**2 + sth**2)
  r2    = va*sqrt((cth-epsi)**2 + sth**2)
  th1   = atan2(sth, cth-1._8) !+ pi
  th2   = atan2(sth, cth-epsi) 
  !if (cth < epsi)     th2 = th2 + pi
  
  ! géométrie

  ck1 = cos(vk*th1)
  ck2 = cos((vk-1._8)*th2)
  sk1 = sin(vk*th1)
  sk2 = sin((vk-1._8)*th2)
  kr  = r1**vk / r2**(vk-1._8)

  x(i) = kr*(ck1*ck2 + sk1*sk2)
  y(i) = kr*(sk1*ck2 - ck1*sk2)

  !write(*,'(4f8.4)') theta, atan2(sth,cth), x(i), y(i)

  ! vitesse

  ck1 = cos((vk-1._8)*th1)
  ck2 = cos(vk*th2)
  sk1 = sin((vk-1._8)*th1)
  sk2 = sin(vk*th2)
  kr  = r2**vk / r1**(vk-1._8)
  pA  = ck1*ck2 + sk1*sk2
  pB  = sk1*ck2 - ck1*sk2
  D0  = va*( 1._8 + vk*(epsi-1._8) )
  D1  = pA*(va*cth-D0) - pB*va*sth
  D2  = pA*va*sth      + pB*(va*cth-D0)
  pp  = 2._8*kr*(sin(alp)-sin(alp-theta))/(D1**2+D2**2)

  if (r1 == 0._8) then
    u(i) = 0._8
    v(i) = 0._8
  else
    u(i) =   pp * (D1*sth + D2*cth)
    v(i) = - pp * (D1*cth - D2*sth)
  endif

  p(i) = 1._8 - (u(i)**2 + v(i)**2)
  !write(*,'(8f8.4)') theta, D1, D2, r1, r2, u(i), v(i), p(i)

enddo

! -- écriture du fichier géométrie  --

print*
print*," * écriture du fichier géométrie vooren-geo.dat"

open(10, file="vooren-geo.dat", form="formatted")

write(10,*) "# profil van de Vooren ",npts," panneaux"
write(10,*) "# corde:",corde,", epsilon:",epsi,", angle BF:",deltaBF

do i = 0, npts
  write(10,'(2e16.8)') x(i), y(i)
enddo

close(10)

! -- écriture du fichier résultat  --

print*
print*," * écriture du fichier résultat vooren-res.dat"

open(10, file="vooren-res.dat", form="formatted")

write(10,*) "# profil van de Vooren ",npts," panneaux"
write(10,*) "# corde:",corde,", epsilon:",epsi,", angle BF:",deltaBF
write(10,*) "# incidence:",alpha

do i = 0, npts
  write(10,'(5e16.8)') x(i), y(i), u(i), v(i), p(i)
enddo

close(10)


print*
print*,"------------------------------------------------"

deallocate(x, y, u, v, p)


endprogram

!------------------------------------------------------------------------------!
! Liste de fonctions : count_struct       Auteur : J. Gressier
!                                         Date   : Mai 2002
! Fonction                                Modif  :
!   Bibliotheque de procedures et fonctions pour la gestion de maillages
!   sructures
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! Fonction : compte le nombre de bloc de la zone
!------------------------------------------------------------------------------!
integer function nb_block(zone)
use STRMESH
implicit none

type(st_zone)           :: zone
type(st_block), pointer :: p
integer                 :: i

i =  0
p => zone%block
do while (associated(p))
  i =  i + 1
  p => p%next
enddo
nb_block = i

endfunction nb_block


!------------------------------------------------------------------------------!
! Fonction : compte le nombre de connection du bloc
!------------------------------------------------------------------------------!
integer function nb_connect(block)
use STRMESH
implicit none

type(st_block)            :: block
type(st_connect), pointer :: p
integer                   :: i

i =  0
p => block%connect
do while (associated(p))
  i =  i + 1
  p => p%next
enddo
nb_connect = i

endfunction nb_connect


!------------------------------------------------------------------------------!
! Fonction : compte le nombre de conditions aux limites du bloc
!------------------------------------------------------------------------------!
integer function nb_bound(block)
use STRMESH
implicit none

type(st_block)          :: block
type(st_bound), pointer :: p
integer                 :: i

i =  0
p => block%bound
do while (associated(p))
  i =  i + 1
  p => p%next
enddo
nb_bound = i

endfunction nb_bound


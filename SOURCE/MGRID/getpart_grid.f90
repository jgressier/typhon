!------------------------------------------------------------------------------!
! Procedure : getpart_grid                Auteur : Jerome RODRIGUEZ
!                                         Date   : Mars 2005
! Fonction                                Modif  :
!   Decoupage de grille selon l'axe x
!
! Defauts/Limitations/Divers :
!   
!
!------------------------------------------------------------------------------!
subroutine getpart_grid(fullgrid, ipart, npart, grid)

use OUTPUT
use MGRID
use USTMESH
use MESHBASE
use CONNECTIVITY
!use SUBGRID
!use METIS

implicit none

!------------------------------------------------------------------------------!
! Definition ST_CHAINGRAPH :
!------------------------------------------------------------------------------!
type st_chaingraph
  integer                            :: dim
  integer, dimension(:), allocatable :: tab
  type(st_chaingraph), pointer       :: next
endtype st_chaingraph
 
 
!------------------------------------------------------------------------------!
! Definition ST_CHAINCOUPLE :
!------------------------------------------------------------------------------!
type st_chaincouple
  integer                            :: id1
  integer                            :: id2
  type(st_chaincouple), pointer       :: next
endtype st_chaincouple
!------------------------------------------------------------------------------!
 

! -- Declaration des entrees  --
type(st_grid),target            :: fullgrid  ! Grille initiale a decouper
integer                         :: ipart, npart  ! index of part to extract and tot nb of parts

! -- Declaration des entrees / sorties --
type(st_grid)                   :: grid       

! -- Declaration des sorties --
type(st_grid) , pointer         :: pgrid         ! Première grille découpee


! -- Declaration des variables internes --
integer                         :: first_id      ! identifiants des grilles
integer                         :: second_id
integer                         :: first_cpu
integer                         :: second_cpu
type(st_grid) , pointer         :: pfullgrid ! Grille initiale a decouper


type(st_chaincouple),pointer     :: pchaincouple
type(st_chaincouple), target     :: chaincouple

type(st_chaingraph),pointer     :: pchaingraph
type(st_chaingraph), target     :: chaingraph

! Variables fonction METIS
integer, dimension(:), allocatable ::  vwgt, adjwgt  ! Weight of vertices and edges
integer                            ::  wgtflag       ! Weight option
integer, dimension(:), allocatable ::  adjncy        ! CSR DATA
integer, dimension(:), allocatable ::  xadj          ! CSR DATA
integer                            ::  n             ! Num of vertices
integer                            ::  numflag       ! FLAG TO BE SET TO 1 IN FORTRAN
integer                            ::  nparts        ! Number of parts 
integer, dimension(5)              ::  options       ! Misc Options
integer                            ::  edgecut       ! Number of edge cut
integer, dimension(:), allocatable ::  part          ! Result of METIS

! Autres variables
integer                            ::  size_adjncy   ! Size of adjncy
integer                            ::  size_xadj     ! Size of xadj
integer                            ::  cur_adjncy    ! Other variable

integer, dimension(:), allocatable :: tab_parts      ! Number of cells in each parts
integer, dimension(:), allocatable :: tab_intcell 
integer                            :: ncell_real    ! nombre reel de cellules a extraire
integer                            :: i, j, k  ! compteur de boucle
integer                            :: id_tmp !stockage temporaire d'id
integer                            :: count_tmp ! stockahe temporaire d'un compteur de chaine
integer                            :: count_tmp2 ! stockahe temporaire d'un compteur de chaine
! -- Debut de la procedure --

!--------------------------------------------------------------------
! Initialization of zone parameters



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creation des couples exhaustifs des voisins
! les couples sont crées dans les deux sens (i,j) et (j,i)

! Traitement du cas initial
allocate(pchaincouple)
pchaincouple=>chaincouple
pchaincouple%id1=fullgrid%umesh%facecell%fils(1,1) ! cell1 et cell2 sont les
pchaincouple%id2=fullgrid%umesh%facecell%fils(1,2) ! deux cellules de la face
allocate(pchaincouple%next)
pchaincouple=>pchaincouple%next
pchaincouple%id2=fullgrid%umesh%facecell%fils(1,1) ! cell1 et cell2 sont les
pchaincouple%id1=fullgrid%umesh%facecell%fils(1,2) ! deux cellules de la face

! Traitement des autres cas
do i=2, fullgrid%umesh%nface_int ! Parcours des faces de la grille mere
   allocate(pchaincouple%next)
   pchaincouple%next%id1=fullgrid%umesh%facecell%fils(i,1) ! cell1 et cell2 sont les
   pchaincouple%next%id2=fullgrid%umesh%facecell%fils(i,2) ! deux cellules de la face
   pchaincouple=>pchaincouple%next
   allocate(pchaincouple%next)
   pchaincouple%next%id2=fullgrid%umesh%facecell%fils(i,1) ! cell1 et cell2 sont les
   pchaincouple%next%id1=fullgrid%umesh%facecell%fils(i,2) ! deux cellules de la face
   pchaincouple=>pchaincouple%next
   nullify(pchaincouple%next)
enddo


pchaincouple=>chaincouple
first_id = 0
do while(associated(pchaincouple))
   first_id = first_id + 1
   write(str_w,*) ".          Edge:",first_id, "  Vertex:(", pchaincouple%id1, ",", pchaincouple%id2, ")"
   call print_info(8,adjustl(str_w))
   pchaincouple=>pchaincouple%next
enddo



allocate(pchaingraph)


!!!!! Premier cas i=1
pchaincouple=>chaincouple
count_tmp  =  0
! On compte le nombre de voisins
do while(associated(pchaincouple))
   if (pchaincouple%id1.eq.1) then
      count_tmp = count_tmp + 1
   endif
     pchaincouple=>pchaincouple%next
enddo

! On alloue
chaingraph%dim = count_tmp
allocate(chaingraph%tab(count_tmp))

! On remplis
pchaincouple=>chaincouple
j = 0

do while(associated(pchaincouple))
   if (pchaincouple%id1.eq.1) then
      j = j + 1
      ! Controle d'erreur
      if (j>count_tmp) then
         call erreur("Fonction METIS","Erreur numero 107235")
      endif
      chaingraph%tab(j) = pchaincouple%id2
   endif
   pchaincouple=>pchaincouple%next
enddo
! Controle d'erreur
if (j<count_tmp) then
   call erreur("Fonction METIS","Erreur numero 167234")
endif


pchaingraph=>chaingraph

!!!!! Cas general > 2
do i=2,fullgrid%umesh%ncell_int
   pchaincouple=>chaincouple
   count_tmp  =  0

   ! On compte le nombre de voisins
   do while(associated(pchaincouple))
      if (pchaincouple%id1.eq.i) then
      count_tmp = count_tmp + 1
      endif
      pchaincouple=>pchaincouple%next
   enddo

   ! On alloue
   allocate(pchaingraph%next)
   pchaingraph%next%dim = count_tmp
   allocate(pchaingraph%next%tab(count_tmp))
    

   ! On remplis
   pchaincouple=>chaincouple
   j = 0

   do while(associated(pchaincouple))
      if (pchaincouple%id1.eq.i) then
        j = j + 1
        ! Controle d'erreur
        if (j>count_tmp) then
           call erreur("Fonction METIS","Erreure numero 07235")
        endif
        pchaingraph%next%tab(j) = pchaincouple%id2
      endif
      pchaincouple=>pchaincouple%next
   enddo

   ! Controle d'erreur
   if (j<count_tmp) then
      call erreur("Fonction METIS","Erreure numero 67234")
   endif
   pchaingraph => pchaingraph%next
enddo


pchaingraph=>chaingraph
first_id = 0
do while(associated(pchaingraph))
    first_id = first_id + 1
    do i=1, pchaingraph%dim
    write(str_w,*) ".          Vertex:", first_id, " Neigh", pchaingraph%tab(i)
    call print_info(8,adjustl(str_w))
    enddo
    pchaingraph=>pchaingraph%next

enddo


! On compte le nombre total d'elements a mettre (finalement la somme sur tous les vertex
! de leur voisins)
pchaingraph=>chaingraph
count_tmp = 0
count_tmp2 = 0
do while(associated(pchaingraph))
    count_tmp2 = count_tmp2 + 1
    count_tmp = count_tmp +  pchaingraph%dim
    pchaingraph=>pchaingraph%next
enddo


! On alloue ce qu'il faut
size_adjncy = count_tmp
size_xadj = count_tmp2+1
allocate(adjncy(size_adjncy))
allocate(xadj(size_xadj))
xadj(1)=0

pchaingraph=>chaingraph
cur_adjncy = 1  ! indice courant du tbl adjncy

j=0
k=0
do while(associated(pchaingraph))
    k=k+1
    do i=1, pchaingraph%dim
       j=j+1
       adjncy(i + cur_adjncy - 1) = pchaingraph%tab(i)
    enddo
    xadj(k+1) = j
    cur_adjncy = cur_adjncy + pchaingraph%dim
    pchaingraph=>pchaingraph%next
enddo

    do i=1, size_xadj
    write(str_w,*) ".", xadj(i)
    call print_info(8,adjustl(str_w))
    enddo
    write(str_w,*) "."
    call print_info(8,adjustl(str_w))

    do i=1, size_adjncy
    write(str_w,*) ".", adjncy(i)
    call print_info(8,adjustl(str_w))
    enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! L'algo précédent sort deux tableaux de connectivite au format CSR
!  xadj(size_xadj)
!  adjncy(size_adjncy)


n          = size_xadj - 1    ! Number of vertices
nparts     = 2                ! Number of cuts = number of proc
wgtflag    = 0                ! Flag (weighted graph or not)
options(1) = 0                ! Misc Options
options(2) = 0
options(3) = 0
options(4) = 0
options(5) = 0
numflag    = 1                ! TO BE SET TO 1 IN FORTRAN 

allocate(part(n))             ! RESULTS

!!!!!! SET WEIGHT TO 1
j = size_adjncy / 2
allocate(adjwgt(j))
do i=1,j
adjwgt(i)=1
enddo

allocate(vwgt(n))
do i=1,n
vwgt(i)=1
enddo
!!!!!!!!!!!!!!!!!!!!

        write(str_w,*) ".          Num of vertices:",n
        call print_info(8,adjustl(str_w))


call print_info(8,"  CALL METIS LIBRARY...")
call METIS_PartGraphKway( n, xadj, adjncy, vwgt, adjwgt,wgtflag, numflag, nparts, options, edgecut, part)
call print_info(5,"  DONE")


do i=1,nparts
   do j=1, n
      if (part(j).eq.i) then
        write(str_w,*) ".          Vertex:", j, "  belong to:",i
        call print_info(8,adjustl(str_w))
      endif
   enddo
enddo



pfullgrid=>fullgrid
allocate(tab_parts(nparts))

do i=1,nparts
   count_tmp = 0
   do j=1, n
      if (part(j).eq.i) then
         count_tmp = count_tmp + 1
      endif
   enddo
   tab_parts(i)=count_tmp
enddo

! THE REST CAN BE USED FOR 2 PARTS ONLY
if (nparts /= 2) then
  call erreur("CAS NON IMPLEMENTE"," NE PEUX PAS DECOUPER SUR PLUS DE DEUX PROCESSEURS ")
endif

allocate(tab_intcell(tab_parts(1)))
ncell_real = tab_parts(1)

   count_tmp = 0
   do j=1, n
      if (part(j).eq.1) then
         count_tmp = count_tmp + 1
         tab_intcell(count_tmp) = j
      endif
   enddo



     ! call horizontal_subgrid(pfullgrid, ncell_real, tab_intcell, pgrid, 10,11, 1,nbproc, defsolver)

     ! On renvoie le resultat de la fonction horizontal_subgrid dans l'argument de la fonction
     ! pour que la fonction mere puisse en avoir les resultats
     grid=pgrid


      write(str_w,*) ".       Etat variablesde  ", grid%info%mpi_cpu
      call print_info(8,adjustl(str_w))


      write(str_w,*) ".       Etat variablesbis  ", grid%next%info%mpi_cpu
      call print_info(8,adjustl(str_w))


endsubroutine getpart_grid

!------------------------------------------------------------------------------!
! Change history
!
! Sept 2005 : initial subroutine from DIVISION/division_zone in pamr branch
!------------------------------------------------------------------------------!

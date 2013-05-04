!------------------------------------------------------------------------------!
! MODULE : COLOR
!
! Fonction
!
!------------------------------------------------------------------------------!
module COLOR

use MESHPREC        ! accuracy definition
use CONNECTIVITY 
use IOCFD 

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! for graph/mesh, vertex is a cell and edge is a face
!------------------------------------------------------------------------------!
subroutine coo_colorgroup(nedge, nvertex, edgecon, colors)
implicit none
integer(kip),        intent(in)  :: nedge, nvertex
type(st_connect),    intent(in)  :: edgecon         ! nbnodes >= nedge
type(st_genconnect), intent(out) :: colors
! -- internal --
type(st_genconnect)       :: vertexcolor
integer(kip)              :: iv(2), jf, ic
integer(kip), parameter   :: vresize = 10, cresize = 100
logical                   :: colored
! --- BODY ---

! -- init --

call new_genconnect_nelem(colors,      1,       cresize, initdim=0)
call new_genconnect_nelem(vertexcolor, nvertex, vresize, initdim=0)

! -- loop on edges --

do jf = 1, nedge
  colored = .false.
  iv(1:2) = edgecon%fils(jf,1:2)
  if ((iv(2)==0).or.(iv(2)>nvertex)) iv(2) = iv(1)  ! handle incomplete edge (no vertex 2)
  do ic  = 1, colors%nbnodes
    if (index(ic, vertexcolor%node(iv(1)))==0) then
      if (index(ic, vertexcolor%node(iv(2)))==0) then
        ! none of both vertices were found in this color group, so add edge
        call add_element(colors, ic, jf, cresize)           ! add edge to color group
        call add_element(vertexcolor, iv(1), ic, vresize)   ! add color to vertex
        call add_element(vertexcolor, iv(2), ic, vresize)   ! add color to vertex
        colored = .true. 
        exit
      endif
    endif
  enddo
  if (.not.colored) then
    ! add a new color
    call genconnect_addnode(colors, cresize, initdim=0)
    ic = colors%nbnodes
    ! add edge
    call add_element(colors, ic, jf, cresize)           ! add edge to color group
    call add_element(vertexcolor, iv(1), ic, vresize)   ! add color to vertex
    call add_element(vertexcolor, iv(2), ic, vresize)   ! add color to vertex
  endif
enddo

call delete(vertexcolor)

endsubroutine

endmodule COLOR
!------------------------------------------------------------------------------!
! Changes history
!
! May  2013: Created
!------------------------------------------------------------------------------!




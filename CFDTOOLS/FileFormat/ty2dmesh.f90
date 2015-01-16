!> @addtogroup Program
!------------------------------------------------------------------------------!
!> @ingroup Program
!> @brief generation of 2D structured based mesh
!> features
!> - quads, or split of quads in 2 tri or 4 tris
!> - morphing functions
!> - split of boundary tags
!------------------------------------------------------------------------------!
program ty2dmesh

use IOCFD
use VEC3D
use USTMESH
use XBIN_IO
use TYPHON_FMT
use TYFMT_MESH
use FTNARGS
use FCT_PARSER
!use FLUENT

implicit none

!------------------------------------------------------------------------------!
integer            :: nargs
character(len=256) :: filename, str_opt, str_arg, str_val
character(len=16)  :: str_dir, strface
type(st_deftyphon) :: deftyphon
integer(kip), target  :: ni, nj
integer(kip), pointer :: ndir
logical, target    :: niread, njread
logical, pointer   :: ndread
real(krp), target  :: lx, ly
real(krp), pointer :: ldir
logical, target    :: lxread, lyread
logical, pointer   :: ldread
character(len=256), target  :: strx, stry
character(len=256), pointer :: strdir
logical, target    :: strxread, stryread
logical, pointer   :: strdread
integer(kip), target  :: niwjmin, niwjmax, njwimin, njwimax
integer(kip), pointer :: nsdir
logical, target       :: erriwjmin, erriwjmax, errjwimin, errjwimax, spliterr
logical, pointer      :: errdir
type(st_ustmesh)      :: umesh
type(v3d), dimension(:,:,:), pointer :: vertex
type(st_elemvtex), pointer :: elem
integer(kpp)          :: itype, ielem, ntype_mesh, ntype_stor
type(st_fct_node)     :: morphx, morphy, morphz
type(st_fctfuncset)   :: fctenv
logical               :: fctscale, cstscale
!---------------------------
logical, parameter :: lincr = .TRUE.
logical, parameter :: lnoblk = .TRUE.
integer(kip)       :: ierr
integer(kip), parameter :: nsplit = 64
integer(kip), dimension(nsplit), target :: iwjmin, iwjmax, jwimin, jwimax
integer(kip), dimension(:), pointer :: wsdir
integer(kip)       :: dimspl
character(len=16)  :: strnsp
character(len=256) :: bcstr
!---------------------------
integer(kip)       :: i, j
integer(kip)       :: iv, ic, nelem, nvtex, iarg, nn, ii, jj, iboco
integer(kip)       :: iva, ivb, ivc, ivd, ivm
!------------------------------------------------------------------------------!
integer(kpp), parameter :: ni_default = 100
integer(kpp), parameter :: nj_default = 100
integer(kpp), parameter :: mesh_quad = 1
integer(kpp), parameter :: mesh_tri  = 2
integer(kpp), parameter :: mesh_tri4 = 4
logical, target    :: iwjminread, iwjmaxread, jwiminread, jwimaxread
logical, pointer   :: wread ! list of windows is read
logical, target    :: splitjmin, splitjmax, splitimin, splitimax
logical, pointer   :: sread ! list of splits is read
logical            :: fileread
logical            :: typeread
character, parameter :: sep = ':'
character          :: cep
character          :: c_odir ! character for other direction

integer :: idir, iend, ij, ijmin, ijmax, iijj, nij

! BODY

call print_cfdtools_header("TY2DMESH")

!------------------------------
! parse arguments

! default values

dimspl = nsplit-2
strnsp = strof(dimspl)

! Lengths
lx = 1._krp
ly = 1._krp
! Functions
fctscale = .false.
cstscale = .false.
strx = "x"
stry = "y"
! Dimensions
ni = 0
nj = 0
filename  = ""
ntype_mesh = 0

nargs    = command_argument_count()

iarg     = 1
do while (iarg <= nargs)
  call read_command_argument(iarg, str_opt, lincr)
  if ( str_opt == "-h" &
  .or. str_opt == "--help" ) then
    call print_help()
    stop
  endif
  iarg = iarg + 1
enddo

niwjmin = 0
niwjmax = 0
njwimin = 0
njwimax = 0

niread = .FALSE.
njread = .FALSE.
iwjminread = .FALSE.
iwjmaxread = .FALSE.
jwiminread = .FALSE.
jwimaxread = .FALSE.
splitjmin = .FALSE.
splitjmax = .FALSE.
splitimin = .FALSE.
splitimax = .FALSE.
typeread = .FALSE.
fileread = .FALSE.

iarg     = 1
do while (iarg <= nargs)
  call read_command_argument(iarg, str_opt, lincr)
  select case(str_opt)
  ! number of cells or window sizes array in x, y directions
  case ("-nx","-ny", &
        "-ni","-nj")
    if (iarg>nargs) call cfd_error("missing argument(s) after '"//trim(str_opt)//"'")
    ! change to "ni", "nj"
    str_dir = str_tr(str_opt(2: ), 'xy', 'ij')
    ! other direction
    c_odir  = str_tr(str_dir(2:2), 'ij', 'ji')
    ! pointers
    select case(str_dir)
    case("ni") ; ndir => ni ; ndread => niread
    case("nj") ; ndir => nj ; ndread => njread
    endselect
    ! read number or string
    call read_command_argument(iarg, ndir, lincr, ierr, str_arg)
    ! if number
    if (ierr==0) then
      ! check no redefinition
      if (ndread) call cfd_error("too many "//trim(str_arg)//" definitions")
      ! set size read
      ndread = .TRUE.
    ! if string
    else
      ! pointers
      select case(str_arg)
      case ("jmin") ; nsdir => niwjmin ; sread => splitjmin
                      wsdir =>  iwjmin ; wread =>    iwjminread
      case ("jmax") ; nsdir => niwjmax ; sread => splitjmax
                      wsdir =>  iwjmax ; wread =>    iwjmaxread
      case ("imin") ; nsdir => njwimin ; sread => splitimin
                      wsdir =>  jwimin ; wread =>    jwiminread
      case ("imax") ; nsdir => njwimax ; sread => splitimax
                      wsdir =>  jwimax ; wread =>    jwimaxread
      case default
        call cfd_error("integer, '"//c_odir//"min'"// &
                            " or '"//c_odir//"max'"// &
                       " expected after "//trim(str_dir)//" direction,"// &
                       " found '"//trim(str_arg)//"'")
      endselect
      ! check string : "j(min|max)" for "ni", "i(min|max)" for "nj"
      if (str_arg(1:1)/=c_odir) then
        call cfd_error("'"//c_odir//"min'"// &
                   " or '"//c_odir//"max'"// &
                   " expected after "//trim(str_dir)//" direction,"// &
                   " found '"//trim(str_arg)//"'")
      endif
      ! check no redefinition
      if (wread .OR. sread) call cfd_error("too many "//trim(str_arg)//" definitions")
      ! set window read
      wread = .TRUE.
      ! read window sizes array
      if (iarg>nargs) then
        call cfd_error("missing arguments after '"//trim(str_arg)//"'")
      endif
      call read_command_argument(iarg, str_val, lincr)
      call splitstring_integer(str_val, nsdir, wsdir(2:nsplit-1), ierr, sep)
      if (ierr/=0) then
        call cfd_error("'"//sep//"'-separated list of sizes expected after "// &
                       "'"//trim(str_arg)//"', found '"//trim(str_val)//"'")
      endif
      ! check array size
      if (nsdir<0) then
        call cfd_error("too many windows: "// &
                       trim(strof(-nsdir))//" > "//trim(strnsp)//" ( "//trim(str_val)//" )")
      endif
      ! sum-up sizes for positions
      wsdir(1) = 0
      do ii = 1,nsdir
        wsdir(ii+1) = wsdir(ii)+wsdir(ii+1)
      enddo
    endif
  ! lengths in x, y directions
  case ("-lx", "-ly")
    cstscale = .true.
    if (iarg>nargs) call cfd_error("missing argument after '"//trim(str_opt)//"'")
    ! pointers
    select case(str_opt)
    case("-lx") ; ldir => lx ; ldread => lxread
    case("-ly") ; ldir => ly ; ldread => lyread
    endselect
    ! read real
    call read_command_argument(iarg, ldir, lincr, ierr, str_val)
    ! if real
    if (ierr==0) then
      ! check no redefinition
      if (ldread) call cfd_error("too many "//trim(str_opt)//" definitions")
      ! set size read
      ldread = .TRUE.
    ! if not real
    else
      call cfd_error("real expected after '"//trim(str_opt)//"'" &
                               //", found '"//trim(str_val)//"'")
    endif
    select case(str_opt)
    case("-lx") ; strx = strof(lx)//"*x" ; strxread = .TRUE.
    case("-ly") ; stry = strof(ly)//"*y" ; stryread = .TRUE.
    endselect
    cstscale = .false.
    fctscale = .true.
    ldir = 1.0_krp
  ! scaling functions for x, y
  case ("-fx", "-fy")
    fctscale = .true.
    if (iarg>nargs) call cfd_error("missing argument after '"//trim(str_opt)//"'")
    ! pointers
    select case(str_opt)
    case("-fx") ; strdir => strx ; strdread => strxread
    case("-fy") ; strdir => stry ; strdread => stryread
    endselect
    call read_command_argument(iarg, strdir, lincr)
    ! set string read
    strdread = .TRUE.
  ! split positions array
  case ("-nisjmin","--nisplitjmin", &
        "-nisjmax","--nisplitjmax", &
        "-njsimin","--njsplitimin", &
        "-njsimax","--njsplitimax")
    ! change to "-n?s?(min|max)"
    if (str_opt(1:2)=="--") str_opt = str_opt(2:)
    if (str_opt(5:8)=="plit") str_opt(5:) = str_opt(9:)
    ! get "n? ?(min|max)"
    str_dir = str_tr(str_opt(2:), "s", " ")
    ! pointers
    select case(str_opt)
    case("-nisjmin")
      nsdir => niwjmin
      wsdir => iwjmin
      sread => splitjmin
      wread => iwjminread
    case("-nisjmax")
      nsdir => niwjmax
      wsdir => iwjmax
      sread => splitjmax
      wread => iwjmaxread
    case("-njsimin")
      nsdir => njwimin
      wsdir => jwimin
      sread => splitimin
      wread => jwiminread
    case("-njsimax")
      nsdir => njwimax
      wsdir => jwimax
      sread => splitimax
      wread => jwimaxread
    endselect
    ! check no redefinition
    if (wread .OR. sread) call cfd_error("too many "//trim(str_opt)//" definitions")
    ! set split read
    sread = .TRUE.
    ! read number of split positions
    if (iarg>nargs) call cfd_error("missing arguments after '"//trim(str_opt)//"'")
    call read_command_argument(iarg, nsdir, lincr, ierr, str_val)
    if (ierr/=0) call cfd_error("integer expected after '"//trim(str_opt)//"'"// &
                                ", found '"//trim(str_val)//"'")
    ! check array size
    if (nsdir<0 .OR. nsdir>dimspl) then
      call cfd_error("too many windows: "//trim(strof(nsdir))//" > "//trim(strnsp))
    endif
    ! read split positions array
    wsdir(1) = 0
    do ii = 2, nsdir+1
      if (iarg>nargs) call cfd_error("missing arguments after '"//trim(str_opt)//"'")
      call read_command_argument(iarg, wsdir(ii), lincr, ierr, str_val)
      print*,"wsdir("//trim(strof(ii))//") = "//trim(strof(wsdir(ii)))
      if (ierr/=0) call cfd_error("integers expected after '"//trim(str_opt)//"'"// &
                                  ", found '"//trim(str_val)//"'")
    enddo
  ! mesh types
  case ("-quad")
    ntype_stor = ntype_mesh
    ntype_mesh = mesh_quad
    if (typeread .AND. ntype_stor/=ntype_mesh) then
      call cfd_error("too many mesh types ("//trim(strof(ntype_stor))// &
                                        ", "//trim(strof(ntype_mesh))//")")
    endif
    typeread = .TRUE.
  case ("-tri")
    ntype_stor = ntype_mesh
    ntype_mesh = mesh_tri
    if (typeread .AND. ntype_stor/=ntype_mesh) then
      call cfd_error("too many mesh types ("//trim(strof(ntype_stor))// &
                                        ", "//trim(strof(ntype_mesh))//")")
    endif
    typeread = .TRUE.
  case ("-tri4")
    ntype_stor = ntype_mesh
    ntype_mesh = mesh_tri4
    if (typeread .AND. ntype_stor/=ntype_mesh) then
      call cfd_error("too many mesh types ("//trim(strof(ntype_stor))// &
                                        ", "//trim(strof(ntype_mesh))//")")
    endif
    typeread = .TRUE.
  case default
    if (fileread) then
      call cfd_error("too many filenames ("//trim(filename)// &
                                       ", "//trim(str_opt)//")")
    endif
    filename = basename(trim(str_opt), xtyext_mesh)
    fileread = .TRUE.
  endselect
enddo

! check i dimensions
if (niread) then
  ! if i-dimension was read
  spliterr = .FALSE.
  ! sum of i-window sizes on jmin must fit if read
  if (iwjminread .AND. ni/=iwjmin(niwjmin+1)) spliterr = .TRUE.
  ! sum of i-window sizes on jmax must fit if read
  if (iwjmaxread .AND. ni/=iwjmax(niwjmax+1)) spliterr = .TRUE.
else
  ! if i-dimension was not read
  ! sum of i-window sizes on jmin and jmax must fit if both read
  spliterr = iwjminread .AND. iwjmaxread .AND. &
             iwjmin(niwjmin+1)/=iwjmax(niwjmax+1)
endif
! error
if (spliterr) then
  if (niread) write(6,'(x,2a)') 'ni (read) = ',trim(strof(ni))
  if (iwjminread) then
    write(6,'(x,2a,x,$)') 'ni (jmin) = ',trim(strof(iwjmin(niwjmin+1)))
    cep = '('
    do i = 1, niwjmin+1
      write(6,'(2a,$)') cep,trim(strof(iwjmin(i)))
      cep = sep
    enddo
    write(6,'(a)') ')'
    endif
  if (iwjmaxread) then
    write(6,'(x,2a,x,$)') 'ni (jmax) = ',trim(strof(iwjmax(niwjmax+1)))
    cep = '('
    do i = 1, niwjmax+1
      write(6,'(2a,$)') cep,trim(strof(iwjmax(i)))
      cep = sep
    enddo
    write(6,'(a)') ')'
  endif
  call cfd_error("wrong sizes in I-direction")
endif

! check j dimensions
if (njread) then
  ! if j-dimension was read
  spliterr = .FALSE.
  ! sum of j-window sizes on imin must fit if read
  if (jwiminread .AND. nj/=jwimin(njwimin+1)) spliterr = .TRUE.
  ! sum of j-window sizes on imax must fit if read
  if (jwimaxread .AND. nj/=jwimax(njwimax+1)) spliterr = .TRUE.
else
  ! if j-dimension was not read
  ! sum of j-window sizes on imin and imax must fit if both read
  spliterr = jwiminread .AND. jwimaxread .AND. &
             jwimin(njwimin+1)/=jwimax(njwimax+1)
endif
! error
if (spliterr) then
  if (njread) write(6,'(x,2a)') 'nj (read) = ',trim(strof(nj))
  if (jwiminread) then
    write(6,'(x,2a,x,$)') 'nj (imin) = ',trim(strof(jwimin(njwimin+1)))
    cep = '('
    do j = 1, njwimin+1
      write(6,'(2a,$)') cep,trim(strof(jwimin(j)))
      cep = sep
    enddo
    write(6,'(a)') ')'
  endif
  if (jwimaxread) then
    write(6,'(x,2a,x,$)') 'nj (imax) = ',trim(strof(jwimax(njwimax+1)))
    cep = '('
    do j = 1, njwimax+1
      write(6,'(2a,$)') cep,trim(strof(jwimax(j)))
      cep = sep
    enddo
    write(6,'(a)') ')'
  endif
  call cfd_error("wrong sizes in J-direction")
endif

! set i-dimension if not read
if (.NOT. niread) then
  if     (iwjminread) then
    ni = iwjmin(niwjmin+1)
  elseif (iwjmaxread) then
    ni = iwjmax(niwjmax+1)
  else
    ni = ni_default
  endif
endif
! if jmin split was read then add last position
if (splitjmin) then
  niwjmin = niwjmin+1
  iwjmin(niwjmin+1) = ni
endif
! if jmax split was read then add last position
if (splitjmax) then
  niwjmax = niwjmax+1
  iwjmax(niwjmax+1) = ni
endif

! set j-dimension if not read
if (.NOT. njread) then
  if     (jwiminread) then
    nj = jwimin(njwimin+1)
  elseif (jwimaxread) then
    nj = jwimax(njwimax+1)
  else
    nj = nj_default
  endif
endif
! if imin split was read then add last position
if (splitimin) then
  njwimin = njwimin+1
  jwimin(njwimin+1) = nj
endif
! if imax split was read then add last position
if (splitimax) then
  njwimax = njwimax+1
  jwimax(njwimax+1) = nj
endif

if (fctscale) then
  if (cstscale) call cfd_error("must not mix -lx/ly and -fx/fy definitions")
  print*,'X scaling function '//trim(strx)
  call string_to_funct(strx, morphx, ierr)
  print*,'Y scaling function '//trim(stry)
  call string_to_funct(stry, morphy, ierr)
  !
  call string_to_funct("z", morphz, ierr)
endif

!------------------------------------------------------------
! default: creates a (NI_DEF)x(NJ_DEF) uniform mesh in 1x1 box
!------------------------------------------------------------

if (ntype_mesh==0) ntype_mesh = mesh_quad

select case(ntype_mesh)
case(mesh_quad)
  print*,'creating '  //trim(strof(ni))//'x'//trim(strof(nj))//' 2D quad mesh'
case(mesh_tri)
  print*,'creating 2x'//trim(strof(ni))//'x'//trim(strof(nj))//' 2D tri mesh'
case(mesh_tri4)
  print*,'creating 4x'//trim(strof(ni))//'x'//trim(strof(nj))//' 2D tri mesh'
case default
  call cfd_error("unknown mesh type")
endselect

do nn = 1, 4
  select case(nn)
  case(1)
    errdir => erriwjmin
    nsdir => niwjmin
    wsdir => iwjmin
    ndir => ni
    str_dir = 'I'
    strface = 'J-min'
  case(2)
    errdir => erriwjmax
    nsdir => niwjmax
    wsdir => iwjmax
    ndir => ni
    str_dir = 'I'
    strface = 'J-max'
  case(3)
    errdir => errjwimin
    nsdir => njwimin
    wsdir => jwimin
    ndir => nj
    str_dir = 'J'
    strface = 'I-min'
  case(4)
    errdir => errjwimax
    nsdir => njwimax
    wsdir => jwimax
    ndir => nj
    str_dir = 'J'
    strface = 'I-max'
  endselect
  errdir = .FALSE.
  if (nsdir<=1) nsdir = 1
  if (.TRUE.) then
    wsdir(      1) = 0
    wsdir(nsdir+1) = ndir
    write(6,'(x,4a,i2)') trim(str_dir),'-windows [sizes] on ', &
                         trim(strface),' boundary: ',nsdir
    do ii = 1,nsdir
      write(6,'(3x,a1,a1,i2,a3,i3,a3,i3,a3,i3,a1)') &
            trim(str_dir),'(',ii,'): ', &
                    wsdir(ii),' - ',wsdir(ii+1), &
              ' [ ',wsdir(ii+1)  -  wsdir(ii),']'
      if (wsdir(ii+1)<=wsdir(ii)) then
        errdir = .TRUE.
        print*,'    ** error **'
      endif
    enddo
  endif
enddo

!erriwjmin = .false.
!if (niwjmin/=0) then
!  iwjmin(        1) = 0
!  iwjmin(niwjmin+1) = ni
!  print*,'I-splits [sizes] on J-min boundary: '//trim(strof(niwjmin))
!  do ii = 1,niwjmin
!    erriwjmin = erriwjmin .OR. (iwjmin(ii)<=0)
    !iwjmin(ii) = iwjmin(ii) + iwjmin(ii-1)
!    print*,'    I('//trim(strof(ii))//') = '//trim(strof(iwjmin(ii)))// &
!               ' ['//trim(strof(iwjmin(ii)-iwjmin(ii-1)))//']'
!  enddo
!    print*,'    I('//trim(strof(ii))//') = '//trim(strof(iwjmin(ii)))// &
!               ' ['//trim(strof(iwjmin(ii)-iwjmin(ii-1)))//']'
!  erriwjmin = erriwjmin .OR. (ni<=iwjmin(niwjmin))
!endif

!erriwjmax = .false.
!if (niwjmax/=0) then
!  iwjmax(        1) = 0
!  iwjmax(niwjmax+1) = ni
!  print*,'I-splits [sizes] on J-max boundary: '//trim(strof(niwjmax))
!  do ii = 1, niwjmax
!    erriwjmax = erriwjmax .OR. (iwjmax(ii)<=0)
    !iwjmax(ii) = iwjmax(ii) + iwjmax(ii-1)
!    print*,'    I('//trim(strof(ii))//') = '//trim(strof(iwjmax(ii)))// &
!               ' ['//trim(strof(iwjmax(ii)-iwjmax(ii-1)))//']'
!  enddo
!    print*,'    I('//trim(strof(ii))//') = '//trim(strof(iwjmax(ii)))// &
!               ' ['//trim(strof(iwjmax(ii)-iwjmax(ii-1)))//']'
!  erriwjmax = erriwjmax .OR. (ni<=iwjmax(niwjmax))
!endif

!errjwimin = .false.
!if (njwimin/=0) then
!  jwimin(        1) = 0
!  jwimin(njwimin+2) = nj
!  print*,'J-splits [sizes] on I-min boundary: '//trim(strof(njwimin))
!  do jj = 1, njwimin+1
!    errjwimin = errjwimin .OR. (jwimin(jj)<=0)
    !jwimin(jj) = jwimin(jj) + jwimin(jj-1)
!    print*,'    J('//trim(strof(jj))//') = '//trim(strof(jwimin(jj)))// &
!               ' ['//trim(strof(jwimin(jj)-jwimin(jj-1)))//']'
!  enddo
!    print*,'    J('//trim(strof(jj))//') = '//trim(strof(jwimin(jj)))// &
!               ' ['//trim(strof(jwimin(jj)-jwimin(jj-1)))//']'
!  errjwimin = errjwimin .OR. (nj<=jwimin(njwimin+1))
!endif

!errjwimax = .false.
!if (njwimax/=0) then
!  jwimax(        1) = 0
!  jwimax(njwimax+2) = nj
!  print*,'J-splits [sizes] on I-max boundary: '//trim(strof(njwimax))
!  do jj = 1, njwimax+1
!    errjwimax = errjwimax .OR. (jwimax(jj)<=0)
    !jwimax(jj) = jwimax(jj) + jwimax(jj-1)
!    print*,'    J('//trim(strof(jj))//') = '//trim(strof(jwimax(jj)))// &
!               ' ['//trim(strof(jwimax(jj)-jwimax(jj-1)))//']'
!  enddo
!    print*,'    J('//trim(strof(jj))//') = '//trim(strof(jwimax(jj)))// &
!               ' ['//trim(strof(jwimax(jj)-jwimax(jj-1)))//']'
!  errjwimax = errjwimax .OR. (nj<=jwimax(njwimax+1))
!endif

spliterr = erriwjmin .OR. erriwjmax .OR. errjwimin .OR. errjwimax
if (spliterr) then
  print*,'***'
  if (erriwjmin) print*,'*** I-splits on I-min boundary are wrong-sized'
  if (erriwjmax) print*,'*** I-splits on I-max boundary are wrong-sized'
  if (errjwimin) print*,'*** J-splits on J-min boundary are wrong-sized'
  if (errjwimax) print*,'*** J-splits on J-max boundary are wrong-sized'
  print*,'***'
  call cfd_error("wrong splits")
endif

if (filename == "") then
  call print_help()
  call cfd_error("missing file name")
else
  filename = trim(filename)//"."//xtyext_mesh
endif

call init_ustmesh(umesh, 1)

!------------------------------
! creates VERTICES
!------------------------------
print*,'. vertices'

select case(ntype_mesh)
case(mesh_quad, mesh_tri)
  umesh%mesh%nvtex  = (ni+1)*(nj+1)
case(mesh_tri4)
  umesh%mesh%nvtex  = (ni+1)*(nj+1)+ni*nj
case default
  call cfd_error("unknown mesh type")
endselect

umesh%nvtex       = umesh%mesh%nvtex                   ! nb of vertices (redundant)

allocate(umesh%mesh%vertex(1:umesh%mesh%nvtex, 1, 1))
vertex => umesh%mesh%vertex

do i = 1, ni+1
  do j = 1, nj+1
    iv = (i-1)*(nj+1)+j
    vertex(iv, 1, 1) = v3d( (i-1)*(lx/ni), (j-1)*(ly/nj), 0._krp )
  enddo
enddo

if (fctscale) then
  print*,'    mesh morphing computation...'
  call new_fctfuncset(fctenv)
  call morph_vertex(fctenv, umesh%mesh, morphx, morphy, morphz)
  print*,'    done'
  call delete_fctfuncset(fctenv)
  call delete_fct_node(morphx)
  call delete_fct_node(morphy)
  call delete_fct_node(morphz)
endif

if (ntype_mesh == mesh_tri4) then
do i = 1, ni
  do j = 1, nj
    iva = (i-1)*(nj+1)+j   ! lower left  corner
    ivb = iva+1            ! upper left  corner
    ivc = iva  +(nj+1)     ! lower right corner
    ivd = iva+1+(nj+1)     ! upper right corner
    ivm = (ni+1)*(nj+1)+(i-1)*nj+j
    vertex(ivm, 1,1) = .25_krp*(vertex(iva, 1,1) + vertex(ivb, 1,1)   &
                              + vertex(ivc, 1,1) + vertex(ivd, 1,1) )
  enddo
enddo
endif

!------------------------------
! creates elements (iso-i lines)
!------------------------------
select case(ntype_mesh)
case(mesh_quad)
  print*,'. QUAD elements'
  itype = elem_quad4
  nelem = ni*nj
case(mesh_tri)
  print*,'. TRI elements (QUAD split into 2 TRI)'
  itype = elem_tri3
  nelem = 2*ni*nj
case(mesh_tri4)
  print*,'. TRI elements (QUAD split into 4 TRI)'
  itype = elem_tri3
  nelem = 4*ni*nj
case default
  call cfd_error("unknown mesh type")
endselect

ielem = getindex_genelemvtex(umesh%cellvtex, itype)

if (ielem /= 0) call cfd_error("element section already exists")

call addelem_genelemvtex(umesh%cellvtex)
ielem = umesh%cellvtex%nsection
call new_elemvtex(umesh%cellvtex%elem(ielem), nelem, itype)
elem => umesh%cellvtex%elem(ielem)
nvtex = elem%nvtex

do i = 1, ni
  do j = 1, nj
      ic  = (i-1)*nj+j       ! QUAD cell index
      iva = (i-1)*(nj+1)+j   ! lower left  corner | b    d |
      ivb = iva+1            ! upper left  corner |        |
      ivc = iva  +(nj+1)     ! lower right corner |        |
      ivd = iva+1+(nj+1)     ! upper right corner | a    c |
      select case(ntype_mesh)
      case(mesh_quad)
        elem%ielem   (ic)          = ic
        elem%elemvtex(ic, 1:nvtex) = (/ iva, ivc, ivd, ivb /)
      case(mesh_tri)
        elem%ielem   (2*ic-1         ) = 2*ic-1
        elem%elemvtex(2*ic-1, 1:nvtex) = (/ iva, ivc, ivb /)
        elem%ielem   (2*ic           ) = 2*ic
        elem%elemvtex(2*ic  , 1:nvtex) = (/ ivb, ivc, ivd /)
      case(mesh_tri4)
        ivm = (ni+1)*(nj+1)+(i-1)*nj+j   ! center vertex
        elem%ielem   (4*ic-3         ) = 4*ic-3
        elem%elemvtex(4*ic-3, 1:nvtex) = (/ iva, ivc, ivm /)
        elem%ielem   (4*ic-2         ) = 4*ic-2
        elem%elemvtex(4*ic-2, 1:nvtex) = (/ ivc, ivd, ivm /)
        elem%ielem   (4*ic-1         ) = 4*ic-1
        elem%elemvtex(4*ic-1, 1:nvtex) = (/ ivd, ivb, ivm /)
        elem%ielem   (4*ic           ) = 4*ic
        elem%elemvtex(4*ic  , 1:nvtex) = (/ ivb, iva, ivm /)
    endselect
  enddo
enddo

!------------------------------
! creates FACES elements (ONLY boundaries)
!------------------------------
print*,'. BC faces'

nelem = 2*(ni+nj)
nvtex = 2                                  ! faces are bars
call new(umesh%facevtex, nelem, nvtex)
umesh%nface     = nelem
umesh%nface_lim = nelem

do j = 1, nj
  umesh%facevtex%fils(   j, 1:2) = (/    j,    j+1 /)   ! IMIN faces
  iv = (i-1)*(nj+1)                                     ! bottom right corner
  umesh%facevtex%fils(nj+j, 1:2) = (/ iv+j, iv+j+1 /)   ! IMAX faces
enddo
do i = 1, ni
  umesh%facevtex%fils(2*nj   +i, 1:2) = (/ (i-1)*(nj+1)+1,     i*(nj+1)+1 /) ! JMIN faces
  umesh%facevtex%fils(2*nj+ni+i, 1:2) = (/     i*(nj+1),   (i+1)*(nj+1)   /) ! JMAX faces
enddo

!------------------------------
! BC marks
!------------------------------
bcstr = '. BC marks'
bcstr = trim(bcstr)//' (IMIN'
if (njwimin>1) bcstr = trim(bcstr)//'[1:'//trim(strof(njwimin))//']'
bcstr = trim(bcstr)//', IMAX'
if (njwimax>1) bcstr = trim(bcstr)//'[1:'//trim(strof(njwimax))//']'
bcstr = trim(bcstr)//', JMIN'
if (niwjmin>1) bcstr = trim(bcstr)//'[1:'//trim(strof(niwjmin))//']'
bcstr = trim(bcstr)//', JMAX'
if (niwjmax>1) bcstr = trim(bcstr)//'[1:'//trim(strof(niwjmax))//']'
bcstr = trim(bcstr)//')'

print*,trim(bcstr)

! create bocos
call createboco(umesh, niwjmin+niwjmax+njwimin+njwimax)   ! creates 4 boco
                                                          ! (IMIN, IMAX, JMIN, JMAX)
                                                          ! plus splits

iboco = 0
nn = 0
do idir = ichar('I'), ichar('J')                ! window of constant I or J
do iend = 1, 2                                  ! window ijMIN or ijMAX
  select case(char(idir)//strof(iend))          ! Pointers to corresponding variables
    case('I1') ; nsdir => njwimin
                 wsdir =>  jwimin
    case('I2') ; nsdir => njwimax
                 wsdir =>  jwimax
    case('J1') ; nsdir => niwjmin
                 wsdir =>  iwjmin
    case('J2') ; nsdir => niwjmax
                 wsdir =>  iwjmax
  endselect
  print '(a,$)',"    "
  do iijj = 1, nsdir                                            ! Loop on windows
    select case(iend)                             ! Create name of BC
      case(1) ; strface = char(idir)//"MIN"
      case(2) ; strface = char(idir)//"MAX"
    endselect
    if (nsdir>1) strface = trim(strface)//trim(strof(iijj))     ! Add number if several windows
    print '(1x,a,$)',trim(strface)
    iboco = iboco + 1
    ijmin = wsdir(iijj)
    ijmax = wsdir(iijj+1)
    nij = ijmax-ijmin
    call new_ustboco(umesh%boco(iboco), trim(strface), nij)     ! Create boco of size nij
    umesh%boco(iboco)%iface(1:nij) = (/ (nn+ijmin+ij, ij=1,nij) /) ! Fill boco array
  enddo
  print '()'
  nn = nn + wsdir(nsdir+1)
enddo
enddo

iboco = 0
!
print '(a,$)',"    "
do jj = 1, njwimin
  strface = "IMIN"
  if (njwimin>1) strface = "IMIN"//trim(strof(jj))
  print '(1x,a,$)',trim(strface)
  iboco = iboco + 1
  call new_ustboco(umesh%boco(iboco), trim(strface), jwimin(jj+1)-jwimin(jj))
  umesh%boco(iboco)%iface(1:jwimin(jj+1)-jwimin(jj)) = (/ (j, j=jwimin(jj)+1,jwimin(jj+1)) /)
enddo
print '()'
!
print '(a,$)',"    "
do jj = 1, njwimax
  strface = "IMAX"
  if (njwimax>1) strface = "IMAX"//trim(strof(jj))
  print '(1x,a,$)',trim(strface)
  iboco = iboco + 1
  call new_ustboco(umesh%boco(iboco), trim(strface), jwimax(jj+1)-jwimax(jj))
  umesh%boco(iboco)%iface(1:jwimax(jj+1)-jwimax(jj)) = (/ (nj+j, j=jwimax(jj)+1,jwimax(jj+1)) /)
enddo
print '()'
!
print '(a,$)',"    "
do ii = 1, niwjmin
  strface = "JMIN"
  if (niwjmin>1) strface = "JMIN"//trim(strof(ii))
  print '(1x,a,$)',trim(strface)
  iboco = iboco + 1
  call new_ustboco(umesh%boco(iboco), trim(strface), iwjmin(ii+1)-iwjmin(ii))
  umesh%boco(iboco)%iface(1:iwjmin(ii+1)-iwjmin(ii)) = (/ (2*nj+i, i=iwjmin(ii)+1,iwjmin(ii+1)) /)
enddo
print '()'
!
print '(a,$)',"    "
do ii = 1, niwjmax
  strface = "JMAX"
  if (niwjmax>1) strface = "JMAX"//trim(strof(ii))
  print '(1x,a,$)',trim(strface)
  iboco = iboco + 1
  call new_ustboco(umesh%boco(iboco), trim(strface), iwjmax(ii+1)-iwjmax(ii))
  umesh%boco(iboco)%iface(1:iwjmax(ii+1)-iwjmax(ii)) = (/ (2*nj+ni+i, i=iwjmax(ii)+1,iwjmax(ii+1)) /)
enddo
print '()'

!------------------------------------------------------------
! Create mesh file
!------------------------------------------------------------
print*
print*,'creating file '//trim(filename)
!------------------------------
! open xbin file

!call display(umesh)

call typhon_openwrite(trim(filename), deftyphon, 1)

call typhonwrite_ustmesh(deftyphon, umesh)

!------------------------------
! close file and end program

call typhon_close(deftyphon)
print*,'done.'
call print_cfdtools_tailer()

contains

subroutine print_help()
  implicit none
  print*,"command line: ty2dmesh [options] filename.tym"
  print*
  print*,"available options:"
  print*
  print*,"  -h|--help  : print this help"
  print*
  ! Dimensions
  print*,"  -nx|-ni NI : number of I-cells (ex.: -ni 128, default "//trim(strof(ni_default))//")"
  print*,"  -ny|-nj NJ : number of J-cells (ex.: -nj  64, default "//trim(strof(nj_default))//")"
  print*
  print*,"  -nx|-ni jmin|jmax NI(1)"//sep//"..."//sep//"NI(NWI) :"
  print*,"                   list of I-window lengths on jmin|jmax boundary"
  print*,"  -ny|-nj imin|imax NJ(1)"//sep//"..."//sep//"NJ(NWJ) :"
  print*,"                   list of J-window lengths on imin|imax boundary"
  print*
  ! Lengths
  print*,"  -lx LX     : domain length (ex.: -lx 1.5, default 1.0)"
  print*,"  -ly LY     : domain height (ex.: -ly 2.5, default 1.0)"
  print*
  ! Functions
  print*,"  -fx expr   : scaling function of x from [0:1] (instead of -lx)"
  print*,"  -fy expr   : scaling function of y from [0:1] (instead of -ly)"
  print*
  ! Splits
  print*,"  -nisjmin|--nisplitjmin NSI NI(1) ... NI(NSI) :"
  print*,"  -nisjmax|--nisplitjmax NSI NI(1) ... NI(NSI) :"
  print*,"  -njsimin|--njsplitimin NSJ NJ(1) ... NJ(NSJ) :"
  print*,"  -njsimax|--njsplitimax NSJ NJ(1) ... NJ(NSI) :"
  print*,"               number of I/J-dir-splits for the I/J-min/max boundary"
  print*,"               and list of split positions"
  print*
  ! Cell type
  print*,"  -quad      : generates quad (default)"
  print*,"  -tri       : generates tri  (split each quad)"
  print*,"  -tri4      : generates tri  (split each quad into 4 tri)"

  call print_cfdtools_tailer()
endsubroutine print_help

endprogram ty2dmesh
!------------------------------------------------------------------------------!
! Change history
!
! May 2010 : created, write cartesian 100x100 cells mesh of 1x1 box
! Apr 2011 : command line parameters
!------------------------------------------------------------------------------!

! Fortran 90 version of cgnslib.h : MODULE

module CGNSLIB

  integer, parameter :: cgnslen = 32
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      modes for cgns file                                          *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  integer, parameter :: MODE_READ   = 0
  integer, parameter :: MODE_WRITE  = 1
  integer, parameter :: MODE_CLOSED = 2
  integer, parameter :: MODE_MODIFY = 3

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      some error code                                               *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  integer, parameter :: ALL_OK         = 0
  integer, parameter :: ERROR          = 1
  integer, parameter :: NODE_NOT_FOUND = 2
  integer, parameter :: INCORRECT_PATH = 3

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Dimensional Units                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  integer, parameter :: Null = 0
  integer, parameter :: UserDefined = 1
  integer, parameter :: Kilogram  = 2
  integer, parameter :: Gram      = 3
  integer, parameter :: Slug      = 4
  integer, parameter :: PoundMass = 5

  character(len=cgnslen), parameter :: MassUnitsName(0:5) = &
    (/'Null                            ','UserDefined                     ',&
      'Kilogram                        ','Gram                            ',&
      'Slug                            ','PoundMass                       '/)

  integer, parameter :: Meter      = 2
  integer, parameter :: Centimeter = 3 
  integer, parameter :: Millimeter = 4
  integer, parameter :: Foot       = 5
  integer, parameter :: Inch       = 6
  character(len=cgnslen), parameter :: LengthUnitsName(0:6) = &
    (/'Null                            ','UserDefined                     ',&
      'Meter                           ','Centimeter                      ',&
      'Millimeter                      ','Foot                            ',&
      'Inch                            ' /)

  integer, parameter :: Second = 2
  character(len=cgnslen), parameter :: TimeUnitsName(0:2) = &
    (/'Null                            ','UserDefined                     ',&
      'Second                          ' /)

  integer, parameter :: Kelvin     = 2
  integer, parameter :: Celcius    = 3
  integer, parameter :: Rankine    = 4
  integer, parameter :: Fahrenheit = 5
  character(len=cgnslen), parameter :: TemperatureUnitsName(0:5) = &
    (/'Null                            ','UserDefined                     ',&
      'Kelvin                          ','Celcius                         ',&
      'Rankine                         ','Fahrenheit                      '/)

  integer, parameter :: Degree = 2
  integer, parameter :: Radian = 3
  character(len=cgnslen), parameter :: AngleUnitsName(0:3) = &
    (/'Null                            ','UserDefined                     ',&
      'Degree                          ','Radian                          '/)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Data Class                                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  character(len=cgnslen), parameter :: DataClassName(0:6) = &
    (/'Null                            ','UserDefined                     ',&
      'Dimensional                     ','NormalizedByDimensional         ',&
      'NormalizedByUnknownDimensional  ','NondimensionalParameter         ',&
      'DimensionlessConstant           ' /)
  integer, parameter :: Dimensional                    = 2
  integer, parameter :: NormalizedByDimensional        = 3
  integer, parameter :: NormalizedByUnknownDimensional = 4
  integer, parameter :: NondimensionalParameter        = 5
  integer, parameter :: DimensionlessConstant          = 6


!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Grid Location                                                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: GridLocationName(0:8) = &
    (/'Null                            ','UserDefined                     ',&
      'Vertex                          ','CellCenter                      ',&
      'FaceCenter                      ','IFaceCenter                     ',&
      'JFaceCenter                     ','KFaceCenter                     ',&
      'EdgeCenter                      '/)
  integer, parameter :: Vertex      = 2
  integer, parameter :: CellCenter  = 3
  integer, parameter :: FaceCenter  = 4
  integer, parameter :: IFaceCenter = 5
  integer, parameter :: JFaceCenter = 6
  integer, parameter :: KFaceCenter = 7
  integer, parameter :: EdgeCenter  = 8

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Grid Connectivity Types                                          *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: GridConnectivityTypeName(0:4) = &
    (/'Null                            ','UserDefined                     ',&
      'Overset                         ','Abutting                        ',&
      'Abutting1to1                    '/)
  integer, parameter :: Overset      = 2
  integer, parameter :: Abutting     = 3
  integer, parameter :: Abutting1to1 = 4

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Point Set Types                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: PointSetTypeName(0:8) = &
    (/'Null                            ','UserDefined                     ',&
      'PointList                       ','PointListDonor                  ',&
      'PointRange                      ','PointRangeDonor                 ',&
      'ElementRange                    ','ElementList                     ',&
      'CellListDonor                   ' /)
  integer, parameter :: PointList       = 2
  integer, parameter :: PointListDonor  = 3
  integer, parameter :: PointRange      = 4
  integer, parameter :: PointRangeDonor = 5
  integer, parameter :: ElementRange    = 6
  integer, parameter :: ElementList     = 7
  integer, parameter :: CellListDonor   = 8

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Governing Equations and Physical Models Types                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  integer, parameter :: FullPotential             = 2
  integer, parameter :: Euler                     = 3
  integer, parameter :: NSLaminar                 = 4
  integer, parameter :: NSTurbulent               = 5
  integer, parameter :: NSLaminarIncompressible   = 6
  integer, parameter :: NSTurbulentIncompressible = 7
  character(len=cgnslen), parameter :: GoverningEquationsTypeName(0:7) = &
    (/'Null                            ','UserDefined                     ',&
      'FullPotential                   ','Euler                           ',&
      'NSLaminar                       ','NSTurbulent                     ',&
      'NSLaminarIncompressible         ','NSTurbulentIncompressible       '/)

! Gas Model
  integer, parameter :: Ideal                       = 2
  integer, parameter :: VanderWaals                 = 3
! Viscosity model
  integer, parameter :: Constant                    = 4
! Viscosity and thermal conductivity model
  integer, parameter :: PowerLaw                    = 5
  integer, parameter :: SutherlandLaw               = 6
! Thermal conductivity model only
  integer, parameter :: ConstantPrandtl             = 7
! Turbulence closure
  integer, parameter :: EddyViscosity               = 8
  integer, parameter :: ReynoldsStress              = 9
  integer, parameter :: ReynoldsStressAlgebraic     = 10
! Turbulence model
  integer, parameter :: Algebraic_BaldwinLomax      = 11
  integer, parameter :: Algebraic_CebeciSmith       = 12
  integer, parameter :: HalfEquation_JohnsonKing    = 13
  integer, parameter :: OneEquation_BaldwinBarth    = 14
  integer, parameter :: OneEquation_SpalartAllmaras = 15
  integer, parameter :: TwoEquation_JonesLaunder    = 16
  integer, parameter :: TwoEquation_MenterSST       = 17
  integer, parameter :: TwoEquation_Wilcox          = 18
  character(len=cgnslen), parameter :: ModelTypeName(0:18) = &
    (/'Null                            ','UserDefined                     ',&
      'Ideal                           ','VanderWaals                     ',&
      'Constant                        ','PowerLaw                        ',&
      'SutherlandLaw                   ','ConstantPrandtl                 ',&
      'EddyViscosity                   ','ReynoldsStress                  ',&
      'ReynoldsStressAlgebraic         ','Algebraic_BaldwinLomax          ',&
      'Algebraic_CebeciSmith           ','HalfEquation_JohnsonKing        ',&
      'OneEquation_BaldwinBarth        ','OneEquation_SpalartAllmaras     ',&
      'TwoEquation_JonesLaunder        ','TwoEquation_MenterSST           ',&
      'TwoEquation_Wilcox              '/)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Boundary Condition Types                                         *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: BCTypeName(0:25) = &
    (/'Null                            ','UserDefined                     ',&
      'BCAxisymmetricWedge             ','BCDegenerateLine                ',&
      'BCDegeneratePoint               ','BCDirichlet                     ',&
      'BCExtrapolate                   ','BCFarfield                      ',&
      'BCGeneral                       ','BCInflow                        ',&
      'BCInflowSubsonic                ','BCInflowSupersonic              ',&
      'BCNeumann                       ','BCOutflow                       ',&
      'BCOutflowSubsonic               ','BCOutflowSupersonic             ',&
      'BCSymmetryPlane                 ','BCSymmetryPolar                 ',&
      'BCTunnelInflow                  ','BCTunnelOutflow                 ',&
      'BCWall                          ','BCWallInviscid                  ',&
      'BCWallViscous                   ','BCWallViscousHeatFlux           ',&
      'BCWallViscousIsothermal         ','FamilySpecified                 ' /)
  integer, parameter :: BCAxisymmetricWedge     = 2
  integer, parameter :: BCDegenerateLine        = 3
  integer, parameter :: BCDegeneratePoint       = 4
  integer, parameter :: BCDirichlet             = 5
  integer, parameter :: BCExtrapolate           = 6
  integer, parameter :: BCFarfield              = 7
  integer, parameter :: BCGeneral               = 8
  integer, parameter :: BCInflow                = 9
  integer, parameter :: BCInflowSubsonic        = 10
  integer, parameter :: BCInflowSupersonic      = 11
  integer, parameter :: BCNeumann               = 12
  integer, parameter :: BCOutflow               = 13
  integer, parameter :: BCOutflowSubsonic       = 14
  integer, parameter :: BCOutflowSupersonic     = 15
  integer, parameter :: BCSymmetryPlane         = 16
  integer, parameter :: BCSymmetryPolar         = 17
  integer, parameter :: BCTunnelInflow          = 18
  integer, parameter :: BCTunnelOutflow         = 19
  integer, parameter :: BCWall                  = 20
  integer, parameter :: BCWallInviscid          = 21
  integer, parameter :: BCWallViscous           = 22
  integer, parameter :: BCWallViscousHeatFlux   = 23
  integer, parameter :: BCWallViscousIsothermal = 24
  integer, parameter :: FamilySpecified         = 25

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Data types                                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: DataTypeName(0:5) = &
    (/'Null                            ','UserDefined                     ',&
      'Integer                         ','RealSingle                      ',&
      'RealDouble                      ','Character                       ' /)
  integer, parameter :: Integer    = 2
  integer, parameter :: RealSingle = 3
  integer, parameter :: RealDouble = 4
  integer, parameter :: Character  = 5


!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      BCData_t types                                                   *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: BCDataTypeName(0:3) = &
    (/'Null                            ','UserDefined                     ',&
      'Dirichlet                       ','Neumann                         ' /)
  integer, parameter :: Dirichlet = 2
  integer, parameter :: Neumann   = 3

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Element types                                                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: ElementTypeName(0:21) = &
    (/'Null                            ','UserDefined                     ',&
      'NODE                            ','BAR_2                           ',&
      'BAR_3                           ','TRI_3                           ',&
      'TRI_6                           ','QUAD_4                          ',&
      'QUAD_8                          ','QUAD_9                          ',&
      'TETRA_4                         ','TETRA_10                        ',&
      'PYRA_5                          ','PYRA_14                         ',&
      'PENTA_6                         ','PENTA_15                        ',&
      'PENTA_18                        ','HEXA_8                          ',&
      'HEXA_20                         ','HEXA_27                         ',&
      'MIXED                           ','NGON_n                          ' /)
  integer, parameter :: NODE     =  2
  integer, parameter :: BAR_2    =  3
  integer, parameter :: BAR_3    =  4
  integer, parameter :: TRI_3    =  5
  integer, parameter :: TRI_6    =  6
  integer, parameter :: QUAD_4   =  7
  integer, parameter :: QUAD_8   =  8
  integer, parameter :: QUAD_9   =  9
  integer, parameter :: TETRA_4  = 10
  integer, parameter :: TETRA_10 = 11
  integer, parameter :: PYRA_5   = 12
  integer, parameter :: PYRA_14  = 13
  integer, parameter :: PENTA_6  = 14
  integer, parameter :: PENTA_15 = 15
  integer, parameter :: PENTA_18 = 16
  integer, parameter :: HEXA_8   = 17
  integer, parameter :: HEXA_20  = 18
  integer, parameter :: HEXA_27  = 19
  integer, parameter :: MIXED    = 20
  integer, parameter :: NGON_n  = 21

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Zone types                                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: ZoneTypeName(0:3) = &
    (/'Null                            ','UserDefined                     ',&
      'Structured                      ','Unstructured                    ' /)
  integer, parameter :: Structured   =  2
  integer, parameter :: Unstructured =  3

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Rigid Grid Motion types						 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: RigidGridMotionTypeName(0:3) = &
    (/'Null                            ','UserDefined                     ',&
      'ConstantRate                    ','VariableRate                    ' /)
  integer, parameter :: ConstantRate = 2
  integer, parameter :: VariableRate = 3

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Arbitrary Grid Motion types                                      *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: ArbitraryGridMotionTypeName(0:3) = &
    (/'Null                            ','UserDefined                     ',&
      'NonDeformingGrid                ','DeformingGrid                   ' /)
  integer, parameter :: NonDeformingGrid = 2
  integer, parameter :: DeformingGrid = 3

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Simulation type							 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  character(len=cgnslen), parameter :: SimulationTypeName(0:3) = &
    (/'Null                            ','UserDefined                     ',&
      'TimeAccurate                    ','NonTimeAccurate                 ' /)
  integer, parameter :: TimeAccurate = 2
  integer, parameter :: NonTimeAccurate = 3


endmodule CGNSLIB

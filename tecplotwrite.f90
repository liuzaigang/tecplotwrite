module tecplotwrite
    !
    ! ******* Z. LIU 20181024 ******
    !
    ! Objective: Write tecplot binary (.plt) or text (.dat) data file
    ! Below is the help document of tecplot binary data structure (Version tecplot 360 "#!TDV112" )
    ! However, the code is based on version "#!TDV101"
    ! Usage:
    !
    !    1. Define data structure
    !       type(tecplotDataTemplate) :: data2Write
    !    2. Specify the data to write
    !       *(use % to access data structure)
    !       integer :: inunit
    !            - Unit of data file to Write
    !       character(len=128)  :: datatitle
    !            - Filename and title of data file to Write 
    !               ( Extensions .plt or .dat will not be generated automatically. Please define. )
    !       integer :: Nvars
    !            - Number of variables
    !       character(len=string_length), dimension(:), pointer  :: varNames
    !            - Names of variables, dimension: 1:Nvars
    !       integer :: IMax
    !       integer :: JMax
    !       integer :: KMax
    !            - Dimension of the grid
    !       real(SP), dimension(:,:,:,:), pointer :: data
    !            - data to write, single precesion, dimension: 1:IMax,1:JMax,1:KMax,1:Nvars
    !    3. Write
    !       call tecplot_write_binary(data2Write) 
    !            - to write binary data (.plt files)
    !       call tecplot_write_ascii(data2Write)
    !            - to write text data (.dat files)
    !
    ! ******* Z. LIU 20181024 ******
    ! 
    ! Binary Data File Format
    ! Refer to this section only if you wish to write your own functions. Otherwise, refer to Getting Started for instructions for linking with the library provided by Tecplot, Inc.
    ! /*
    ! BINARY FILE FORMAT:
    ! -----------------------------------------------------------------------
    ! The binary data file format (as produced by the preplot) is described below.
    ! The binary datafile has two main sections. A header section and a data
    ! section.
    !  +----------------+
    !  | HEADER SECTION |
    !  +----------------+
    !  +---------+
    !  |FLOAT32 | EOHMARKER, value=357.0
    !  +---------+
    !  +----------------+
    !  | DATA SECTION |
    !  +----------------+
    ! I. HEADER SECTION
    !  The header section contains: the version number of the file, a title
    !  of the file, the names of the variables to be plotted, the
    !  descriptions of all zones to be read in and all text and geometry
    !  definitions.
    !  i. Magic number, Version number
    !  +-----------+
    !  | “#!TDV112”| 8 Bytes, exact characters “#!TDV112”.
    !  +-----------+ Version number follows the “V” and
    !  consumes the next 3 characters (for
    !  example: “V75 “, “V101”).
    !  ii. Integer value of 1.
    !  +-----------+
    !  | INT32 | This is used to determine the byte order
    !  +-----------+ of the reader, relative to the writer.
    !  iii. Title and variable names.
    !  +-----------+
    !  | INT32 | FileType: 0 = FULL,
    !  +-----------+ 1 = GRID,
    !  2 = SOLUTION
    !  +-----------+
    !  | INT32*N | The TITLE. (See note 1.)
    !  +-----------+
    !  +-----------+
    !  | INT32 | Number of variables (NumVar) in the datafile.
    !  +-----------+
    !  +-----------+
    !  | INT32*N | Variable names.
    !  +-----------+ N = L[1] + L[2] + .... L[NumVar]
    !  where:
    !  L[i] = length of the ith variable name + 1
    !  (for the terminating 0 value).
    !  (See note 1.)
    !  iv. Zones
    !  +-----------+
    !  | FLOAT32 | Zone marker. Value = 299.0
    !  +-----------+
    !  +-----------+
    !  | INT32*N | Zone name. (See note 1.)
    !  +-----------+ N = (length of zone name) + 1.
    !  +-----------+
    !  | INT32 | ParentZone: Zero-based zone number within this
    !  +-----------+ datafile to which this zone is
    !  a child.
    !  +-----------+
    !  | INT32 | StrandID: -2 = pending strand ID for assignment
    !  +-----------+ by Tecplot 
    !  -1 = static strand ID
    !  0 <= N < 32700 valid strand ID
    !  +-----------+
    !  | FLOAT64 | Solution time.
    !  +-----------+
    !  +-----------+
    !  | INT32 | Not used. Set to -1.
    !  +-----------+
    !  +-----------+
    !  | INT32 | ZoneType 0=ORDERED, 1=FELINESEG, 
    !  +-----------+ 2=FETRIANGLE, 3=FEQUADRILATERAL,
    !  4=FETETRAHEDRON, 5=FEBRICK,
    !  6=FEPOLYGON, 7=FEPOLYHEDRON
    !  +-----------+
    !  | INT32 | Data packing.
    !  +-----------+ 0 = Block
    !  1 = Point
    !  +-----------+
    !  | INT32 | Specify Var Location. 
    !  +-----------+ 0 = Don’t specify, all data is located
    !  at the nodes.
    !  1 = Specify
    !  if “specify var location” == 1
    !  +-----------+
    !  | INT32*NV | Variable Location (only specify if above is 1). 
    !  +-----------+ 0 = Node, 1 = Cell Centered (See note 5.)
    !  +-----------+
    !  | INT32 | Are raw local 1-to-1 face neighbors supplied? 
    !  +-----------+ (0=FALSE 1=TRUE). These raw values are a 
    !  compact form of the local 1-to-1 face neighbors.
    !  If supplied, Tecplot assumes that the face 
    !  neighbors are fully specified. As such, it
    !  will not perform auto face neighbor assignment. 
    !  This improves Tecplot’s time to first plot. 
    !  See the data section below for format details. 
    !  ORDERED and FELINESEG zones must specify 0 for
    !  this value because raw face neighbors are not
    !  defined for these zone types. FEPOLYGON and
    !  FEPOLYHEDRON zones must specify 0 for this value
    !  since face neighbors are defined in the face map
    !  for these zone types.
    !  +-----------+
    !  | INT32 | Number of miscellaneous user-defined face
    !  +-----------+ neighbor connections (value >= 0). This value
    !  is in addition to the face neighbors supplied
    !  in the raw section. FEPOLYGON and FEPOLYHEDRON
    !  zones must specify 0.
    !  if “number of miscellaneous user-defined 
    !  face neighbor connections” != 0
    !  +-----------+
    !  | INT32 | User defined face neighbor mode
    !  +-----------+ (0=Local 1-to-1, 1=Local 1-to-many, 
    !  2=Global 1-to-1, 3=Global 1-to-many)
    !  if FE Zone:
    !  +-----------+
    !  | INT32 | Indicates if the finite element face neighbors
    !  +-----------+ are completely specified by the miscellaneous
    !  face neighbors given: (0=NO, 1=YES). If yes,
    !  then Tecplot will not perform auto assignment
    !  of face neighbors otherwise all faces not 
    !  specified are considered boundaries. If no,
    !  then Tecplot will perform auto-assignment of
    !  the face neighbors unless the raw face neighbor
    !  array was supplied. This option is not valid
    !  for ORDERED zones.
    !  if Ordered Zone:
    !  +-----------+
    !  | INT32*3 | IMax,JMax,KMax
    !  +-----------+
    !  if FE Zone:
    !  +-----------+
    !  | INT32 | NumPts
    !  +-----------+
    !  if ZoneType is FEPOLYGON or FEPOLYHEDRON:
    !  +-----------+
    !  | INT32 | NumFaces
    !  +-----------+ 
    !  +-----------+
    !  | INT32 | Total number of face nodes. For FEPOLYGON 
    !  +-----------+ zones, this is NumFaces*2.
    !  +-----------+
    !  | INT32 | Total number of boundary faces. If any 
    !  +-----------+ boundary faces exist, include one to represent 
    !  no neighboring element.
    !  +-----------+
    !  | INT32 | Total number of boundary connections. 
    !  +-----------+
    !  +-----------+
    !  | INT32 | NumElements
    !  +-----------+ 
    !  +-----------+
    !  | INT32*3 | ICellDim,JCellDim,
    !  +-----------+ KCellDim (for future use; set to zero) 
    !  For all zone types (repeat for each Auxiliary data name/value pair):
    !  +-----------+
    !  | INT32 | 1=Auxiliary name/value pair to follow
    !  +-----------+ 0=No more Auxiliary name/value pairs.
    !  If the above is 1, then supply the following:
    !  +-----------+
    !  | INT32*N | name string (See note 1.)
    !  +-----------+ 
    !  +-----------+
    !  | INT32 | Auxiliary Value Format 
    !  +-----------+ (Currently only allow 0=AuxDataType_String)
    !  +-----------+
    !  | INT32*N | Value string (See note 1.)
    !  +-----------+ 
    !  v. Geometries
    !  +-----------+
    !  | FLOAT32 | Geometry marker. Value = 399.0
    !  +-----------+
    !  +-----------+
    !  | INT32 | Position CoordSys 0=Grid, 1=Frame,
    !  +-----------+ 2=FrameOffset(not used),
    !  3= OldWindow(not used),
    !  4=Grid3D
    !  +-----------+
    !  | INT32 | Scope 0=Global 1=Local
    !  +-----------+
    !  +-----------+
    !  | INT32 | DrawOrder 0=After, 1=Before
    !  +-----------+
    !  +-----------+
    !  | FLOAT64*3 | (X or Theta),(Y or R),(Z or dummy) 
    !  +-----------+ i.e. the starting location
    !  +-----------+
    !  | INT32 | Zone (0=all)
    !  +-----------+
    !  +-----------+
    !  | INT32 | Color
    !  +-----------+
    !  +-----------+
    !  | INT32 | FillColor
    !  +-----------+
    !  +-----------+
    !  | INT32 | IsFilled (0=no 1=yes)
    !  +-----------+
    !  +-----------+
    !  | INT32 | GeomType 0=Line, 1=Rectangle 2=Square,
    !  +-----------+ 3=Circle, 4=ellipse
    !  +-----------+
    !  | INT32 | LinePattern 0=Solid 1=Dashed 2=DashDot 
    !  +-----------+ 3=DashDotDot 4=Dotted
    !  5=LongDash
    !  +-----------+
    !  | FLOAT64 | Pattern Length
    !  +-----------+
    !  +-----------+
    !  | FLOAT64 | Line Thickness
    !  +-----------+
    !  +-----------+
    !  | INT32 | NumEllipsePts
    !  +-----------+
    !  +-----------+
    !  | INT32 | Arrowhead Style 0=Plain, 1=Filled, 2=Hollow
    !  +-----------+
    !  +-----------+
    !  | INT32 | Arrowhead Attachment 0=None, 1=Beg, 2=End, 3=Both
    !  +-----------+
    !  +-----------+
    !  | FLOAT64 | Arrowhead Size
    !  +-----------+
    !  +-----------+
    !  | FLOAT64 | Arrowhead Angle
    !  +-----------+
    !  +-----------+
    !  | IN32*N | Macro Function Command (string: N = Length+1)
    !  +-----------+
    !  +-----------+
    !  | INT32 | Polyline Field Data Type 
    !  +-----------+ 1=Float, 2=Double (GTYPE)
    !  +-----------+
    !  | INT32 | Clipping (0=ClipToAxes, 1=ClipToViewport,
    !  +-----------+ 2=ClipToFrame)
    ! If the geometry type is line then:
    !  +-----------+
    !  | INT32 | Number of polylines
    !  +-----------+
    !  +-----------+
    !  | INT32 | Number of points, line 1.
    !  +-----------+
    !  +-----------+
    !  | GTYPE*N | X-block geometry points N=NumPts
    !  +-----------+
    !  +-----------+
    !  | GTYPE*N | Y-block geometry points N=NumPts
    !  +-----------+
    !  +-----------+
    !  | GTYPE*N | Z-block geometry points N=NumPts (Grid3D Only)
    !  +-----------+
    !  .
    !  .
    !  .
    ! If the geometry type is Rectangle then
    !  +-----------+
    !  | GTYPE*2 | X and Y offset for far corner of rectangle
    !  +-----------+
    ! If the geometry type is Circle then
    !  +-----------+
    !  | GTYPE | Radius
    !  +-----------+
    ! If the geometry type is Square then
    !  +-----------+
    !  | GTYPE | Width
    !  +-----------+
    ! If the geometry type is Ellipse then
    !  +-----------+
    !  | GTYPE*2 | X and Y Radii
    !  +-----------+
    !  vi. Text
    !  +-----------+
    !  | FLOAT32 | Text marker. Value=499.0
    !  +-----------+
    !  +-----------+
    !  | INT32 | Position CoordSys 0=Grid, 1=Frame, 
    !  +-----------+ 2=FrameOffset(not used),
    !  3= OldWindow(not used),
    !  4=Grid3D(New to V10)
    !  +-----------+
    !  | INT32 | Scope 0=Global 1=Local
    !  +-----------+
    !  +-----------+
    !  | FLOAT64*3 | (X or Theta),(Y or R),(Z or dummy)
    !  +-----------+ Starting Location
    !  +-----------+
    !  | INT32 | FontType
    !  +-----------+
    !  +-----------+
    !  | INT32 | Character Height Units 0=Grid, 1=Frame, 2=Point
    !  +-----------+
    !  +-----------+
    !  | FLOAT64 | Height of characters
    !  +-----------+
    !  +-----------+
    !  | INT32 | Text Box type 0=NoBox 1=Hollow 2=Filled
    !  +-----------+
    !  +-----------+
    !  | FLOAT64 | Text Box Margin
    !  +-----------+
    !  +-----------+
    !  | FLOAT64 | Text Box Margin Linewidth
    !  +-----------+
    !  +-----------+
    !  | INT32 | Text Box Outline Color
    !  +-----------+
    !  +-----------+
    !  | INT32 | Text Box Fill Color
    !  +-----------+
    !  +-----------+
    !  | FLOAT64 | Angle
    !  +-----------+
    !  +-----------+
    !  | FLOAT64 | Line Spacing
    !  +-----------+
    !  +-----------+
    !  | INT32 | Text Anchor. 0=left, 1=center, 
    !  +-----------+ 2=right, 3=midleft
    !  4=midcenter 5=midright,
    !  6=headleft 7=headcenter
    !  8=headright
    !  +-----------+
    !  | INT32 | Zone (0=all)
    !  +-----------+
    !  +-----------+
    !  | INT32 | Color
    !  +-----------+
    !  +-----------+
    !  | INT32*N | MacroFunctionCommand (string: N = Length + 1)
    !  +-----------+
    !  +-----------+
    !  | INT32 | Clipping (0=ClipToAxes, 
    !  +-----------+ 1=ClipToViewport, 2=ClipToFrame)
    !  +-----------+
    !  | INT32*N | Text. N=Text Length+1
    !  +-----------+
    !  vii.CustomLabel
    !  +-----------+
    !  | FLOAT32 | CustomLabel Marker; F=599
    !  +-----------+
    !  +-----------+
    !  | INT32 | Number of labels
    !  +-----------+
    !  +-----------+
    !  | INT32*N | Text for label 1. (N=length of label + 1)
    !  +-----------+ See note 1.
    !  +-----------+
    !  | INT32*N | Text for label 2. (N=length of label + 1)
    !  +-----------+ See note 1.
    !  .
    !  .
    !  .
    !  +-----------+
    !  | INT32*N | Text for label NumLabels.
    !  +-----------+ (N=length of label + 1) See note 1.
    !  viii.UserRec
    !  +-----------+
    !  | FLOAT32 | UserRec Marker; F=699
    !  +-----------+
    !  +-----------+
    !  | INT32*N | Text for UserRec. See note 1.
    !  +-----------+
    !  ix. Dataset Auxiliary data.
    !  +-----------+
    !  | FLOAT32 | DataSetAux Marker; F=799.0
    !  +-----------+
    !  +-----------+
    !  | INT32*N | Text for Auxiliary “Name”. See note 1.
    !  +-----------+
    !  +-----------+
    !  | INT32 | Auxiliary Value Format (Currently only
    !  +-----------+ allow 0=AuxDataType_String)
    !  +-----------+
    !  | INT32*N | Text for Auxiliary “Value”. See note 1.
    !  +-----------+
    !  x. Variable Auxiliary data.
    !  +-----------+
    !  | FLOAT32 | VarAux Marker; F=899.0
    !  +-----------+
    !  +-----------+
    !  | INT32*N | Variable number (zero based value)
    !  +-----------+
    !  +-----------+
    !  | INT32*N | Text for Auxiliary “Name”. See note 1.
    !  +-----------+
    !  +-----------+
    !  | INT32 | Auxiliary Value Format (Currently only
    !  +-----------+ allow 0=AuxDataType_String)
    !  +-----------+
    !  | INT32*N | Text for Auxiliary “Value”. See note 1.
    !  +-----------+
    ! II. DATA SECTION (don’t forget to separate the header from the data
    !  with an EOHMARKER). The data section contains all of the data 
    !  associated with the zone definitions in the header.
    !  i. For both ordered and fe zones:
    !  +-----------+
    !  | FLOAT32 | Zone marker Value = 299.0
    !  +-----------+
    !  +-----------+
    !  | INT32*N | Variable data format, N=Total number of vars
    !  +-----------+ 1=Float, 2=Double, 3=LongInt,
    !  4=ShortInt, 5=Byte, 6=Bit
    !  +-----------+
    !  | INT32 | Has passive variables: 0 = no, 1 = yes.
    !  +-----------+
    !  if “has passive variables” != 0
    !  +-----------+
    !  | INT32*NV | Is variable passive: 0 = no, 1 = yes
    !  +-----------+ (Omit entirely if “Has passive variables” is 0).
    !  +-----------+
    !  | INT32 | Has variable sharing 0 = no, 1 = yes. 
    !  +-----------+ 
    !  if “has variable sharing” != 0
    !  +-----------+
    !  | INT32*NV | Zero based zone number to share variable with 
    !  +-----------+ (relative to this datafile). (-1 = no sharing).
    !  (Omit entirely if “Has variable sharing” is 0).
    !  +-----------+
    !  | INT32 | Zero based zone number to share connectivity
    !  +-----------+ list with (-1 = no sharing). FEPOLYGON and
    !  FEPOLYHEDRON zones use this zone number to
    !  share face map data.
    !  Compressed list of min/max pairs for each non-shared and non-passive
    !  variable. For each non-shared and non-passive variable (as specified
    !  above):
    !  +-----------+
    !  | FLOAT64 | Min value
    !  +-----------+
    !  +-----------+
    !  | FLOAT64 | Max value
    !  +-----------+
    !  +-----------+
    !  | xxxxxxxxxx| Zone Data. Each variable is in data format as
    !  +-----------+ specified above.
    !  ii. specific to ordered zones
    !  if “zone number to share connectivity list with” == -1 &&
    !  “num of misc. user defined face neighbor connections” != 0
    !  +-----------+
    !  | INT32*N | Face neighbor connections.
    !  +-----------+ N = (number of miscellaneous user defined
    !  face neighbor connections) * P 
    !  (See note 5 below).
    !  iii. specific to fe zones
    !  if ZoneType is NOT FEPOLYGON or FEPOLYHEDRON:
    !  if “zone number to share connectivity lists with” == -1
    !  +-----------+
    !  | INT32*N | Zone Connectivity Data N=L*JMax 
    !  +-----------+ (see note 2 below ).
    !  if “zone number to share connectivity lists with” == -1 &&
    !  “raw local 1-to-1 face neighbors are supplied”
    !  +-----------+
    !  | INT32*N | Raw local 1-to-1 face neighbor array.
    !  +-----------+ N = (NumElements * NumFacesPerElement)
    !  (See note 3 below).
    !  if “zone number to share connectivity lists with” == -1 &&
    !  “num of misc. user defined face neighbor connections” != 0
    !  +-----------+
    !  | INT32*N | Face neighbor connections.
    !  +-----------+ N = (number of miscellaneous user defined
    !  face neighbor connections) * P 
    !  (See note 4 below).
    !  if ZoneType is FEPOLYGON or FEPOLYHEDRON:
    !  if “zone number to share face map data with” == -1
    !  +-----------+
    !  | INT32*F | Face node offsets into the face nodes array
    !  +-----------+ below. Does not exist for FEPOLYGON zones.
    !  F = NumFaces+1.
    !  +-----------+
    !  | INT32*FN | Face nodes array containing the node numbers
    !  +-----------+ for all nodes in all faces.
    !  FN = total number of face nodes.
    !  +-----------+
    !  | INT32*F | Elements on the left side of all faces. 
    !  +-----------+ Boundary faces use a negative value which is
    !  the negated offset into the face boundary 
    !  connection offsets array. A value of “-1” 
    !  indicates there is no left element.
    !  F = NumFaces.
    !  +-----------+
    !  | INT32*F | Elements on the right side of all faces. See
    !  +-----------+ description of left elements above for more
    !  details. F = NumFaces.
    !  if “total number of boundary faces” != 0
    !  +-----------+
    !  | INT32*NBF | Boundary face connection offsets into the 
    !  +-----------+ boundary face connecion elements array and
    !  the boundary face connection zones array.
    !  The number of elements for a face (F) is 
    !  determined by offset[-o] - offset[-o-1]
    !  where ‘o’ is the negative value from either
    !  the left or right elements arrays above. 
    !  Offset[0] = 0. Offset[1] = 0 so that -1 as
    !  the left or right element always indicates
    !  no neighboring element. If the number of 
    !  elements is 0, then there is no neighboring
    !  element. 
    !  NBF = total number of boundary faces + 1. 
    !  +-----------+
    !  | INT32*NBI | Boundary face connection elements. A value of 
    !  +-----------+ “-1” indicates there is no element on part of 
    !  the face.
    !  NBI = total number of boundary connections.
    !  +-----------+
    !  | INT32*NBI | Boundary face connection zones. A value of 
    !  +-----------+ “-1” indicates the current zone. 
    !  NBI = total number of boundary connections.
    ! NOTES:
    ! 1. All character data is represented by INT32 values.
    !  Example: The letter “A” has an ASCII value of 65. The WORD
    !  written to the data file for the letter “A” is then
    !  65. In fortran this could be done by doing the following:
    !  Integer*32 I
    !  .
    !  .
    !  I = ICHAR(‘A’);
    !  WRITE(10) I
    !  All character strings are null terminated 
    !  (i.e. terminated by a zero value)
    ! 2. This represents JMax sets of adjacency zero based indices where each
    !  set contains L values and L is
    !  2 for LINESEGS
    !  3 for TRIANGLES
    !  4 for QUADRILATERALS
    !  4 for TETRAHEDRONS
    !  8 for BRICKS
    ! 3. The raw face neighbor array is dimensioned by (number of elements for
    !  the zone) times (the number of faces per element), where each member
    !  of the array holds the zero-based element neighbor of that face. A
    !  boundary face is one that has no neighboring element and is 
    !  represented by a -1. Faces should only be neighbors if they logically
    !  share nodes and they should be reciprocal.
    ! 4. FaceNeighbor Mode # values Data
    !  ---------------------------------------------------------------------
    !  LocalOneToOne 3 cz,fz,cz
    !  LocalOneToMany nz+4 cz,fz,oz,nz,cz1,cz2,...,czn
    !  GlobalOneToOne 4 cz,fz,ZZ,CZ
    !  GlobalOneToMany 2*nz+4 cz,fz,oz,nz,ZZ1,CZ1,ZZ2,CZ2,...,ZZn,CZn
    !  Where:
    !  cz = cell in current zone (zero based)
    !  fz = face of cell in current zone (zero based)
    !  oz = face obscuration flag (only applies to one-to-many):
    !  0 = face partially obscured
    !  1 = face entirely obscured
    !  nz = number of cell or zone/cell associations
    !  (only applies to one-to-many)
    !  ZZ = remote Zone (zero based)
    !  CZ = cell in remote zone (zero based)
    !  cz,fz combinations must be unique and multiple entries are
    !  not allowed. Additionally, Tecplot assumes that with the
    !  one-to-one face neighbor modes, a supplied cell face is
    !  entirely obscured by its neighbor. With one-to-many, the
    !  obscuration flag must be supplied.
    !  Face neighbors that are not supplied are run through
    !  Tecplot’s auto face neighbor generator (FE only).
    ! 5. Cell centered variable (DATA SECTION)
    !  To make reading of cell centered binary data efficient, Tecplot stores
    !  IMax*JMax*KMax numbers of cell centered values, where IMax, JMax,
    !  and KMax represent the number of points in the I, J, and K directions.
    !  Therefore extra zero values (ghost values) are written to the data file
    !  for the slowest moving indices. For example, if your data’s IJK 
    !  dimensions are 2x3x2, a cell-centered variable will have 1x2x1 
    !  (i.e. (I-1)x(J-1)x(K-1)) significant values. However, 2x3x2 values must
    !  be written out because it must include the ghost values. Assume that the
    !  two significant cell-centered values are 1.5 and 12.5. The ghost values
    !  will be output with a zero value.
    !  So if the zone was dimensioned 2x3x2 its cell centered variable would be
    !  represented as follows:
    !  1.5 0.0 12.5 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
    !  If the zone was dimensioned 3x2x2 its cell centered variable would be
    !  represented as follows:
    !  1.5 12.5 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
    !  and if the zone was dimensioned 2x2x3 its cell centered variable would be
    !  represented as follows:
    !  1.5 0.0 0.0 0.0 12.5 0.0 0.0 0.0 0.0 0.0 0.0 0.0
    !  For large variables the wasted space is less significant that it
    !  is for the small example above.
    ! -----------------------------------------------------------------------
    ! */
    !
    ! Now begins the code --lzg
    implicit none
    private

    integer, parameter :: SP = 4
    integer, parameter :: WP = 8
    integer, parameter :: string_length = 16
    

    integer :: iunitWrite
    
    type tecplotDataTemplate
        integer :: inunit
        character(len=128)  :: datatitle
        integer :: Nvars
        character(len=string_length), dimension(:), pointer  :: varNames
        !real(SP) :: SolutionTime
        integer :: IMax
        integer :: JMax
        integer :: KMax
        !integer :: NumElements
        real(SP), dimension(:,:,:,:), pointer :: data
    end type tecplotDataTemplate
    
    public :: tecplotDataTemplate, tecplot_write_binary, tecplot_write_ascii


    contains

    ! *************** IMPORTANT NOTE ************
    ! If FORM='UNFORMATTED', each record is preceded and terminated with an INTEGER*4 count, 
    ! making each record 8 characters longer than normal. This convention is not shared with 
    ! other languages, so it is useful only for communicating between FORTRAN programs.
    ! Binary sequential files are sequences of bytes with no internal structure. 
    ! There are no records. The file contains only the information specified as I/O list 
    ! items in WRITE statements referring to the file.
    ! *************** IMPORTANT NOTE ************

    subroutine WriteString(string2write)
        implicit none 
        character(len=*) :: string2write
        integer :: length
        integer :: i
        integer*4 :: ascNum

        length = len_trim(string2write)

        do i = 1, length
            ascNum = ichar(string2write(i:i))
            write(iunitWrite) ascNum
        end do

        write(iunitWrite) 0 !char(0)

    end subroutine WriteString

    subroutine tecplot_create_file_binary (data2Write)
        implicit none 
        type(tecplotDataTemplate) :: data2Write

        iunitWrite = data2Write%inunit
        open(iunitWrite, file=trim(adjustL(data2Write%datatitle)), form='binary',action='write')

    end subroutine tecplot_create_file_binary

    subroutine tecplot_create_file_ascii (data2Write)
        implicit none 
        type(tecplotDataTemplate) :: data2Write

        iunitWrite = data2Write%inunit
        open(iunitWrite, file=trim(adjustL(data2Write%datatitle)), form='formatted',action='write')

    end subroutine tecplot_create_file_ascii

    subroutine tecplot_write_header_binary (data2Write)
        implicit none 
        type(tecplotDataTemplate) :: data2Write
        integer :: i

        ! i. Magic number, Version number
        ! exact characters, not string, no ending
        !call WriteString(8, "#!TDV112") 
        !write(iunitWrite) "#!TDV112"
        write(iunitWrite) "#!TDV101" ! this version is more concise

        ! ii. Integer value of 1.
        write(iunitWrite) 1

        ! iii. Title and variable names.
        ! ("#!TDV112") write(iunitWrite) 0 ! FileType: 0 = FULL, 1 = GRID, 2 = SOLUTION
        ! The TITLE.
        call WriteString(trim(adjustL(data2Write%datatitle))) 

        ! Number of variables (NumVar) in the datafile.
        write(iunitWrite) data2Write%Nvars

        ! Variable names.
        do i = 1, data2Write%Nvars
            call WriteString(data2Write%varNames(i)) 
        end do

        ! Zone marker. Value = 299.0
        write(iunitWrite) 299.0_SP

        ! Zone name.
        call WriteString(trim(adjustL(data2Write%datatitle))) 

        ! ParentZone: Zero-based zone number within this datafile to which this zone is a child.
        ! ("#!TDV112") write(iunitWrite) 0

        ! StrandID: -2 = pending strand ID for assignment by Tecplot 
        ! -1 = static strand ID 
        ! 0 <= N < 32700 valid strand ID
        ! ("#!TDV112") write(iunitWrite) -2

        ! Solution time.
        ! ("#!TDV112") write(iunitWrite) data2Write%SolutionTime

        ! Not used. Set to -1. 
        write(iunitWrite) -1

        ! ZoneType 
        ! 0=ORDERED, 
        ! 1=FELINESEG, 
        ! 2=FETRIANGLE, 
        ! 3=FEQUADRILATERAL,
        ! 4=FETETRAHEDRON, 
        ! 5=FEBRICK,
        ! 6=FEPOLYGON, 
        ! 7=FEPOLYHEDRON
        write(iunitWrite) 0

        ! Data packing. 0 = Block 1 = Point
        write(iunitWrite) 0

        ! Specify Var Location. 0 = Don’t specify, all data is located at the nodes.
        write(iunitWrite) 0

        ! if “specify var location” == 1 
        ! {

            ! Variable Location (only specify if above is 1). 0 = Node, 1 = Cell Centered (See note 5.)
            !write(iunitWrite) 0
        ! }

        ! Are raw local 1-to-1 face neighbors supplied? 
        ! ORDERED and FELINESEG zones must specify 0 for this value
        ! ("#!TDV112") write(iunitWrite) 0

        ! Number of miscellaneous user-defined face neighbor connections (value >= 0). 
        write(iunitWrite) 0

        ! if “number of miscellaneous user-defined face neighbor connections” != 0 

            ! User defined face neighbor mode
            !write(iunitWrite) 0
            ! if FE zone

                !Indicates if the finite element face neighbors are completely specified by the miscellaneous face neighbors given: (0=NO, 1=YES). 
                !write(iunitWrite) 0

        ! if Ordered Zone:
            ! IMax,JMax,KMax
            write(iunitWrite) data2Write%IMax
            write(iunitWrite) data2Write%JMax
            write(iunitWrite) data2Write%KMax

        ! if FE zone
            ! NumPts
            !write(iunitWrite) NumPts
            ! if ZoneType is FEPOLYGON or FEPOLYHEDRON:
                !...

        ! NumElements
        ! ("#!TDV112") write(iunitWrite)    data2Write%NumElements

        ! ICellDim,JCellDim,KCellDim (for future use; set to zero)
        ! ("#!TDV112")write(iunitWrite) 0
        ! ("#!TDV112")write(iunitWrite) 0
        ! ("#!TDV112")write(iunitWrite) 0

        ! For all zone types (repeat for each Auxiliary data name/value pair):
        ! 1=Auxiliary name/value pair to follow 0=No more Auxiliary name/value pairs.
        write(iunitWrite) 0

        ! If the above is 1, then supply the following:
            ! name string (See note 1.)
            ! Auxiliary Value Format
            ! Value string

        ! v. Geometries
        ! ...

        ! vi. Text
        ! ...

        ! vii.CustomLabel
        ! ...

        ! viii.UserRec
        ! ...

        ! ix. Dataset Auxiliary data.
        ! ...

        ! x. Variable Auxiliary data.
        ! ...

    end subroutine tecplot_write_header_binary


    subroutine tecplot_write_data_binary (data2Write)
        implicit none 
        type(tecplotDataTemplate) :: data2Write
        integer :: i, j ,k, n

        ! i. For both ordered and fe zones:

        ! Zone marker Value = 299.0
        write(iunitWrite) 299.0_SP

        ! Variable data format
        ! 1=Float, 2=Double, 3=LongInt, 4=ShortInt, 5=Byte, 6=Bit  
        do n = 1, data2Write%Nvars
            write(iunitWrite) 1
        end do

        ! Has passive variables: 0 = no, 1 = yes.
        ! ("#!TDV112")write(iunitWrite) 0

        ! Has variable sharing 0 = no, 1 = yes.
        write(iunitWrite) 0

        ! Zero based zone number to share connectivity list with (-1 = no sharing).
        write(iunitWrite) -1 

        ! Zone Data.
        do n = 1, data2Write%Nvars
            write(*,*) 'Writing variable: '// trim(adjustL(data2Write%varNames(n)))
            write(iunitWrite) data2Write%data(1:data2Write%IMax,1:data2Write%JMax,1:data2Write%KMax,n)
            ! do k = 1, data2Write%KMax
            !     do j = 1, data2Write%JMax
            !         do i = 1, data2Write%IMax
            !             write(iunitWrite) data2Write%data(i,j,k,n)
            !         end do
            !     end do
            ! end do
        end do

        ! ii. specific to ordered zones
        ! if “zone number to share connectivity list with” == -1 && “num of misc. user defined face neighbor connections” != 0

    end subroutine tecplot_write_data_binary

    subroutine tecplot_write_binary(data2Write)
        implicit none 
        type(tecplotDataTemplate) :: data2Write

        call tecplot_create_file_binary (data2Write)

        ! write tecplot files
        write(*,*) 'Writing tecplot binary file .. '

        call tecplot_write_header_binary (data2Write)

        ! EOHMARKER
        write(iunitWrite) 357.0_SP

        call tecplot_write_data_binary (data2Write)

        close(iunitWrite)

        write(*,*) 'Success '

    end subroutine tecplot_write_binary

    subroutine tecplot_write_ascii(data2Write)
        implicit none 
        type(tecplotDataTemplate) :: data2Write
        integer :: i,j,k,n
        integer :: CountInLine
    
        ! ** Open the tecplot file to write **
        call tecplot_create_file_ascii (data2Write)
       
        ! write tecplot files
        write(*,*) 'Writing tecplot ascii file .. '
      
        write(iunitWrite, "(A)") 'TITLE = "'//trim(adjustL(data2Write%datatitle))//'"' 
        
        write(iunitWrite, "(A,$)") 'VARIABLES = '
        do n = 1, data2Write%Nvars
            write(iunitWrite, "(A)") trim(adjustL(data2Write%varNames(n)))
        end do
      
        write(iunitWrite, "(3(A,I10))") 'ZONE T="'//trim(adjustL(data2Write%datatitle))//'", I=',data2Write%IMax, &
                                        ', J=',data2Write%JMax,', K=',data2Write%KMax
        write(iunitWrite,"(A)") 'F=BLOCK' !'F=POINT'! 
        
        CountInLine = 0
        do n = 1, data2Write%Nvars
            write(*,*) 'Writing variable: '// trim(adjustL(data2Write%varNames(n)))
            write(iunitWrite, "(20ES18.9)") data2Write%data(1:data2Write%IMax,1:data2Write%JMax,1:data2Write%KMax,n)
            ! do k = 1, data2Write%KMax
            !     do j = 1, data2Write%JMax
            !         do i = 1, data2Write%IMax
            !             write(iunitWrite, "(ES18.9, $)") data2Write%data(i,j,k,n)
            !             CountInLine = CountInLine + 1
            !             if (CountInLine .ge. 20) then
            !                 write(iunitWrite, *)
            !                 CountInLine = 0
            !             end if
            !         end do
            !     end do
            ! end do
        end do

        close(iunitWrite)

        write(*,*) 'Success '

        write(*,*) 'Compress data ...'

        call system("tar -zcvf "//trim(adjustL(data2Write%datatitle))//".tar.gz "//trim(adjustL(data2Write%datatitle)))
        
      end subroutine tecplot_write_ascii

end module tecplotwrite
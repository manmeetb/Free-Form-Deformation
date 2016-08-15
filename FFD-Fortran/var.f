	MODULE VAR

	CHARACTER(40) :: SolidBoundaryPointsFile
	CHARACTER(40) :: SolidBoundaryPointsFile1
	CHARACTER(40) :: SolidBoundaryPointsFile2

	CHARACTER(40) :: SolidBoundaryPointsOutputFileInitial
	CHARACTER(40) :: SolidBoundaryPointsOutputFileFinal
	CHARACTER(40) :: InputFile
	CHARACTER(40) :: OutputFile
	SAVE SolidBoundaryPointsFile, SolidBoundaryPointsOutputFileInitial 
	SAVE SolidBoundaryPointsOutputFileFinal, InputFile, OutputFile
		
	! creating an array that will hold the x,y and z values of the solid boundary points. 
	! The array will be made in the following format:
	! A(BodyIndex, SolidBodyPointIndex, 7), where BodyIndex is the index of the body of interest and
	! SolidBodyPointIndex is the index of the point in question for the body. Also, the 1-3 positions	
	! will hold the x,y and z coordinates and the positons 4-6 will hold the T,U,V coordinates of the
	! point and 7 holds the label of the point. For wings, this label explaings whether the point
	! is on the leading edge, trailing edge, wing tip or wing root
	! . This array will be analagous to the SolidBoundaryPointArray in the python program. Also,
	! when multiple elements are being used, SolidBodyPointIndex is the going to be the max 
	! number of solid bnd pts of one of the elements.
		

	! The array for holding all the solid boundary points. The format will be 
	! ALLSBPts(SolidPointIndex, 8), where the 7 holds for 1-3 (x,y,z), 4-6 (t,u,v)
	! 7 = point label, 8 = extra
	REAL*8, ALLOCATABLE::SolidBoundaryPoints(:,:)
	INTEGER :: SolidBoundaryPointsSize
	
	! This array will hold the number of points for each element. The way the datastructure is
	! set up is NumSolidBoundaryPoints(index of the Element, number of solid boundary points
	! for that element)
	INTEGER, ALLOCATABLE::NumSolidBoundaryPoints(:,:)
	INTEGER :: NumElements	! the number of elements	
	! Create a multidimensional array datastructure that will hold the different z slices
	! and their information. This information includes the max and min x,y and z values of each cross section
	! The data is stored in the following format: ZCrossSectionsData(BodyIndex, CrossSectionIndex, 6)
	! Where 1-2 = x limits (xmin, xmax), 3-4 = y limits (ymin, ymax), 5-6 = z limits 
	! (zmin, zmax)
	! In addition, ZCrossSectionsSize is an array which holds the number of cross sections
	! for each element (so that we know how far to loop when moving through the ZCrossSections
	! Data array
	REAL*8, ALLOCATABLE::ZCrossSectionsData(:,:,:)
	INTEGER, ALLOCATABLE :: ZCrossSectionsSize(:,:)


! This data structure holds the data for each cross section for a specific body. It will
! have the following format: CrossSectionData(BodyIndex, SliceIndex, 6). The 6 have the 
! following values: 1-2 is for xmax, xmin, and so on.
	REAL*8, ALLOCATABLE::CrossSectionsData(:,:,:)
! Holds the size of the the cross section data (ie the number of slices that were studied
! for each body. So the paramter it takes is body index
	INTEGER, ALLOCATABLE::CrossSectionsSize(:) 

! The axis direction to be used. 
	INTEGER, ALLOCATABLE :: AxisDirection(:)
! The number of slices of the object to study
	INTEGER, ALLOCATABLE :: NumSlices(:)
! The data strucutre that lets us know whether a box should be 
! placed or not
	INTEGER, ALLOCATABLE :: FFDBoolean(:)


	! The ffd points array has the following format. FFDPoints(BodyIndex,I,J,K, 3), where
	! the BodyIndex is for the body of question, (I,J,K) are for the local coordinates of the point 
! for the given body (they start from 1 not 0) and the 1-3 numbers are the x,y,z values of the index. 

	REAL*8, ALLOCATABLE::FFDPoints(:,:,:,:,:)

! The number of FFD points in each direction
	INTEGER, ALLOCATABLE :: NXFFD(:)
	INTEGER, ALLOCATABLE :: NYFFD(:)
	INTEGER, ALLOCATABLE :: NZFFD(:)
	! Used for allocating the FFD points array
	INTEGER :: MaxNXFFD, MaxNYFFD, MaxNZFFD

	SAVE MaxNXFFD, MaxNYFFD, MaxNZFFD

	INTEGER :: FFDPointsSize
	REAL*8 :: CONST_FFDXMax, CONST_FFDXMin, CONST_FFDYMax, CONST_FFDYMin
	REAL*8 :: CONST_FFDZMax, CONST_FFDZMin
	
	SAVE CONST_FFDXMax, CONST_FFDXMin, CONST_FFDYMax, CONST_FFDYMin, 
     . 	CONST_FFDZMax, CONST_FFDZMin

	

! The maximum and minimum Z values for the object in question. This
! must be an array because we must store the data for each element
	REAL*8, ALLOCATABLE :: MaxZ(:,:)
	REAL*8, ALLOCATABLE :: MinZ(:,:)

! The maximum and minimum x,y and z values for each element. The data structure
! has the following form MaxValueElem(numElements,3) where 1-3 are for the max x
! y and z values.
	REAL*8, ALLOCATABLE :: MaxValueElem(:,:)
	REAL*8, ALLOCATABLE :: MinValueElem(:,:)
	

! The FFDBoxLimits
	REAL*8 :: FFDZMax, FFDZMin, FFDXMaxEpsilon 
	REAL*8 :: FFDXMinEpsilon, FFDYMaxEpsilon, FFDYMinEpsilon
	
! Holds the information about the FFD Box. This array will have the following
! properties: FFDBoxProperties(BodyIndex,NZFFD, 5), where the 5 is for z, 
! xMax, xMin, yMax and yMin for the given cross section. 
	REAL*8, ALLOCATABLE::FFDBoxProperties(:,:,:)

! Holds the properties of the specific FFD volume for the body. Holds the following in
! the data structure: FFDVolProperties(BodyIndex, numPlanes, 7). Here, the first value 
! will hold the location of the plane along the axis chosen and the 6 values hold
! the max and min x,y and z values for the plane (two of these will be unused)   
	REAL*8, ALLOCATABLE::FFDVolProperties(:,:,:)	
	
! The information for each element based on its runcard. The dimension will be 
! ElementRuncard(numElements, 2), 1 = axis direction, 2 = NXFFD, 3 = NYFFD, 
! 4 = NZFFD, 5 = number of planes along axis direction

	SAVE WingMaxZ, WingMinZ, FFDZMax, FFDZMin, FFDXMaxEpsilon
	SAVE FFDXMinEpsilon, FFDYMaxEpsilon, FFDYMinEpsilon


	END MODULE VAR

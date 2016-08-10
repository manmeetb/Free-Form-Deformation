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
		
		REAL*8, ALLOCATABLE::SolidBoundaryPoints(:,:,:)
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


	! The ffd points array has the following format. FFDPoints(BodyIndex,I,J,K, 3), where
	! the BodyIndex is for the body of question, (I,J,K) are for the local coordinates of the point 
! for the given body (they start from 1 not 0) and the 1-3 numbers are the x,y,z values of the index. 

	REAL*8, ALLOCATABLE::FFDPoints(:,:,:,:,:)

! The number of FFD points in each direction
	INTEGER :: NXFFD
	INTEGER :: NYFFD
	INTEGER :: NZFFD

	SAVE NXFFD, NYFFD, NZFFD

	INTEGER :: FFDPointsSize
	REAL*8 :: CONST_FFDXMax, CONST_FFDXMin, CONST_FFDYMax, CONST_FFDYMin
	REAL*8 :: CONST_FFDZMax, CONST_FFDZMin
	
	SAVE CONST_FFDXMax, CONST_FFDXMin, CONST_FFDYMax, CONST_FFDYMin, 
     . 	CONST_FFDZMax, CONST_FFDZMin

	

! The maximum and minimum Z values for the object in question. This
! must be an array because we must store the data for each element
	REAL*8, ALLOCATABLE :: MaxZ(:,:)
	REAL*8, ALLOCATABLE :: MinZ(:,:)

! The FFDBoxLimits
	REAL*8 :: FFDZMax, FFDZMin, FFDXMaxEpsilon 
	REAL*8 :: FFDXMinEpsilon, FFDYMaxEpsilon, FFDYMinEpsilon
	
! Holds the information about the FFD Box. This array will have the following
! properties: FFDBoxProperties(BodyIndex,NZFFD, 5), where the 5 is for z, 
! xMax, xMin, yMax and yMin for the given cross section. 
	REAL*8, ALLOCATABLE::FFDBoxProperties(:,:,:)
	
	SAVE WingMaxZ, WingMinZ, FFDZMax, FFDZMin, FFDXMaxEpsilon
	SAVE FFDXMinEpsilon, FFDYMaxEpsilon, FFDYMinEpsilon


	END MODULE VAR

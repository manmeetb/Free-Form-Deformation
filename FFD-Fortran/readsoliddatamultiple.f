
! This subroutine will be in charge of reading the data for when 
! multiple elements are present

	SUBROUTINE READSOLIDDATAMULTIPLE()
! use the global variable module
	USE VAR

	! A dummy string used when reading the files	
	CHARACTER(40) :: Dummy
		
! Open the file that needs to be read:
!	WRITE(*,*) "File: ", SolidBoundaryPointsFile
	
	OPEN(UNIT = 14, FILE = SolidBoundaryPointsFile)

! The file has the following format:
!
!- Num Elements
!- element with maximum number of points (for fortran when creating arrays)
!- Num Points Element 1
!    (list points)
!- Num Points Element 2
!    (list points)
!...


	! first read how many elements there are
	intNumElements = 0
	READ(14,*,IOSTAT=io), intNumElements
	NumElements = intNumElements	


	! read the maximum number of points possible for an element
	intMaxPts = 0
	READ(14,*,IOSTAT=io), intMaxPts

	! allocate the solid boundary points array that will be 
	! used for holding all the points
	ALLOCATE(SolidBoundaryPoints(intNumElements, intMaxPts, 7))

	! allocate the array for the number of solid body points for each
	! element
	ALLOCATE(NumSolidBoundaryPoints(intNumElements, 1)) 

	!read in the data now for each element
	DO 10 intI = 1, NumElements
	intNumPts = 0

	!Allocate the arrays now to fill in all the data	

	READ(14,*,IOSTAT=io), intNumPts
	!WRITE(*,*) "num points: ", intNumPts	
	!Store the number of points for the ith element
	NumSolidBoundaryPoints(intI, 1) = intNumPts
	DO 20 intJ = 1, intNumPts

		READ(14,*, IOSTAT = io), rx,ry,rz, rl
		SolidBoundaryPoints(intI,intJ,1) = rx	
		SolidBoundaryPoints(intI,intJ,2) = ry	
		SolidBoundaryPoints(intI,intJ,3) = rz
		
		!T,U,V data set to 0 initially
		SolidBoundaryPoints(intI,intJ,4) = 0	
		SolidBoundaryPoints(intI,intJ,5) = 0	
		SolidBoundaryPoints(intI,intJ,6) = 0	

		! the label value for the point
		SolidBoundaryPoints(intI,intJ,7) = rl	
	
  20	CONTINUE
  10	CONTINUE	
	
	CLOSE(14) !Close the file after the number of points has now been computed


	! Read the runcard file 
	! In the variables, RC is for runcard	
	OPEN(UNIT = 15, FILE = "runcard.txt")

	! The first 5 lines do not contain information
	DO 21 intI = 1,5
	READ(15,*,IOSTAT=io), Dummy
	!WRITE(*,*) "Dummy: ", Dummy
  21	CONTINUE


	READ(15,*,IOSTAT=io), intNumElementsRC
	! Allocate the NFFD arrays and the axis array
	
	ALLOCATE(NXFFD(NumElements))
	ALLOCATE(NYFFD(NumElements))
	ALLOCATE(NZFFD(NumElements))
	ALLOCATE(AxisDirection(NumElements))
	ALLOCATE(NumSlices(NumElements))	

	! Read the file and load the data for each element
	DO 30 intI = 1, intNumElementsRC
	READ(15,*,IOSTAT=io), intH
	READ(15,*,IOSTAT=io), int1, int2, int3
	READ(15,*,IOSTAT=io), int4
	READ(15,*,IOSTAT=io), int5

	NXFFD(intH) = int1
	NYFFD(intH) = int2
	NZFFD(intH) = int3
	AxisDirection(intH) = int4
	NumSlices(intH) = int5
  30	CONTINUE
	
	CLOSE(15)	


	intMaxNXFFD = 0
	intMaxNYFFD = 0
	intMaxNZFFD = 0

	! Determine the maximum number of NXFFD, NYFFD and 
	! NZFFD points 
	DO 40 intH = 1,NumElements
	intMaxNXFFD = MAX(intMaxNXFFD,NXFFD(intH))
	intMaxNYFFD = MAX(intMaxNYFFD,NYFFD(intH))
	intMaxNZFFD = MAX(intMaxNZFFD,NZFFD(intH))		
  40	CONTINUE	

	MaxNXFFD = intMaxNXFFD
	MaxNYFFD = intMaxNYFFD
	MaxNZFFD = intMaxNZFFD

	
	END





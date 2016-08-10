
! This subroutine will be in charge of reading the data for when 
! multiple elements are present

	SUBROUTINE READSOLIDDATAMULTIPLE()
! use the global variable module
	USE VAR

		
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
	
	

	END


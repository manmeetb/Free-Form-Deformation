
! The subroutine that is used for loading the initial FFD data.
! This data is what is output into the "initial" files by the 
! FFD grid generation code.

	SUBROUTINE LOADFFDINITIALDATA()
	USE VAR

	CHARACTER (len=200) :: Line1, Line2, Line3

	intNumElements = 0
	intElementIndex = 0
	intNumFFDPoints = 0


	! Open the file with all the initial ffd data
	OPEN(UNIT = 14, FILE = SolidBoundaryPointsOutputFileInitial)

	! First read the number of elements
	READ(14,*, IOSTAT = io) Line1, Line2

	READ(Line2, '(i5)') intNumElements

	WRITE(*,*) "NumElements: ", intNumElements

	! Now first do a run through of the whole file to find
	! whats the maximum number of solid body points of all the
	! elements

!	DO 10 intI = 1, intNumElements
	! For each element iterate through the file
	
	! Read the index of the element
!	READ(14,*,IOSTAT = io) Line1, Line2	
!	READ(Line2, '(i5)') intElementIndex

!	WRITE(*,*) "Element Index: ", intElementIndex

	


	

! 10	CONTINUE	
	
	CLOSE(14)


	END




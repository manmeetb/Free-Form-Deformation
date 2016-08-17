	!print the initial FFD data into the output file
	
	SUBROUTINE PRINTINITIALDATAMULTIPLE()
	USE VAR


	! print the data into the files in the following format
	! First, print the number of elements, then print the element
	! number and then the FFD data for that element

	!Writing all the FFD point data and the Solid Boundary point data into a file
	!for the python to be able to plot
	OPEN(UNIT = 14, FILE = SolidBoundaryPointsOutputFileInitial)

	WRITE(14,*) "NumElements: ", NumElements
		
	DO 30 intH = 1, NumElements
	WRITE(14,*) "Element: ", intH
		
	!First write the FFD Point Data 
	!Print the title along with the number of points
	WRITE(14,*) "FFD Points ", (NXFFD(intH))*
     . 	(NYFFD(intH))*(NZFFD(intH))

	DO 10 i = 1, NXFFD(intH)
	DO 10 j = 1, NYFFD(intH)
	DO 10 k = 1, NZFFD(intH)
		WRITE(14,*) (i-1), " " ,(j-1), " " ,(k-1)
		WRITE(14,*) FFDPoints(intH,i,j,k,1), " ", 
     . 	FFDPoints(intH,i,j,k,2), " ", FFDPoints(intH,i,j,k,3)
	
  10	CONTINUE

	! Loop through and find out how many points there are 
	! for the H'th element
	intNumPoints = 0
	DO 40 intI = 1,SolidBoundaryPointsSize
	IF (SolidBoundaryPoints(intI,7) .EQ. intH) THEN
	intNumPoints = intNumPoints + 1
	ENDIF
  40	CONTINUE

	WRITE(14,*) "SolidBoundaryPointData ", 	intNumPoints
	DO 20 intI = 1,SolidBoundaryPointsSize
	
	IF (SolidBoundaryPoints(intI,7) .EQ. intH) THEN
		WRITE(14,*) SolidBoundaryPoints(intI,1), " ",
     .	SolidBoundaryPoints(intI,2), " ",
     .	SolidBoundaryPoints(intI,3), " ",
     .	SolidBoundaryPoints(intI,8), " ",
     .	SolidBoundaryPoints(intI,9), " ",
     .	SolidBoundaryPoints(intI,10), " ",
     .	SolidBoundaryPoints(intI,11), " ",
     .	SolidBoundaryPoints(intI,12), " ",
     .	SolidBoundaryPoints(intI,13), " "






		WRITE(14,*) SolidBoundaryPoints(intI,4), " ",
     .	SolidBoundaryPoints(intI,5), " ",
     . 	SolidBoundaryPoints(intI,6)		
	ENDIF

  20	CONTINUE		

  30	CONTINUE
	CLOSE(14)

	END



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
	WRITE(14,*) "FFD Points ", (NXFFD)*(NYFFD)*(NZFFD)
	DO 10 i = 1, NXFFD
	DO 10 j = 1, NYFFD
	DO 10 k = 1, NZFFD
		WRITE(14,*) (i-1), " " ,(j-1), " " ,(k-1)
		WRITE(14,*) FFDPoints(intH,i,j,k,1), " ", 
     . 	FFDPoints(intH,i,j,k,2), " ", FFDPoints(intH,i,j,k,3)
	
  10	CONTINUE
	intNumPoints = NumSolidBoundaryPoints(intH,1)
	WRITE(14,*) "SolidBoundaryPointData ", 	intNumPoints
	DO 20 intI = 1,intNumPoints
		WRITE(14,*) SolidBoundaryPoints(intH,intI,1), " ",
     .	SolidBoundaryPoints(intH,intI,2), " ",
     .	SolidBoundaryPoints(intH,intI,3), " ",
     .	SolidBoundaryPoints(intH,intI,7)
		WRITE(14,*) SolidBoundaryPoints(intH,intI,4), " ",
     .	SolidBoundaryPoints(intH,intI,5), " ",
     . 	SolidBoundaryPoints(intH,intI,6)
			
  20	CONTINUE		

  30	CONTINUE
	CLOSE(14)

	END



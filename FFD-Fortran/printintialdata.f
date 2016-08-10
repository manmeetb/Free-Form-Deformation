	SUBROUTINE PRINTINTITIALDATA()

	!Writing all the FFD point data and the Solid Boundary point data into a file
	!for the python to be able to plot
	OPEN(UNIT = 14, FILE = SolidBoundaryPointsOutputFile)
	!First write the FFD Point Values
	WRITE(14,*) "FFD Points: X, Y, Z"
	DO 10 i = 1, NXFFD
	DO 10 j = 1, NYFFD
	DO 10 k = 1, NZFFD
		WRITE(14,*) (i-1), " , " ,(j-1), " , " ,(k-1)
		WRITE(14,*) FFDPoints(1,i,j,k,1), ", ", FFDPoints(1,i,j,k,2), 
     . 	", ", FFDPoints(1,i,j,k,3)
	
  10	CONTINUE
	
	WRITE(14,*) "Solid Boundary Point Data"	
	DO 20 intI = 1,SolidBoundaryPointsSize
		WRITE(14,*) SolidBoundaryPoints(1,intI,1), ", ",
     .	SolidBoundaryPoints(1,intI,2), ", ",
     .	SolidBoundaryPoints(1,intI,3)
		WRITE(14,*) SolidBoundaryPoints(1,intI,4), ", ",
     .	SolidBoundaryPoints(1,intI,5), ", ",
     . 	SolidBoundaryPoints(1,intI,6)
			
  20	CONTINUE		
	CLOSE(14)
	



	END






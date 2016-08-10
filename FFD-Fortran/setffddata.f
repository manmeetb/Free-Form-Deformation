	SUBROUTINE SETFFDDATA()
	!The subroutine for set the FFD points	
	USE VAR

	rFFDdx = (CONST_FFDXMax-CONST_FFDXMin)/(NXFFD-1)
    	rFFDdy = (CONST_FFDYMax - CONST_FFDYMin) / (NYFFD - 1)
    	rFFDdz = (CONST_FFDZMax - CONST_FFDZMin) / (NZFFD - 1)
	
	!WRITE(*,*) "before loop"
	!WRITE(*,*) "NXFFD: ", NXFFD
	!WRITE(*,*) "NYFFD: ", NYFFD
	!WRITE(*,*) "NZFFD: ", NZFFD
	
	!Create the multidimensional array
	ALLOCATE(FFDPoints(1,NXFFD,NYFFD,NZFFD,3))
		
  	DO 10 i=1,NXFFD
	DO 10 j=1,NYFFD
	DO 10 k=1,NZFFD
			
		FFDPoints(1,i,j,k,1) = (CONST_FFDXMin + rFFDdx*(i-1))
		FFDPoints(1,i,j,k,2) = (CONST_FFDYMin + rFFDdy*(j-1))
		FFDPoints(1,i,j,k,3) = (CONST_FFDZMin + rFFDdz*(k-1))	
	
   10	CONTINUE	 	

	
	END

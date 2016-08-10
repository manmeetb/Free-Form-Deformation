	PROGRAM MAINLOADDATA
	USE VAR
	
	! the main program for the fortran routine. This routine
	! will load previously processed fortran files and then 
	! make changes 

	!Set the number of FFD points in each direction
	NXFFD = 10		
	NYFFD = 3
	NZFFD = 8
	
	!Set the limits for the FFD Box
	CONST_FFDXMin = 0.00001
	CONST_FFDXMax = 1.000001
	CONST_FFDYMin = -0.060001
	CONST_FFDYMax = 0.060001
	CONST_FFDZMin = -0.0001
	CONST_FFDZMax = 3.06001
	
	InputFile = 
     . 	'DataFiles/FortranPointDataCRMFinal.txt'
	OutputFile = 
     . 	'FortranPointDataCRMFinal.txt'	
	
	CALL READDATA()

!	CALL DEFORMLATTICE()
	
!	CALL MODSHAPEFFD()

!	CALL PRINTFINALDATA()
	

	
	!deallocating the array's space
!	DEALLOCATE(SolidBoundaryPoints)
!	DEALLOCATE(FFDPoints)
	STOP
	END

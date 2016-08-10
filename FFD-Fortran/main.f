	PROGRAM MAIN
	USE VAR
	
! (done) - implement way to search through points and label them as leading, 
!	trailing, wing tip, etc. Do this in python
!- Store all the different constraint points in array data structures
!	when reading the solid bnd point data. For now, keep this on
!	hold. We want to keep the code as general as possible and
! 	adding unneeded arrays for this is not required at this time.
! - After capturing the entire shape of the wing, refine the point locations 
!	to take the wing tip, root, leading edge and trailing edge out of 
!	the box. 
! 	- Find a way for the algorithm to know which direction to refine the 
!	to remove constrained points from the box. For now, code the algorithm
! 	so that for example to remove the wing tip, the box should be refined
!	in the right direction	
! - Create a text file that will hold properties about the problem
!	such as the names of the files, what to constrain, num of pts, ...	

	! the main program for the fortran routine. 

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
	
	SolidBoundaryPointsOutputFileInitial = 
     . 	'FPD_CRMDesignInitialWinglet.txt'	
	SolidBoundaryPointsOutputFileFinal = 
     . 	'FPD_CRMDesignFinalWinglet.txt'	
	SolidBoundaryPointsFile = 
     . 	'NewWingDeformations/IF_crmdes.dat'

	CALL READSOLIDDATAMULTIPLE()
	
	CALL SETFFDDATAAUTOMULTIPLE()	

	CALL PRINTINITIALDATAMULTIPLE()

!	CALL LOADFFDINITIALDATA()
	
!	CALL SETFFDDATAAUTO()

!	CALL SETFFDDATA()
	
!	intStatus = -1		
!	CALL ATTACHFFDNEWTON(intStatus)

!	CALL PRINTINITIALDATA()
	
	CALL DEFORMLATTICE()
	
	CALL MODSHAPEFFD()

	CALL PRINTFINALDATA()
	

	
	!deallocating the array's space
	DEALLOCATE(SolidBoundaryPoints)
	DEALLOCATE(NumSolidBoundaryPoints)
	DEALLOCATE(ZCrossSectionsSize)	
	DEALLOCATE(FFDPoints)
	DEALLOCATE(ZCrossSectionsData)
	DEALLOCATE(FFDBoxProperties)
	DEALLOCATE(MaxZ)
	DEALLOCATE(MinZ)		
	STOP
	END

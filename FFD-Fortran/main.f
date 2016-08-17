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
	
	SolidBoundaryPointsOutputFileInitial = 
     . 	'FPD_CRMWingBodyInitialAug16.txt'	
	SolidBoundaryPointsOutputFileFinal = 
     . 	'FPD_CRMWingBodyFinalAug16.txt'	
	SolidBoundaryPointsFile = 
     . 	'INPUT_SepMeshFile.txt'


	CALL READSOLIDDATAMULTIPLE()
!	WRITE(*,*) "completed read"
	CALL SETFFDDATAAXIS()	
!	WRITE(*,*) "completed set ffd"
	
!	CALL SETFFDDATAAUTOMULTIPLE()	

	CALL PRINTINITIALDATAMULTIPLE()


!	CALL LOADFFDINITIALDATA()
	
!	CALL SETFFDDATAAUTO()

!	CALL SETFFDDATA()
	
!	intStatus = -1		
!	CALL ATTACHFFDNEWTON(intStatus)

!	CALL PRINTINITIALDATA()

!	CALL DEFORMLATTICE()
	
!	CALL MODSHAPEFFD()

	CALL PRINTFINALDATA()
	

	
	!deallocating the array's space
	DEALLOCATE(SolidBoundaryPoints)
	DEALLOCATE(NXFFD)
	DEALLOCATE(NYFFD)
	DEALLOCATE(NZFFD)
	DEALLOCATE(AxisDirection)
	DEALLOCATE(NumSlices)
	DEALLOCATE(Rank)
	DEALLOCATE(ConnectivityInfo)	
	
	DEALLOCATE(CrossSectionsSize)	
	DEALLOCATE(FFDPoints)
	DEALLOCATE(CrossSectionsData)
	DEALLOCATE(FFDVolProperties)
	DEALLOCATE(MaxValueElem)
	DEALLOCATE(MinValueElem)		
	STOP
	END

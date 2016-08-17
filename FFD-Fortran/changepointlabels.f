
	! The subroutine that is used for changing the labels of
	! points that are now in the FFD volume
	SUBROUTINE CHANGEPOINTLABELS(intH)
	USE VAR

	! The index of the element
	INTEGER :: intH
	
	! Get the limits of the FFD box. This is because
	! we want to only test points that are sufficiently
	! close to our element

		! the dimensions of the FFD box
	realMaxX = 0.0
	realMinX = 0.0
	realMaxY = 0.0
	realMinY = 0.0
	realMaxZ = 0.0
	realMinZ = 0.0		
	intFirstValueLabel = 0

	DO 10 intI = 1,NXFFD(intH)
	DO 10 intJ = 1,NYFFD(intH)
	DO 10 intK = 1,NZFFD(intH)
	
	realX = FFDPoints(intH,intI,intJ,intK,1)
	realY = FFDPoints(intH,intI,intJ,intK,2)
	realZ = FFDPoints(intH,intI,intJ,intK,3)

	! If this is the first FFD points
	IF (intFirstValueLabel .EQ. 0) THEN
	realMaxX = realX
	realMinX = realX

	realMaxY = realY
	realMinY = realY

	realMaxZ = realZ
	realMinZ = realZ
	intFirstValueLabel = 1
	ENDIF

	realMaxX = MAX(realMaxX, realX)
	realMinX = MIN(realMinX, realX)

	realMaxY = MAX(realMaxY, realY)
	realMinY = MIN(realMinY, realY)

	realMaxZ = MAX(realMaxZ, realZ)
	realMinZ = MIN(realMinZ, realZ)
  10	CONTINUE	

	WRITE(*,*) "FFD MaxX: ", realMaxX
	WRITE(*,*) "FFD MinX: ", realMinX
	WRITE(*,*) "FFD MaxY: ", realMaxY
	WRITE(*,*) "FFD MinY: ", realMinY
	WRITE(*,*) "FFD MaxZ: ", realMaxZ
	WRITE(*,*) "FFD MinZ: ", realMinZ
	

	realX = 1480.30420
	realY = 158.571213
	realZ = 250.766724

	realT = 0.0
	realU = 0.0
	realV = 0.0

	intStatus = -1
		
	CALL NEWTONSOLVE(0.5,0.5,0.5,realX,realY,realZ,
     . 	realT,realU,realV, intStatus, intH)

	IF (intStatus .NE. 1) THEN
	! we are going to range our guesses from 0.05
	! to 0.95 with increments of 0.05
	DO 225 intk=0,18
	realGuess = 0.05 + REAL(intk)*0.05
	CALL NEWTONSOLVE(realGuess,realGuess,realGuess,realX,
     . 	realY,realZ, realT,realU,realV, intStatus, 
     . 	intH)
	
	IF (intStatus .EQ. 1) THEN
	EXIT
	ENDIF
  225	CONTINUE
	
	! Tried varying initial conditions to no success
	IF (intStatus .NE. 1) THEN
		WRITE(*,*) "Failed Newton Raphson Change Pts"		
		STOP
	ENDIF
	ENDIF	


	WRITE(*,*) "Test T: ", realT
	WRITE(*,*) "Test U: ", realU
	WRITE(*,*) "Test V: ", realV

!	STOP

	! Now, loop through all points that are not part of the element 
	! and compute their T,U,V values. 

	DO 20 intI = 1,SolidBoundaryPointsSize
	IF (SolidBoundaryPoints(intI,7) .NE. intH) THEN
	realX = SolidBoundaryPoints(intI,1)
	realY = SolidBoundaryPoints(intI,2)
	realZ = SolidBoundaryPoints(intI,3)

	

	IF ((realX .LT. realMaxX) .AND. 
     . 	(realX .GT. realMinX) .AND. 
     . 	(realY .LT. realMaxY) .AND. 
     .	(realY .GT. realMinY) .AND. 
     .	(realZ .LT. realMaxZ) .AND. 
     .	(realZ .GT. realMinZ)) THEN

	realT = 0.0
	realU = 0.0
	realV = 0.0

	WRITE(*,*) "Change_Index: ", intI
	
	CALL NEWTONSOLVE(0.5,0.5,0.5,realX,realY,realZ,
     .	realT,realU,realV, intStatus, intH)

	! Newton Raphson Failure using 0.5 so try numbers
	! starting from 0.05 and increment by 0.05 each time
	! for the initial guess
	IF (intStatus .NE. 1) THEN
	! we are going to range our guesses from 0.05
	! to 0.95 with increments of 0.05
	DO 25 intk=0,18
	realGuess = 0.05 + REAL(intk)*0.05
	CALL NEWTONSOLVE(realGuess,realGuess,realGuess,realX,
     . 	realY,realZ, realT,realU,realV, intStatus, 
     . 	intH)
	
	IF (intStatus .EQ. 1) THEN
	EXIT
	ENDIF
  25	CONTINUE
	
	! Tried varying initial conditions to no success
	IF (intStatus .NE. 1) THEN
		WRITE(*,*) "Failed Newton Raphson Change Pts"		
		STOP
	ENDIF
	ENDIF	

	WRITE(*,*) "realT: ", realT
	WRITE(*,*) "realU: ", realU
	WRITE(*,*) "realV: ", realV
	! Check if the T,U and V values are between 0 and 1
	
	IF ((realT .GE. 0.0) .AND. (realT .LE. 1.0) 
     . 	.AND. (realU .GE. 0.0) .AND. (realU .LE. 1.0) 
     .  .AND. (realV .GE. 0.0) .AND. (realV .LE. 1.0)) THEN
	!point is inside the box. Change the point's element
	! label
	WRITE(*,*) "oldH: ", SolidBoundaryPoints(intI,7)
	SolidBoundaryPoints(intI,7) = intH	
	WRITE(*,*) "newH: ", SolidBoundaryPoints(intI,7)
		
	ENDIF
 




	ENDIF 
	ENDIF

  20	CONTINUE

	END


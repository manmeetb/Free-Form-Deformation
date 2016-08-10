! The subroutine that will be used for finding the initial T,U,V
! values of the the solid boundary points
	
	SUBROUTINE ATTACHFFDNEWTONCONSTRAINT(intStatusIndex,intConst)
	USE VAR

	INTEGER :: intStatusIndex, intConst

	DO 100 iIndex = 1,SolidBoundaryPointsSize
	intLabel = SolidBoundaryPoints(1,iIndex,7)
	! If the intConst label is -1 then all the points have to 
	! have a t,u,v value computed for them
	IF ((intLabel .EQ. intConst) .OR. (intConst .EQ. -1)) THEN
	
	WRITE(*,*) "Constraint Index: ", iIndex
	!Get the x,y,z values of the solid boundary point in question
	realX11 = SolidBoundaryPoints(1,iIndex,1)
	realY11 = SolidBoundaryPoints(1,iIndex,2)
	realZ11 = SolidBoundaryPoints(1,iIndex,3)	

	realAns1 = 0.0
	realAns2 = 0.0
	realAns3 = 0.0
	
	WRITE(*,*) "	label: " , intLabel	
		
	CALL NEWTONSOLVE(0.5,0.5,0.5,realX11,realY11,realZ11,
     .	realAns1,realAns2,realAns3)
	
	!Fill the T,U,V information into the solid boundary point array
	SolidBoundaryPoints(1,iIndex,4) = realAns1
	SolidBoundaryPoints(1,iIndex,5) = realAns2
	SolidBoundaryPoints(1,iIndex,6) = realAns3

	WRITE(*,*) "	t: ", realAns1
	WRITE(*,*) "	u: ", realAns2
	WRITE(*,*) "	v: ", realAns3

	IF ((realAns1 .LT. 1.0) .AND. (realAns1 .GT. 0.0) 
     . 	.AND. (realAns2 .LT. 1.0) .AND. (realAns2 .GT. 0.0) 
     .  .AND. (realAns3 .LT. 1.0) .AND. (realAns3 .GT. 0.0)) THEN
		!The constraint point is still in the box
		intStatusIndex = 1
		IF (intConst .NE. -1) THEN
			! Only exit if all the points dont need their 
			! t,u,v values calculated
			GOTO 200
		ENDIF
	ENDIF
	ENDIF
  100	CONTINUE


  200 	realw = 0.0
	
	END


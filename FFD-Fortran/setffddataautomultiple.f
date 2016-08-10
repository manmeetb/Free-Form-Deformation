
	
	! The subroutine that is used for setting the ffd data for the multiple
	! elements. This is the subroutine used for attaching the FFD 
	! points automatically onto the wing
	
	SUBROUTINE SETFFDDATAAUTOMULTIPLE()
	
	USE VAR



	CALL ATTACHINITIALMESH()	


	! Allocate the space needed for the FFD box. For now,
	! assume each body has the same amount of FFD points 
	! in each coordinate directions
	ALLOCATE(FFDPoints(NumElements,NXFFD,NYFFD,NZFFD,3))

	
	! Make a subroutine for placing an original mesh. This mesh will have 
	! No tolerance values for the epsilon x and y. These tolerance values
	! will then be adjusted when computng the local coordinates to 
	! ensure that all points on the wing are contained in the box. Thus, in 
	! this subroutine we will call attachffdnewton at each ffd box placement
	! iteration.
	
	CALL CREATEFFDMESH()



	!The magnitude of the jump when adjusting the box. Set this value to 
	! 
	

	realEpsilon = 0.025

	! Now, the algorithm must run the attachffdnewton subroutine
	! This subroutine should be changed so that it now
	! stops running if a point is found whose t,u,v values are
	! not in the range [0,1]. If this is the case, the box must be 
	! adjusted accordingly to enclose the point


	OPEN(UNIT = 32, FILE = "OptimizationHistory.txt")	
	

	DO 30 intH = 1,NumElements

	WRITE(32,*) " "
	WRITE(32,*) "element:", intH
	intStepSize = 5

	!Update rest of the box now that the cross sections are placed
	! properly	
	DO WHILE(.TRUE.)	

	!REMOVE. USED ONLY FOR PLOTTING INITIAL BOX
	!EXIT
	
	intStatusIndex = -1

	realTmin = 0.0
	realTmax = 0.0
	realUmin = 0.0
	realUmax = 0.0
	realVmin = 0.0
	realVmax = 0.0



	CALL ATTACHFFDNEWTON(intStatusIndex,realTmin,realTmax, 
     . 	realUmin, realUmax, realVmin, realVmax, intStepSize,
     . 	intH)
	
!	STOP	
		
	If((intStatusIndex .EQ. -1) .AND. 
     . 	(intStepSize .EQ. 1)) THEN
			
		WRITE(32,*) "Final Results:"
	
		WRITE(32,*) "	realTmin: ", realTmin
		WRITE(32,*) "	realTmax: ", realTmax
		WRITE(32,*) "	realUmin: ", realUmin
		WRITE(32,*) "	realUmax: ", realUmax
		WRITE(32,*) "	realVmin: ", realVmin
		WRITE(32,*) "	realVmax: ", realVmax
		
		
		EXIT
	ENDIF

	If((intStatusIndex .EQ. -1) .AND. 
     . 	(intStepSize .NE. 1)) THEN
			
		WRITE(32,*) "Final Results:"
	
		WRITE(32,*) "	realTmin: ", realTmin
		WRITE(32,*) "	realTmax: ", realTmax
		WRITE(32,*) "	realUmin: ", realUmin
		WRITE(32,*) "	realUmax: ", realUmax
		WRITE(32,*) "	realVmin: ", realVmin
		WRITE(32,*) "	realVmax: ", realVmax
		
		
		! Now refine the process
		intStepSize = 1
	ENDIF
	

	! There are points that are not enclosed
	realXMaxE = 0.0
	realXMinE = 0.0
	realYMaxE = 0.0
	realYMinE = 0.0
	realZMaxE = 0.0
	realZMinE = 0.0

	! verify that these are correct for any configuration
	! of the object. I believe that it is the case because 
	! we create the FFD rect from the minimum z to maximum z
	! value, and then all cross sections are created from the 
	! min to max x and y values
	
	IF (realTmin .LT. 0) THEN
		realXMinE = (-1.0)*realEpsilon
	ENDIF

	IF (realTmax .GT. 1) THEN
		realXMaxE = realEpsilon
	ENDIF
	
	IF (realUmin .LT. 0) THEN
		realYMinE = -1.0*realEpsilon
	ENDIF

	IF (realUmax .GT. 1) THEN
		realYMaxE = realEpsilon
	ENDIF

	IF (realVmin .LT. 0) THEN
		realZMinE = -1.0 * realEpsilon
	ENDIF

	IF (realVmax .GT. 1) THEN
		realZMaxE = realEpsilon
	ENDIF


	
	DO 920 intl = 1, NZFFD
	WRITE(32,*) "z: ", FFDBoxProperties(intH,intl,1)
	WRITE(32,*) "xMax: ", FFDBoxProperties(intH,intl,2)
	WRITE(32,*) "xMin: ", FFDBoxProperties(intH,intl,3)
	WRITE(32,*) "yMax: ", FFDBoxProperties(intH,intl,4)
	WRITE(32,*) "yMin: ", FFDBoxProperties(intH,intl,5)
  920	CONTINUE	

	
	WRITE(*,*) "	realTmin: ", realTmin
	WRITE(*,*) "	realTmax: ", realTmax
	WRITE(*,*) "	realUmin: ", realUmin
	WRITE(*,*) "	realUmax: ", realUmax
	WRITE(*,*) "	realVmin: ", realVmin
	WRITE(*,*) "	realVmax: ", realVmax


	WRITE(32,*) "	realTmin: ", realTmin
	WRITE(32,*) "	realTmax: ", realTmax
	WRITE(32,*) "	realUmin: ", realUmin
	WRITE(32,*) "	realUmax: ", realUmax
	WRITE(32,*) "	realVmin: ", realVmin
	WRITE(32,*) "	realVmax: ", realVmax	
	CALL UPDATEFFDBOX(realXMaxE, realXMinE, realYMaxE, 
     . 	realYMinE, realZMaxE, realZMinE, intH)


	!Update the FFD rects now for the exact z cross sections
	DO 280 intm = 1, NZFFD
		CALL CREATEFFDRECTEXACT(intH,intm)			
  280	CONTINUE
	
	DO 720 intl = 1, NZFFD
	WRITE(32,*) "z: ", FFDBoxProperties(intH,intl,1)
	WRITE(32,*) "xMax: ", FFDBoxProperties(intH,intl,2)
	WRITE(32,*) "xMin: ", FFDBoxProperties(intH,intl,3)
	WRITE(32,*) "yMax: ", FFDBoxProperties(intH,intl,4)
	WRITE(32,*) "yMin: ", FFDBoxProperties(intH,intl,5)
  720	CONTINUE	
	WRITE(32,*) " "

	
	ENDDO


  30	CONTINUE
	
	CLOSE(32)	


	
	END


















! The function that is used for comapring two z points. It will check to see
! whether the points, to their second decimal place, are identical.
!Have the function return 1 if the numbers are identical to the second
!decimal place and 0 otherwise
	INTEGER FUNCTION CompareZPoints(realNumDecimal, realZ1, realZ2)
	
	realMultiplier = 10**realNumDecimal

	intZ1 = int(realZ1*realMultiplier)
	intZ2 = int(realZ2*realMultiplier)

!	WRITE(*,*) "intZ1_3digit: ", intZ1_3decimal
!	WRITE(*,*) "intZ2_3digit: ", intZ2_3decimal
	intStatus = 0
	IF (intZ1 .EQ. intZ2) THEN
		intStatus = 1
	ENDIF
	CompareZPoints = intStatus
	END











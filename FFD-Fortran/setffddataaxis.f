	
	! The subroutine that will be used for placing the data for the 
	! element in question along a specified axis. The X axis will be 1,
	! and so on. 
	SUBROUTINE SETFFDDATAAXIS()
	
	USE VAR

	ALLOCATE(FFDPoints(NumElements,MaxNXFFD,MaxNYFFD,
     . 	MaxNZFFD,3))
	
	
	
	CALL ATTACHINITIALMESHAXIS()

	CALL CREATEFFDMESHAXIS()

		

	

	!Now, adjust the shape so that it is completely contained in the FFD
	! volume
	
	!realEpsilon = 0.025
        realEpsilon = 5
	DO 30 intH = 1,NumElements

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
     .  realUmin, realUmax, realVmin, realVmax, intStepSize,
     .  intH)

!       STOP    

	If((intStatusIndex .EQ. -1) .AND.
     .  (intStepSize .EQ. 1)) THEN

                WRITE(*,*) "Final Results:"

                WRITE(*,*) "   realTmin: ", realTmin
                WRITE(*,*) "   realTmax: ", realTmax
                WRITE(*,*) "   realUmin: ", realUmin
                WRITE(*,*) "   realUmax: ", realUmax
                WRITE(*,*) "   realVmin: ", realVmin
                WRITE(*,*) "   realVmax: ", realVmax


                EXIT
        ENDIF

        If((intStatusIndex .EQ. -1) .AND.
     .  (intStepSize .NE. 1)) THEN

                WRITE(*,*) "Intermediary Results:"

                WRITE(*,*) "   realTmin: ", realTmin
                WRITE(*,*) "   realTmax: ", realTmax
                WRITE(*,*) "   realUmin: ", realUmin
                WRITE(*,*) "   realUmax: ", realUmax
                WRITE(*,*) "   realVmin: ", realVmin
                WRITE(*,*) "   realVmax: ", realVmax


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

	

	! Because the i,j and k points move the increasing x,y and 
	! z axis respectively, the following will work for any axis
	! direction chosen to make the box.
	
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



   	WRITE(*,*) "    realTmin: ", realTmin
        WRITE(*,*) "    realTmax: ", realTmax
        WRITE(*,*) "    realUmin: ", realUmin
        WRITE(*,*) "    realUmax: ", realUmax
        WRITE(*,*) "    realVmin: ", realVmin
        WRITE(*,*) "    realVmax: ", realVmax


        CALL UPDATEFFDBOX(realXMaxE, realXMinE, realYMaxE,
     .  realYMinE, realZMaxE, realZMinE, intH)

	! Change the FFD points locations in the data structures
	CALL FILLFFDDATAAXIS(intH)
	!EXIT

        ENDDO


  30    CONTINUE

	END



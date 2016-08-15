

! The subroutine for updating the FFDBoxProperties. It takes the epsilon
! values for the x,y and z limits. The epsilon values will be negative
! themselves so just add them to update the z values
	SUBROUTINE UPDATEFFDBOX(realXMaxE, realXMinE, 
     . 	realYMaxE, realYMinE, realZMaxE, realZMinE,intH)
	
	USE VAR
	
	INTEGER :: intH
	REAL :: realZMaxE, realZMinE, realXMaxE, realXMinE,
     . 	realYMaxE, realYMinE


	! If the x axis has been chosen
	IF (AxisDirection(intH) .EQ. 1) THEN
	CALL UPDATEBOXX2(realXMaxE, realXMinE, 
     . 	realYMaxE, realYMinE, realZMaxE, realZMinE, intH)
	ENDIF 

	! If the y axis has been chosen
	IF (AxisDirection(intH) .EQ. 2) THEN
	CALL UPDATEBOXY2(realXMaxE, realXMinE, 
     . 	realYMaxE, realYMinE, realZMaxE, realZMinE, intH)
	ENDIF 


	! If the z axis has been chosen
	IF (AxisDirection(intH) .EQ. 3) THEN
	
	CALL UPDATEBOXZ2(realXMaxE, realXMinE,
     .  realYMaxE, realYMinE, realZMaxE, realZMinE, intH)

	ENDIF


	END


	! Updating the box if the x axis is chosen
	SUBROUTINE UPDATEBOXX2(realXMaxE, realXMinE,
     .  realYMaxE, realYMinE, realZMaxE, realZMinE, intH)

	USE VAR
		
	INTEGER :: intH
	REAL :: realZMaxE, realZMinE, realXMaxE, realXMinE,
     . 	realYMaxE, realYMinE
	

	! Now first update the X limits if the epislon values
	! are non zero
	IF ((realXMaxE .NE. 0.0) .OR. (realXMinE .NE. 0.0)) THEN
		! Compute the new z max and min values.
		! Recall that the FFDBox array holds data for 
		! the cross section with the smallest z value to the 
		! cross section with the largest z value

	realNewXMin = FFDVolProperties(intH,1, 1)
	realNewXMax = FFDVolProperties(intH,NXFFD(intH), 1)
	
	realNewXMin = realNewXMin + realXMinE
	realNewXMax = realNewXMax + realXMaxE

	FFDVolProperties(intH,1,1) = realNewXMin
	FFDVolProperties(intH,NXFFD(intH),1) = realNewXMax
	
	ENDIF	

	!Update all the limits for the FFD box using the given epsilon
	! terms
	DO 320 intl = 1, NXFFD(intH)
	FFDVolProperties(intH,intl,6) = FFDVolProperties(intH,intl,6)
     . 	 + realZMaxE
	FFDVolProperties(intH,intl,7) = FFDVolProperties(intH,intl,7)
     . 	 + realZMinE
	FFDVolProperties(intH,intl,4) = FFDVolProperties(intH,intl,4)
     . 	 + realYMaxE
	FFDVolProperties(intH,intl,5) = FFDVolProperties(intH,intl,5)
     . 	 + realYMinE
  320	CONTINUE	
	END



	! Updating the box if the x axis is chosen
	SUBROUTINE UPDATEBOXY2(realXMaxE, realXMinE,
     .  realYMaxE, realYMinE, realZMaxE, realZMinE, intH)

	USE VAR
		
	INTEGER :: intH
	REAL :: realZMaxE, realZMinE, realXMaxE, realXMinE,
     . 	realYMaxE, realYMinE
	

	! Now first update the X limits if the epislon values
	! are non zero
	IF ((realYMaxE .NE. 0.0) .OR. (realYMinE .NE. 0.0)) THEN
		! Compute the new z max and min values.
		! Recall that the FFDBox array holds data for 
		! the cross section with the smallest z value to the 
		! cross section with the largest z value

	realNewYMin = FFDVolProperties(intH,1, 1)
	realNewYMax = FFDVolProperties(intH,NYFFD(intH), 1)
	
	realNewYMin = realNewYMin + realYMinE
	realNewYMax = realNewYMax + realYMaxE

	FFDVolProperties(intH,1,1) = realNewYMin
	FFDVolProperties(intH,NYFFD(intH),1) = realNewYMax
	
	ENDIF	

	!Update all the limits for the FFD box using the given epsilon
	! terms
	DO 320 intl = 1, NXFFD(intH)
	FFDVolProperties(intH,intl,2) = FFDVolProperties(intH,intl,6)
     . 	 + realXMaxE
	FFDVolProperties(intH,intl,3) = FFDVolProperties(intH,intl,7)
     . 	 + realXMinE
	FFDVolProperties(intH,intl,4) = FFDVolProperties(intH,intl,4)
     . 	 + realYMaxE
	FFDVolProperties(intH,intl,5) = FFDVolProperties(intH,intl,5)
     . 	 + realYMinE
  320	CONTINUE	
	END









	! Updating the box if the z axis is chosen
	SUBROUTINE UPDATEBOXZ2(realXMaxE, realXMinE,
     .  realYMaxE, realYMinE, realZMaxE, realZMinE, intH)

	USE VAR
		
	INTEGER :: intH
	REAL :: realZMaxE, realZMinE, realXMaxE, realXMinE,
     . 	realYMaxE, realYMinE
	

	! Now first update the Z limits if the epislon values
	! are non zero
	IF ((realZMaxE .NE. 0.0) .OR. (realZMinE .NE. 0.0)) THEN
		! Compute the new z max and min values.
		! Recall that the FFDBox array holds data for 
		! the cross section with the smallest z value to the 
		! cross section with the largest z value

	realNewZMin = FFDVolProperties(intH,1, 1)
	realNewZMax = FFDVolProperties(intH,NZFFD(intH), 1)
	
	realNewZMin = realNewZMin + realZMinE
	realNewZMax = realNewZMax + realZMaxE
		
	FFDVolProperties(intH,1,1) = realNewZMin
	FFDVolProperties(intH,NZFFD(intH),1) = realNewZMax
	ENDIF	

	!Update all the limits for the FFD box using the given epsilon
	! terms
	DO 320 intl = 1, NZFFD(intH)
	FFDVolProperties(intH,intl,2) = FFDVolProperties(intH,intl,2)
     . 	 + realXMaxE
	FFDVolProperties(intH,intl,3) = FFDVolProperties(intH,intl,3)
     . 	 + realXMinE
	FFDVolProperties(intH,intl,4) = FFDVolProperties(intH,intl,4)
     . 	 + realYMaxE
	FFDVolProperties(intH,intl,5) = FFDVolProperties(intH,intl,5)
     . 	 + realYMinE
  320	CONTINUE	

	END


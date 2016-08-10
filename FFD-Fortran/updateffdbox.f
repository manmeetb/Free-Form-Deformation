

! The subroutine for updating the FFDBoxProperties. It takes the epsilon
! values for the x,y and z limits. The epsilon values will be negative
! themselves so just add them to update the z values
	SUBROUTINE UPDATEFFDBOX(realXMaxE, realXMinE, 
     . 	realYMaxE, realYMinE, realZMaxE, realZMinE,intH)
	
	USE VAR
	
	INTEGER :: intH
	REAL :: realZMaxE, realZMinE, realXMaxE, realXMinE,
     . 	realYMaxE, realYMinE

!	WRITE(*,*) "RealZMaxE: ", realZMaxE
!	WRITE(*,*) "RealZMinE: ", realZMinE


	! Now first update the Z limits if the epislon values
	! are non zero
	IF ((realZMaxE .NE. 0.0) .OR. (realZMinE .NE. 0.0)) THEN
		! Compute the new z max and min values.
		! Recall that the FFDBox array holds data for 
		! the cross section with the smallest z value to the 
		! cross section with the largest z value

	realNewZMin = FFDBoxProperties(intH,1, 1)
	realNewZMax = FFDBoxProperties(intH,NZFFD, 1)
	
	realNewZMin = realNewZMin + realZMinE
	realNewZMax = realNewZMax + realZMaxE
		
	
	realExactdz = (realNewZMax - realNewZMin)/(Real(NZFFD)-1.0)
	
		! Fill first the z values of each cross section
	DO 310 intl = 0, (NZFFD-1)
	realZvalue = realNewZMin + Real(intl)*realExactdz
	FFDBoxProperties(intH,intl+1, 1) = realZValue
  310	CONTINUE

	DO 340 intl = 1, NZFFD
		!Find the z value of the exact FFD cross section of interest
	realFFDCrossSectZ = FFDBoxProperties(intH,intl,1)

	!Use the information from slices near the ffd plane to find out the 
	! dimensions
	realZCrossMin = realFFDCrossSectZ - 0.125*(realExactdz)
    	realZCrossMax = realFFDCrossSectZ + 0.125*(realExactdz)

		!At one end of the object (minimum z value)
	IF (intl .EQ. 1) THEN
		realZCrossMin = realFFDCrossSectZ
		realZCrossMax = realFFDCrossSectZ + 0.125*realExactdz
	ENDIF
		!At the other end of the object (max z value)
	IF (intl .EQ. NZFFD) THEN
		realZCrossMax = realFFDCrossSectZ
		realZCrossMin = realFFDCrossSectZ - 0.125*realExactdz
	ENDIF
	
	realAverageXMax = 0.0
	realAverageXMin = 0.0
	realAverageYMax = 0.0
	realAverageYMin = 0.0
	intNumValues = 0


 	!loop through all the cross sections to get the average data
        DO 40 intm = 1, ZCrossSectionsSize(intH,1)
        realZMinSlice = ZCrossSectionsData(intH,intm,5)
        realZMaxSlice = ZCrossSectionsData(intH,intm,6)


        ! A slice is considered to be in the vicinity if any
        ! part of the slice falls in the range. Check if:
        !       1. both z max and zmin are in the range
        !       2. If ZCrossMin is between the max and min slice
        !       3. If ZCrossMax is between the max and min
        IF(((realZMaxSlice .LE. realZCrossMax) .AND.
     .   (realZMinSlice .GE. realZCrossMin)) .OR.
     .  ((realZCrossMin .LE. realZMaxSlice) .AND.
     .  (realZCrossMin .GE. realZMinSlice)) .OR.
     .  ((realZCrossMax .LE. realZMaxSlice) .AND.
     .  (realZCrossMax .GE. realZMinSlice))) THEN

        realAverageXMax = realAverageXMax +
     .  ZCrossSectionsData(intH,intm,2)
        realAverageXMin = realAverageXMin +
     .  ZCrossSectionsData(intH,intm,1)
        realAverageYMax = realAverageYMax +
     .  ZCrossSectionsData(intH,intm,4)
        realAverageYMin = realAverageYMin +
     .  ZCrossSectionsData(intH,intm,3)
        intNumValues = intNumValues + 1
        ENDIF

  40    CONTINUE



	realAverageXMax = realAverageXMax/REAL(intNumValues) 
	realAverageXMin = realAverageXMin/REAL(intNumValues)
	realAverageYMax = realAverageYMax/REAL(intNumValues)
	realAverageYMin = realAverageYMin/REAL(intNumValues)
	
	!Now, with the data from the closest physical cross section
	! fill the data for the exact cross section
	
	FFDBoxProperties(intH,intl,2) = realAverageXMax
	FFDBoxProperties(intH,intl,3) = realAverageXMin
 	FFDBoxProperties(intH,intl,4) = realAverageYMax
 	FFDBoxProperties(intH,intl,5) = realAverageYMin

  340	CONTINUE	
	
	! Now that all the cross sections are rearranged, the 
	! old epsilon x max and x min values are obsolete so
	! set them to 0.0

	realXMaxE = 0.0
	realXMinE = 0.0
	realYMaxE = 0.0
	realYMinE = 0.0

	ENDIF	

	!Update all the limits for the FFD box using the given epsilon
	! terms
	DO 320 intl = 1, NZFFD
	FFDBoxProperties(intH,intl,2) = FFDBoxProperties(intH,intl,2)
     . 	 + realXMaxE
	FFDBoxProperties(intH,intl,3) = FFDBoxProperties(intH,intl,3)
     . 	 + realXMinE
	FFDBoxProperties(intH,intl,4) = FFDBoxProperties(intH,intl,4)
     . 	 + realYMaxE
	FFDBoxProperties(intH,intl,5) = FFDBoxProperties(intH,intl,5)
     . 	 + realYMinE
  320	CONTINUE	


	END




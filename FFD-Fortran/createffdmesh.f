!Floating number error is triggering here

! The subroutine used for creating the initial FFD mesh around the object

	SUBROUTINE CREATEFFDMESH()
	USE VAR

	! Create the data structure in charge of holding the 
	! FFD box properties for each element
	ALLOCATE(FFDBoxProperties(NumElements, NZFFD,5))
	
	
	! make the properties of the FFD box for each object
	DO 10 intH = 1, NumElements
	!Calculate the exact seperation of the Z Cross sections
	!By using the objects minimum and maximum z values	
	realExactdz = (MaxZ(intH,1) - 
     . 	MinZ(intH,1))/(Real(NZFFD)-1.0)

!	WRITE(*,*) "readDz: ", realExactdz
	
	realFFDZMin = MinZ(intH,1)
	realFFDZMax = MaxZ(intH,1)

!	WRITE(*,*) "realExactdx: ", realExactdz	
	! Fill first the z values of each cross section
	DO 20 intl = 0, (NZFFD-1)
	realZvalue = realFFDZMin + Real(intl)*realExactdz
	FFDBoxProperties(intH, intl+1, 1) = realZValue
  20	CONTINUE
	
	!Go through the cross section slices that are near 
	! each FFD cross section to find how big to make the FFD
	! plane
	DO 30 intl = 1, NZFFD

	!Find the z value of the exact FFD cross section of interest
	realFFDCrossSectZ = FFDBoxProperties(intH, intl,1)

	realZCrossMin = realFFDCrossSectZ - 0.125*(realExactdz)
	realZCrossMax = realFFDCrossSectZ + 0.125*(realExactdz)
	
		!At one end of the object (min z value)
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
	! 	1. both z max and zmin are in the range
	! 	2. If ZCrossMin is between the max and min slice
	! 	3. If ZCrossMax is between the max and min
	IF(((realZMaxSlice .LE. realZCrossMax) .AND.
     . 	 (realZMinSlice .GE. realZCrossMin)) .OR. 
     .	((realZCrossMin .LE. realZMaxSlice) .AND. 
     . 	(realZCrossMin .GE. realZMinSlice)) .OR. 
     . 	((realZCrossMax .LE. realZMaxSlice) .AND. 
     . 	(realZCrossMax .GE. realZMinSlice))) THEN

	realAverageXMax = realAverageXMax + 
     . 	ZCrossSectionsData(intH,intm,2)	
	realAverageXMin = realAverageXMin + 
     . 	ZCrossSectionsData(intH,intm,1)	
	realAverageYMax = realAverageYMax + 
     . 	ZCrossSectionsData(intH,intm,4)	
	realAverageYMin = realAverageYMin + 
     . 	ZCrossSectionsData(intH,intm,3)		
	intNumValues = intNumValues + 1	
	ENDIF

  40	CONTINUE

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
  30	CONTINUE	
  10	CONTINUE
	! We now have the properties for each FFD box that needs to
	! be made.


	DO 50 intH=1, NumElements
	!Create the FFD rects now for the z cross sections
	DO 60 intm = 1, NZFFD
		CALL CREATEFFDRECTEXACT(intH, intm)			
  60	CONTINUE
  50	CONTINUE	


	END




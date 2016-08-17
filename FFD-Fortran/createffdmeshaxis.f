

	! The subroutine that is used for creating the initial FFD mesh
	! around the object

	SUBROUTINE CREATEFFDMESHAXIS(H)
	USE VAR

	! The index of the element
	INTEGER :: H

	intH = H
	! make the properties of the FFD box for the object
	IF (AxisDirection(intH) .EQ. 1) THEN
	CALL INITIALIZEBOXX(intH)
	ENDIF
	
	IF (AxisDirection(intH) .EQ. 2) THEN
	CALL INITIALIZEBOXY(intH)
	ENDIF
	
	IF(AxisDirection(intH) .EQ. 3) THEN
	CALL INITIALIZEBOXZ(intH)
	ENDIF



	! If a plane on this element's FFD box will eventually
	! be connected to another box then we must make that plane
	! flat (for now do this, eventually make this plane arbitrarily
	! shaped). 

	! loop over all the faces of this element
	DO 20 intF = 1,6
	IF(ConnectivityInfo(intH,intF,1) .NE. -1) THEN
	! We have a face with a connection to another face
	intElem2 = ConnectivityInfo(intH,intF,1)
	
	!Check to make sure this element comes next in the queue
	! ranking system (i.e. it is processed after)
	DO 21 intR = 1,NumElements
	! Checking all the elements that came before this one
	IF(intR .GT. Rank(intH)) THEN
	IF(Rank(intR) .EQ. intElem2) THEN
		! Make the plane of FFD points flat
	CALL MAKEFLATPLANE(intH,intF)
	
	ENDIF
	ENDIF
  21	CONTINUE	
		
	ENDIF	
  20	CONTINUE



	
	! If we are building along the z axis
	IF(AxisDirection(intH) .EQ. 1) THEN	
	DO 10 intm = 1,NXFFD(H)
	
	WRITE(*,*) "ZMax: ",FFDVolProperties(H,intm,6)
	WRITE(*,*) "ZMin: ",FFDVolProperties(H,intm,7) 		


  10	CONTINUE
	ENDIF




!	WRITE(*,*) "Completed Initialize Box"

	CALL FILLFFDDATAAXIS(intH)

	END







	SUBROUTINE INITIALIZEBOXX(H)
	USE VAR

	INTEGER::H
	
	! Find the number of planes that are being placed along the 
	! x axis
	intNumPlanes = NXFFD(H) !This will be read from a data structure
	

	!Calculate the exact seperation of the X Cross sections
        !By using the objects minimum and maximum x values      
        realExactdx = (MaxValueElem(H,1) -
     .  MinValueElem(H,1))/(Real(intNumPlanes)-1.0)

	realFFDXMin = MinValueElem(H,1)
        realFFDXMax = MaxValueElem(H,1)

	! Fill first the x values of each cross section
        DO 30 intl = 0, (intNumPlanes-1)
        realXvalue = realFFDXMin + Real(intl)*realExactdx
		!1st index holds the plane's location along the axis chosen
        FFDVolProperties(H, intl+1, 1) = realXValue
  30    CONTINUE	

	! Go through the cross section slices that are near 
        ! each FFD cross section to find how big to make the FFD
        ! plane
	DO 31 intl = 1,intNumPlanes

	!Find the x value of the exact FFD cross section of interest
        realFFDCrossSectX = FFDVolProperties(H, intl,1)

	!The range around the plane for which data will be needed
	realXCrossMin = realFFDCrossSectX - 0.125*(realExactdx)
        realXCrossMax = realFFDCrossSectX + 0.125*(realExactdx)
	
         !At one end of the object (min z value)
        IF (intl .EQ. 1) THEN
                realXCrossMin = realFFDCrossSectX
                realXCrossMax = realFFDCrossSectX + 0.125*realExactdx
        ENDIF
                !At the other end of the object (max z value)
        IF (intl .EQ. NZFFD(H)) THEN
                realXCrossMax = realFFDCrossSectX
                realXCrossMin = realFFDCrossSectX - 0.125*realExactdx
        ENDIF

	
	realAverageXMax = 0.0
	realAverageXMin = 0.0
	realAverageYMax = 0.0
        realAverageYMin = 0.0
        realAverageZMax = 0.0
        realAverageZMin = 0.0
        intNumValues = 0

        DO 40 intm = 1, CrossSectionsSize(H)
        realXMinSlice = CrossSectionsData(H,intm,1)
        realXMaxSlice = CrossSectionsData(H,intm,2)

	        ! A slice is considered to be in the vicinity if any
        ! part of the slice falls in the range. Check if:
        !       1. both z max and zmin are in the range
        !       2. If ZCrossMin is between the max and min slice
        !       3. If ZCrossMax is between the max and min
        IF(((realXMaxSlice .LE. realXCrossMax) .AND.
     .   (realXMinSlice .GE. realXCrossMin)) .OR.
     .  ((realXCrossMin .LE. realXMaxSlice) .AND.
     .  (realXCrossMin .GE. realXMinSlice)) .OR.
     .  ((realXCrossMax .LE. realXMaxSlice) .AND.
     .  (realXCrossMax .GE. realXMinSlice))) THEN

	realAverageZMax = realAverageZMax +
     .  CrossSectionsData(H,intm,6)
        realAverageZMin = realAverageZMin +
     .  CrossSectionsData(H,intm,5)
        realAverageYMax = realAverageYMax +
     .  CrossSectionsData(H,intm,4)
        realAverageYMin = realAverageYMin +
     .  CrossSectionsData(H,intm,3)
        intNumValues = intNumValues + 1
	ENDIF
  40	CONTINUE

	realAverageZMax = realAverageZMax/REAL(intNumValues)
        realAverageZMin = realAverageZMin/REAL(intNumValues)
        realAverageYMax = realAverageYMax/REAL(intNumValues)
        realAverageYMin = realAverageYMin/REAL(intNumValues)

	!Now, with the data from the closest physical cross section
        ! fill the data for the exact cross section
        FFDVolProperties(H,intl,2) = realAverageXMax
        FFDVolProperties(H,intl,3) = realAverageXMin
        FFDVolProperties(H,intl,4) = realAverageYMax
        FFDVolProperties(H,intl,5) = realAverageYMin
		! The z values aren't used for this volume
	FFDVolProperties(H,intl,6) = realAverageZMax
	FFDVolProperties(H,intl,7) = realAverageZMin


  31	CONTINUE

	END

	SUBROUTINE MAKEFLATPLANE(intH,intF)
	USE VAR
	! The element and it's ffd face which must be flat
	INTEGER :: intH, intF

	! Only make the FFD plane flat if it isn't orthogonal to
	! the axis that was chosen to place points along. If this
	! is the case, then the plane will be flat already

	intAxis = AxisDirection(intH)

	! The x direction was chosen	
	IF (intAxis .EQ. 1) THEN
	IF ((intF .NE. 1) .AND. (intF .NE. 2)) THEN
	! Making sure that the face is not orthogonal 
	! to the axis chosen. Also supply the number of 
	! planes to change the data for
	CALL CHANGEGLOBALDIM(intH, intF, NXFFD(intH))
	ENDIF
	ENDIF

	! The y direction was chosen
	IF (intAxis .EQ. 2) THEN
	IF ((intF .NE. 3) .AND. (intF .NE. 4)) THEN
	! Making sure that the face is not orthogonal 
	! to the axis chosen
	CALL CHANGEGLOBALDIM(intH, intF, NYFFD(intH))
	ENDIF
	ENDIF

	! The z direction was chosen
	IF (intAxis .EQ. 3) THEN
	IF ((intF .NE. 5) .AND. (intF .NE. 6)) THEN
	! Making sure that the face is not orthogonal 
	! to the axis chosen
	CALL CHANGEGLOBALDIM(intH, intF, NZFFD(intH))
	ENDIF
	ENDIF		 
	

	END

	SUBROUTINE CHANGEGLOBALDIM(intH,intF,intNumPlanes)
	USE VAR
	
	INTEGER :: intH, intF, intNumPlanes

	IF (intF .EQ. 1) THEN
	! Get the maximum x value of all the cross sections
	! and set all the cross sections to have that value
	! for max x

	!Initializing the max x value
	realMaxX = FFDVolProperties(intH,1,2)
	DO 10 intI = 1,intNumPlanes
	realMaxX = MAX(realMaxX, FFDVolProperties(intH,intI,2))
  10	CONTINUE

	! Change all cross sections max x value to be the global max
	DO 11 intI = 1,intNumPlanes
	FFDVolProperties(intH,intI,2) = realMaxX
  11	CONTINUE
	ENDIF

	
	! Change X min
	IF (intF .EQ. 2) THEN

	!Initializing the min x value
	realMinX = FFDVolProperties(intH,1,3)
	DO 20 intI = 1,intNumPlanes
	realMinX = MIN(realMinX, FFDVolProperties(intH,intI,3))
  20	CONTINUE

	! Change all cross sections max x value to be the global max
	DO 21 intI = 1,intNumPlanes
	FFDVolProperties(intH,intI,3) = realMinX
  21	CONTINUE
	ENDIF

	! Max Y
	IF (intF .EQ. 3) THEN
	realMaxY = FFDVolProperties(intH,1,4)
	DO 30 intI = 1,intNumPlanes
	realMaxY = MAX(realMaxY, FFDVolProperties(intH,intI,4))
  30	CONTINUE

	DO 31 intI = 1,intNumPlanes
	FFDVolProperties(intH,intI,4) = realMaxY
  31	CONTINUE
	ENDIF

	
	! Min Y
	IF (intF .EQ. 4) THEN

	!Initializing the min x value
	realMinY = FFDVolProperties(intH,1,5)
	DO 40 intI = 1,intNumPlanes
	realMinY = MIN(realMinY, FFDVolProperties(intH,intI,5))
  40	CONTINUE

	! Change all cross sections max x value to be the global max
	DO 41 intI = 1,intNumPlanes
	FFDVolProperties(intH,intI,5) = realMinY
  41	CONTINUE
	ENDIF


	! Max Z
	IF (intF .EQ. 5) THEN

	realMaxZ = FFDVolProperties(intH,1,6)
	DO 50 intI = 1,intNumPlanes
	realMaxZ = MAX(realMaxZ, FFDVolProperties(intH,intI,6))
	!WRITE(*,*) "max z: ", realMaxZ
	!WRITE(*,*) "min z: ", FFDVolProperties(intH,intI,7)
  50	CONTINUE

	! Change all cross sections max x value to be the global max
	DO 51 intI = 1,intNumPlanes
	FFDVolProperties(intH,intI,6) = realMaxZ
  51	CONTINUE
	ENDIF

	
	! Min Z
	IF (intF .EQ. 6) THEN
	realMinZ = FFDVolProperties(intH,1,7)
	DO 60 intI = 1,intNumPlanes
	realMinZ = MIN(realMinZ, FFDVolProperties(intH,intI,7))
  60	CONTINUE

	DO 61 intI = 1,intNumPlanes
	FFDVolProperties(intH,intI,7) = realMinZ
  61	CONTINUE
	ENDIF



	END


	SUBROUTINE INITIALIZEBOXY(H)
	USE VAR

	INTEGER::H
	
	! Find the number of planes that are being placed along the 
	! x axis
	intNumPlanes = NYFFD(H) !This will be read from a data structure

	!Calculate the exact seperation of the Y Cross sections
        !By using the objects minimum and maximum y values      
        realExactdy = (MaxValueElem(H,2) -
     .  MinValueElem(H,2))/(Real(intNumPlanes)-1.0)

	realFFDYMin = MinValueElem(H,2)
        realFFDYMax = MaxValueElem(H,2)

	! Fill first the y values of each cross section
        DO 30 intl = 0, (intNumPlanes-1)
        realYvalue = realFFDYMin + Real(intl)*realExactdy
		!1st index holds the plane's location along the axis chosen
        FFDVolProperties(H, intl+1, 1) = realYValue
  30    CONTINUE	

	! Go through the cross section slices that are near 
        ! each FFD cross section to find how big to make the FFD
        ! plane
	DO 31 intl = 1,intNumPlanes

	!Find the y value of the exact FFD cross section of interest
        realFFDCrossSectY = FFDVolProperties(H, intl,1)

	!The range around the plane for which data will be needed
	realYCrossMin = realFFDCrossSectY - 0.125*(realExactdy)
        realYCrossMax = realFFDCrossSectY + 0.125*(realExactdy)
	
         !At one end of the object (min y value)
        IF (intl .EQ. 1) THEN
                realYCrossMin = realFFDCrossSectY
                realYCrossMax = realFFDCrossSectY + 0.125*realExactdy
        ENDIF
                !At the other end of the object (max y value)
        IF (intl .EQ. NYFFD(H)) THEN
                realYCrossMax = realFFDCrossSectY
                realYCrossMin = realFFDCrossSectY - 0.125*realExactdy
        ENDIF

	
	realAverageXMax = 0.0
	realAverageXMin = 0.0
	realAverageYMax = 0.0
        realAverageYMin = 0.0
        realAverageZMax = 0.0
        realAverageZMin = 0.0
        intNumValues = 0

        DO 40 intm = 1, CrossSectionsSize(H)
        realYMinSlice = CrossSectionsData(H,intm,3)
        realYMaxSlice = CrossSectionsData(H,intm,4)

	        ! A slice is considered to be in the vicinity if any
        ! part of the slice falls in the range. Check if:
        !       1. both z max and zmin are in the range
        !       2. If ZCrossMin is between the max and min slice
        !       3. If ZCrossMax is between the max and min
        IF(((realYMaxSlice .LE. realYCrossMax) .AND.
     .   (realYMinSlice .GE. realYCrossMin)) .OR.
     .  ((realYCrossMin .LE. realYMaxSlice) .AND.
     .  (realYCrossMin .GE. realYMinSlice)) .OR.
     .  ((realYCrossMax .LE. realYMaxSlice) .AND.
     .  (realYCrossMax .GE. realYMinSlice))) THEN

	realAverageZMax = realAverageZMax +
     .  CrossSectionsData(H,intm,6)
        realAverageZMin = realAverageZMin +
     .  CrossSectionsData(H,intm,5)
        realAverageXMax = realAverageXMax +
     .  CrossSectionsData(H,intm,2)
        realAverageXMin = realAverageXMin +
     .  CrossSectionsData(H,intm,1)
        intNumValues = intNumValues + 1
	ENDIF
  40	CONTINUE

	realAverageZMax = realAverageZMax/REAL(intNumValues)
        realAverageZMin = realAverageZMin/REAL(intNumValues)
        realAverageXMax = realAverageXMax/REAL(intNumValues)
        realAverageXMin = realAverageXMin/REAL(intNumValues)

	!Now, with the data from the closest physical cross section
        ! fill the data for the exact cross section
        FFDVolProperties(H,intl,2) = realAverageXMax
        FFDVolProperties(H,intl,3) = realAverageXMin
        FFDVolProperties(H,intl,4) = realAverageYMax
        FFDVolProperties(H,intl,5) = realAverageYMin
		
	FFDVolProperties(H,intl,6) = realAverageZMax
	FFDVolProperties(H,intl,7) = realAverageZMin


  31	CONTINUE

	
	END


	
	



	

	! The subroutine that is used for initializing the box in the
	! z direction
	SUBROUTINE INITIALIZEBOXZ(H)
	
	USE VAR

	INTEGER::H	
	
	! Find the number of planes that are being placed along the 
	! z axis
	intNumPlanes = NZFFD(H) !This will be read from a data structure
	

	!Calculate the exact seperation of the Z Cross sections
        !By using the objects minimum and maximum z values      
        realExactdz = (MaxValueElem(H,3) -
     .  MinValueElem(H,3))/(Real(intNumPlanes)-1.0)

	realFFDZMin = MinValueElem(H,3)
        realFFDZMax = MaxValueElem(H,3)

	! Fill first the z values of each cross section
        DO 30 intl = 0, (intNumPlanes-1)
        realZvalue = realFFDZMin + Real(intl)*realExactdz
		!1st index holds the plane index
        FFDVolProperties(H, intl+1, 1) = realZValue
  30    CONTINUE	

	! Go through the cross section slices that are near 
        ! each FFD cross section to find how big to make the FFD
        ! plane
	DO 31 intl = 1,intNumPlanes

	!Find the z value of the exact FFD cross section of interest
        realFFDCrossSectZ = FFDVolProperties(H, intl,1)

	!The range around the plane for which data will be needed
	realZCrossMin = realFFDCrossSectZ - 0.125*(realExactdz)
        realZCrossMax = realFFDCrossSectZ + 0.125*(realExactdz)
	
         !At one end of the object (min z value)
        IF (intl .EQ. 1) THEN
                realZCrossMin = realFFDCrossSectZ
                realZCrossMax = realFFDCrossSectZ + 0.125*realExactdz
        ENDIF
                !At the other end of the object (max z value)
        IF (intl .EQ. NZFFD(H)) THEN
                realZCrossMax = realFFDCrossSectZ
                realZCrossMin = realFFDCrossSectZ - 0.125*realExactdz
        ENDIF

	

	realAverageXMax = 0.0
        realAverageXMin = 0.0
        realAverageYMax = 0.0
        realAverageYMin = 0.0
        realAverageZMax = 0.0
	realAverageZMin = 0.0
	intNumValues = 0

        DO 40 intm = 1, CrossSectionsSize(H)
        realZMinSlice = CrossSectionsData(H,intm,5)
        realZMaxSlice = CrossSectionsData(H,intm,6)

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
     .  CrossSectionsData(H,intm,2)
        realAverageXMin = realAverageXMin +
     .  CrossSectionsData(H,intm,1)
        realAverageYMax = realAverageYMax +
     .  CrossSectionsData(H,intm,4)
        realAverageYMin = realAverageYMin +
     .  CrossSectionsData(H,intm,3)
        intNumValues = intNumValues + 1
	ENDIF
  40	CONTINUE

	realAverageXMax = realAverageXMax/REAL(intNumValues)
        realAverageXMin = realAverageXMin/REAL(intNumValues)
        realAverageYMax = realAverageYMax/REAL(intNumValues)
        realAverageYMin = realAverageYMin/REAL(intNumValues)

	!Now, with the data from the closest physical cross section
        ! fill the data for the exact cross section
        FFDVolProperties(H,intl,2) = realAverageXMax
        FFDVolProperties(H,intl,3) = realAverageXMin
        FFDVolProperties(H,intl,4) = realAverageYMax
        FFDVolProperties(H,intl,5) = realAverageYMin
		! The z values aren't used for this volume
	FFDVolProperties(H,intl,6) = realAverageZMax
	FFDVolProperties(H,intl,7) = realAverageZMin


  31	CONTINUE



	END




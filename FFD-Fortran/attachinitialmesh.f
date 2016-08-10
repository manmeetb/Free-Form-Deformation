
! This subroutine is in charge of attaching the initial mesh on the object
	
	SUBROUTINE ATTACHINITIALMESH()
	
	USE VAR

	! To attach the initial mesh, first we are going to create
	! a set of arrays that will hold information for each element
	
        ! Allocate the array that will be used for holding the number of cross sections
        ! for each element
        ALLOCATE(ZCrossSectionsSize(NumElements,1))

        !Allocate the array that will hold the max and min Z values of the element
        ALLOCATE(MaxZ(NumElements,1))
        ALLOCATE(MinZ(NumElements,1))


	!Set a number for the number of slices of the body that will be 
	! studied. These slices will have a min and max z value. Then,
	! the max and min x and y values will be found also and stored. Then,
	! When a plane of points will need to be put, it will study the data 
	! from the slices that are in its viscinity

	! For the wing, the seperation of planes of points is quite wide 
	! near the center of the wing. There are about 60 planes so set the
	! number of slices to be around 20 just to be safe
	intNumSlices = 20
	DO 30 intH = 1,NumElements
	ZCrossSectionsSize(intH,1) = intNumSlices
  30	CONTINUE

	! 6 entries will now be in the cross section data because the 
	! depth of the slice needs to be known
	ALLOCATE(ZCrossSectionsData(NumElements,intNumSlices,6))	

	! Loop through all the points once to first get the minimum
	! and maximum z values

	DO 11 intH = 1, NumElements
       	DO 10 intI = 1, NumSolidBoundaryPoints(intH,1)
        realCurrentZ = SolidBoundaryPoints(intH, intI, 3)

       	IF (intI .EQ. 1) THEN
        MaxZ(intH,1) = realCurrentZ
        MinZ(intH,1) = realCurrentZ
        ENDIF
        
	! keeping track of the maximum and min z values of the object
        IF (realCurrentZ .GT. MaxZ(intH,1)) THEN
        MaxZ(intH,1) = realCurrentZ
        ENDIF  

        IF (realCurrentZ .LT. MinZ(intH,1)) THEN
        MinZ(intH,1) = realCurrentZ
        ENDIF
  10    CONTINUE
  11	CONTINUE 

	! Do this a more elegant way later
	realInit = -10000.0

	! Loop through each element
	DO 21 intH = 1,NumElements
	
	!compute the z thickness of each slice for this element
	realdz = (MaxZ(intH,1) - MinZ(intH,1))/REAL(intNumSlices)
	realMinZ = MinZ(intH,1)
	realMaxZ = MaxZ(intH,1)
	
	! set the z limits of the slices first
	DO 22 intI = 1,ZCrossSectionsSize(intH,1)
	realZ1 = realMinZ + REAL(intI-1)*realdz
	realZ2 = realZ1 + realdz
	
	ZCrossSectionsData(intH,intI,1) = realInit !Xmin
	ZCrossSectionsData(intH,intI,2) = realInit !Xmax
	ZCrossSectionsData(intH,intI,3) = realInit !Ymin
	ZCrossSectionsData(intH,intI,4) = realInit !Ymax

	ZCrossSectionsData(intH,intI,5) = realZ1 !Zmin
	ZCrossSectionsData(intH,intI,6) = realZ2 !Zmax
  22	CONTINUE 	



	! Loop through all the solid points and fill the data 
	! for the slices
	DO 23 intI = 1,NumSolidBoundaryPoints(intH,1)
	realXValue = SolidBoundaryPoints(intH,intI,1)
	realYValue = SolidBoundaryPoints(intH,intI,2)
	realZValue = SolidBoundaryPoints(intH,intI,3)

	!Find the cross section the point belongs to
	DO 24 intJ = 1, ZCrossSectionsSize(intH,1)
	realZCrossMin = ZCrossSectionsData(intH,intJ,5)
	realZCrossMax = ZCrossSectionsData(intH,intJ,6)

	!WRITE(*,*) "realZCrossMin: ", realZCrossMin
	!WRITE(*,*) "realZCrossMax: ", realZCrossMax

	IF ((realZValue .LE. realZCrossMax) .AND. 
     . 	(realZValue .GE. realZCrossMin)) THEN
	! found the relevant slice
	
	! See if this is the first point to be in the slice
	IF (ZCrossSectionsData(intH,intJ,1) .EQ. 
     . 	realInit) THEN
	! Initialize all the data for the cross section
	ZCrossSectionsData(intH,intJ,1) = realXValue !Xmin
	ZCrossSectionsData(intH,intJ,2) = realXValue !Xmax
	ZCrossSectionsData(intH,intJ,3) = realYValue !Ymin
	ZCrossSectionsData(intH,intJ,4) = realYValue !Ymax
	
	ELSE

	! this is not the first point to be processed for this cross
	! section

	!Check the x max and xmin values
	IF (realXValue .LT.
     .  ZCrossSectionsData(intH,intJ, 1)) THEN
        ZCrossSectionsData(intH, intJ, 1) = realXValue
        ENDIF

        IF (realXValue .GT.
     .  ZCrossSectionsData(intH, intJ, 2)) THEN
        ZCrossSectionsData(intH, intJ, 2) = realXValue
        ENDIF

        ! check y max and min
        IF (realYValue .LT.
     .  ZCrossSectionsData(intH, intJ, 3)) THEN
        ZCrossSectionsData(intH,intJ,3) = realYValue
        ENDIF

        IF (realYValue .GT.
     .  ZCrossSectionsData(intH, intJ, 4)) THEN
        ZCrossSectionsData(intH,intJ,4) = realYValue
        ENDIF

	ENDIF !If statement checking whether this point has been seen
	ENDIF !If statement for the relevant slice found
  24	CONTINUE ! loop through the cross sections

  23	CONTINUE ! loop through all the points for the element	
  21	CONTINUE ! loop through all the elements


	END



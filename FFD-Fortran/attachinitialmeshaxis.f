

	! The subroutine that is used for placing the initial FFD
	! lattice
	SUBROUTINE ATTACHINITIALMESHAXIS(H)
	
	USE VAR

	! The index of the element we are working with
	INTEGER :: H

	intH = H
	! Loop through all the element's points once to get 
	! the limit values for it (x max/min, etc)

	indexFirstFlag = 0
        DO 30 intI = 1, SolidBoundaryPointsSize
        IF (SolidBoundaryPoints(intI,7) .EQ. intH) THEN
	! If this point belongs to our element
	realX = SolidBoundaryPoints(intI,1)
	realY = SolidBoundaryPoints(intI,2)
	realZ = SolidBoundaryPoints(intI,3)
	
        IF (indexFirstFlag .EQ. 0) THEN
	! If this is the first point, set the initial values
	MaxValueElem(intH,1) = realX
	MaxValueElem(intH,2) = realY
	MaxValueElem(intH,3) = realZ

	MinValueElem(intH,1) = realX	
	MinValueElem(intH,2) = realY
	MinValueElem(intH,3) = realZ
	indexFirstFlag = 1
        ENDIF

	! Keeping track of the maximum and minimum values
	CALL SETMAXMIN(intH, realX, realY, realZ)
	ENDIF
  30    CONTINUE


	! Print all the data
	DO 200 intJ = 1,3
!	WRITE(*,*) "Max",intJ,":",MaxValueElem(intH,intJ)
  200	CONTINUE
	DO 300 intJ = 1,3
!	WRITE(*,*) "Min",intJ,":",MinValueElem(intH,intJ)
  300	CONTINUE
  


	! Loop through each element and compute the data for the
	! slices of the object depending on the axis direction
	! that was chosen

	
	IF (AxisDirection(intH) .EQ. 1) THEN
	CALL COMPUTEDATAX(intH)
	ENDIF	

	IF (AxisDirection(intH) .EQ. 2) THEN
	CALL COMPUTEDATAY(intH)
	ENDIF

	IF (AxisDirection(intH) .EQ. 3) THEN
	CALL COMPUTEDATAZ(intH)
	ENDIF
	

	END





	! For computing the slice data in the x direction
	SUBROUTINE COMPUTEDATAX(H)
	USE VAR
	! The element's index
	INTEGER::H

		! The number of slices to use
	intNumSlices = CrossSectionsSize(H)
	realInit = -10000.0
	
	!compute the x thickness of each slice for this element
        realdx = (MaxValueElem(H,1) - 
     . 		MinValueElem(H,1))/REAL(intNumSlices)
        realMinX = MinValueElem(H,1)
        realMaxX = MaxValueElem(H,1)

        ! set the z limits of the slices first
        DO 50 intI = 1,CrossSectionsSize(H)
        realX1 = realMinX + REAL(intI-1)*realdx
        realX2 = realX1 + realdx

        CrossSectionsData(H,intI,1) = realX1 !Xmin
        CrossSectionsData(H,intI,2) = realX2 !Xmax

        CrossSectionsData(H,intI,3) = realInit !Ymin
        CrossSectionsData(H,intI,4) = realInit !Ymax

        CrossSectionsData(H,intI,5) = realInit !Zmin
        CrossSectionsData(H,intI,6) = realInit !Zmax
  50    CONTINUE

	! Loop through all the element's points now and fill
	! the data for the slices
	DO 51 intI = 1,SolidBoundaryPointsSize
	IF(SolidBoundaryPoints(intI,7) .EQ. H) THEN
	realXValue = SolidBoundaryPoints(intI,1)
        realYValue = SolidBoundaryPoints(intI,2)
        realZValue = SolidBoundaryPoints(intI,3)

	
	!Find the cross section the point belongs to
	DO 52 intJ = 1,CrossSectionsSize(H)
	realXCrossMin = CrossSectionsData(H,intJ,1)
        realXCrossMax = CrossSectionsData(H,intJ,2)

	IF ((realXValue .LE. realXCrossMax) .AND.
     .  (realXValue .GE. realXCrossMin)) THEN
	! We are in the relevant slice now
	
	! See if this is the first point to be in the slice
        IF (CrossSectionsData(H,intJ,3) .EQ.
     .  realInit) THEN
        ! Initialize all the data for the cross section
        CrossSectionsData(H,intJ,5) = realZValue !Zmin
        CrossSectionsData(H,intJ,6) = realZValue !Zmax
        CrossSectionsData(H,intJ,3) = realYValue !Ymin
        CrossSectionsData(H,intJ,4) = realYValue !Ymax
        ELSE

	!Check the z max and z min values
        IF (realZValue .LT.
     .  CrossSectionsData(H,intJ, 5)) THEN
        CrossSectionsData(H, intJ, 5) = realZValue
        ENDIF

        IF (realZValue .GT.
     .  CrossSectionsData(H, intJ, 6)) THEN
        CrossSectionsData(H, intJ, 6) = realZValue
        ENDIF

        ! check y max and min
        IF (realYValue .LT.
     .  CrossSectionsData(H, intJ, 3)) THEN
        CrossSectionsData(H,intJ,3) = realYValue
        ENDIF

        IF (realYValue .GT.
     .  CrossSectionsData(H, intJ, 4)) THEN
        CrossSectionsData(H,intJ,4) = realYValue
        ENDIF

	! End of check for whether this point is the first
	! in the slice's data
	ENDIF
	ENDIF !End of relevant slice if statement
  52	CONTINUE !End of Cross sections loop
	ENDIF
  51	CONTINUE !End of Solid points loop

	
	! Print all the cross section data
	DO 210 intJ = 1,CrossSectionsSize(H)
!	WRITE(*,*) "Cross Sect Index: ", intJ
	DO 220 intm = 1,6
!	WRITE(*,*) intm, ": ", CrossSectionsData(H,intJ,intm)

  220	CONTINUE
  210	CONTINUE
 





	END

	! For computing the slice data in the x direction
	SUBROUTINE COMPUTEDATAY(H)
	USE VAR
	! The element's index
	INTEGER::H

		! The number of slices to use
	intNumSlices = CrossSectionsSize(H)
	realInit = -10000.0
	
	!compute the y thickness of each slice for this element
        realdy = (MaxValueElem(H,2) - 
     . 		MinValueElem(H,2))/REAL(intNumSlices)
        realMinY = MinValueElem(H,2)
        realMaxY = MaxValueElem(H,2)

        ! set the y limits of the slices first
        DO 50 intI = 1,CrossSectionsSize(H)
        realY1 = realMinY + REAL(intI-1)*realdy
        realY2 = realY1 + realdy

        CrossSectionsData(H,intI,1) = realInit !Xmin
        CrossSectionsData(H,intI,2) = realInit !Xmax

        CrossSectionsData(H,intI,3) = realY1 !Ymin
        CrossSectionsData(H,intI,4) = realY2 !Ymax

        CrossSectionsData(H,intI,5) = realInit !Zmin
        CrossSectionsData(H,intI,6) = realInit !Zmax
  50    CONTINUE

	! Loop through all the element's points now and fill
	! the data for the slices
	DO 51 intI = 1,SolidBoundaryPointsSize
	IF(SolidBoundaryPoints(intI,7) .EQ. H) THEN
	realXValue = SolidBoundaryPoints(intI,1)
        realYValue = SolidBoundaryPoints(intI,2)
        realZValue = SolidBoundaryPoints(intI,3)

	
	!Find the cross section the point belongs to
	DO 52 intJ = 1,CrossSectionsSize(H)
	realYCrossMin = CrossSectionsData(H,intJ,3)
        realYCrossMax = CrossSectionsData(H,intJ,4)

	IF ((realYValue .LE. realYCrossMax) .AND.
     .  (realYValue .GE. realYCrossMin)) THEN
	! We are in the relevant slice now
	
	! See if this is the first point to be in the slice
        IF (CrossSectionsData(H,intJ,1) .EQ.
     .  realInit) THEN
        ! Initialize all the data for the cross section
        CrossSectionsData(H,intJ,1) = realXValue !Xmin
        CrossSectionsData(H,intJ,2) = realXValue !Xmax
        CrossSectionsData(H,intJ,5) = realZValue !Zmin
        CrossSectionsData(H,intJ,6) = realZValue !Zmax
        ELSE

	!Check the z max and z min values
        IF (realZValue .LT.
     .  CrossSectionsData(H,intJ, 5)) THEN
        CrossSectionsData(H, intJ, 5) = realZValue
        ENDIF

        IF (realZValue .GT.
     .  CrossSectionsData(H, intJ, 6)) THEN
        CrossSectionsData(H, intJ, 6) = realZValue
        ENDIF

        ! check x max and min
        IF (realXValue .LT.
     .  CrossSectionsData(H, intJ, 1)) THEN
        CrossSectionsData(H,intJ,1) = realXValue
        ENDIF

        IF (realYValue .GT.
     .  CrossSectionsData(H, intJ, 2)) THEN
        CrossSectionsData(H,intJ,2) = realYValue
        ENDIF

	! End of check for whether this point is the first
	! in the slice's data
	ENDIF
	ENDIF !End of relevant slice if statement
  52	CONTINUE !End of Cross sections loop
	ENDIF
  51	CONTINUE !End of Solid points loop

	
	! Print all the cross section data
	DO 210 intJ = 1,CrossSectionsSize(H)
!	WRITE(*,*) "Cross Sect Index: ", intJ
	DO 220 intm = 1,6
!	WRITE(*,*) intm, ": ", CrossSectionsData(H,intJ,intm)

  220	CONTINUE
  210	CONTINUE
 

	END



	! For computing the slice data in the x direction
	SUBROUTINE COMPUTEDATAZ(H)
	USE VAR
	! The element's index
	INTEGER::H
		! The number of slices to use
	intNumSlices = CrossSectionsSize(H)
	realInit = -10000.0
	
	!compute the z thickness of each slice for this element
        realdz = (MaxValueElem(H,3) - 
     . 		MinValueElem(H,3))/REAL(intNumSlices)
        realMinZ = MinValueElem(H,3)
        realMaxZ = MaxValueElem(H,3)

        ! set the z limits of the slices first
        DO 50 intI = 1,CrossSectionsSize(H)
        realZ1 = realMinZ + REAL(intI-1)*realdz
        realZ2 = realZ1 + realdz

        CrossSectionsData(H,intI,1) = realInit !Xmin
        CrossSectionsData(H,intI,2) = realInit !Xmax
        CrossSectionsData(H,intI,3) = realInit !Ymin
        CrossSectionsData(H,intI,4) = realInit !Ymax

        CrossSectionsData(H,intI,5) = realZ1 !Zmin
        CrossSectionsData(H,intI,6) = realZ2 !Zmax
  50    CONTINUE

	! Loop through all the element's points now and fill
	! the data for the slices
	DO 51 intI = 1,SolidBoundaryPointsSize
	IF (SolidBoundaryPoints(intI,7) .EQ. H) THEN
	realXValue = SolidBoundaryPoints(intI,1)
        realYValue = SolidBoundaryPoints(intI,2)
        realZValue = SolidBoundaryPoints(intI,3)

	
	!Find the cross section the point belongs to
	DO 52 intJ = 1,CrossSectionsSize(H)
	realZCrossMin = CrossSectionsData(H,intJ,5)
        realZCrossMax = CrossSectionsData(H,intJ,6)

	IF ((realZValue .LE. realZCrossMax) .AND.
     .  (realZValue .GE. realZCrossMin)) THEN
	! We are in the relevant slice now
	
	! See if this is the first point to be in the slice
        IF (CrossSectionsData(H,intJ,1) .EQ.
     .  realInit) THEN
        ! Initialize all the data for the cross section
        CrossSectionsData(H,intJ,1) = realXValue !Xmin
        CrossSectionsData(H,intJ,2) = realXValue !Xmax
        CrossSectionsData(H,intJ,3) = realYValue !Ymin
        CrossSectionsData(H,intJ,4) = realYValue !Ymax
        ELSE

	!Check the x max and xmin values
        IF (realXValue .LT.
     .  CrossSectionsData(H,intJ, 1)) THEN
        CrossSectionsData(H, intJ, 1) = realXValue
        ENDIF

        IF (realXValue .GT.
     .  CrossSectionsData(H, intJ, 2)) THEN
        CrossSectionsData(H, intJ, 2) = realXValue
        ENDIF

        ! check y max and min
        IF (realYValue .LT.
     .  CrossSectionsData(H, intJ, 3)) THEN
        CrossSectionsData(H,intJ,3) = realYValue
        ENDIF

        IF (realYValue .GT.
     .  CrossSectionsData(H, intJ, 4)) THEN
        CrossSectionsData(H,intJ,4) = realYValue
        ENDIF

	! End of check for whether this point is the first
	! in the slice's data
	ENDIF
	ENDIF !End of relevant slice if statement
  52	CONTINUE !End of Cross sections loop
	ENDIF
  51	CONTINUE !End of Solid points loop

	
	! Print all the cross section data
	DO 210 intJ = 1,CrossSectionsSize(H)
!	WRITE(*,*) "Cross Sect Index: ", intJ
	DO 220 intm = 1,6
!	WRITE(*,*) intm, ": ", CrossSectionsData(H,intJ,intm)

  220	CONTINUE
  210	CONTINUE

	END





	! For setting the maximum and minimum values of an element
	SUBROUTINE SETMAXMIN(elem,x,y,z)
	USE VAR
	INTEGER :: elem
	REAL :: x,y,z

	! The x limits
	IF (x .GT. MaxValueElem(elem,1)) THEN
	MaxValueElem(elem,1) = x
	ENDIF
	
	IF (x .LT. MinValueElem(elem,1)) THEN
	MinValueElem(elem,1) = x
	ENDIF
	
	! The y limits
	IF (y .GT. MaxValueElem(elem,2)) THEN
	MaxValueElem(elem,2) = y
	ENDIF
	
	IF (y .LT. MinValueElem(elem,2)) THEN
	MinValueElem(elem,2) = y
	ENDIF
	
	! The z limits
	IF (z .GT. MaxValueElem(elem,3)) THEN
	MaxValueElem(elem,3) = z
	ENDIF
	
	IF (z .LT. MinValueElem(elem,3)) THEN
	MinValueElem(elem,3) = z
	ENDIF



	END








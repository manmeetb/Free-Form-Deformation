
	SUBROUTINE SETFFDDATAAUTO()
	
	USE VAR
	
	!The subroutine used for attaching the FFD points automatically 
	!onto the wing
	
	INTEGER :: CompareZPoints
		
	! First cycle through all the points and find all the z cross sections
	! Set the cross section counter to 1 for the first point. Also initialize
	! the realPreviousZ value.
	intzCrossSectionCounter = 1
	realPreviousZ = SolidBoundaryPoints(1,1,3)
	realCurrentZ = 0.0
	DO 10 intI = 1,SolidBoundaryPointsSize
	
	!Also while iterating through all the points keep track of the maximum and
	! minimum Z values

	IF (intI .EQ. 1) THEN
		WingMaxZ = SolidBoundaryPoints(1,IntI,3)
		WingMinZ = SolidBoundaryPoints(1,IntI,3)
	ENDIF
	
	realCurrentZ = SolidBoundaryPoints(1, intI, 3)
	
	IF (realCurrentZ .GT. WingMaxZ) THEN
		WingMaxZ = realCurrentZ
	ENDIF	

	IF (realCurrentZ .LT. WingMinZ) THEN
		WingMinZ = realCurrentZ
	ENDIF

	intStatus = CompareZPoints(1, realPreviousZ, realCurrentZ)
	IF (intStatus .EQ. 0) THEN
		! If the status integer flag is 0 then that means that 
		! the two numbers aren't equal. So, update the previousZVariable
		! and increment the zCrossSectionCounter
		
		realPreviousZ = realCurrentZ
		intzCrossSectionCounter = intzCrossSectionCounter + 1
		
	ENDIF
					
  10	CONTINUE

	WRITE(*,*) "Number of cross sections: ", intzCrossSectionCounter		
	ZCrossSectionsSize = intzCrossSectionCounter	
		
	! Fill the ZCross Sections array with the data about all the cross sections
	! To be able to find the max and min for the x,y of each z cross section
	! first fill the first spot of the array with the data from the very first
	! cross section
	
	ALLOCATE(ZCrossSectionsData(1,ZCrossSectionsSize, 5))
	
	!Fill the first index with the data
	ZCrossSectionsData(1,1,1) = SolidBoundaryPoints(1,1,3) !z value
	
	ZCrossSectionsData(1,1,2) = SolidBoundaryPoints(1,1,1) !x max value
	ZCrossSectionsData(1,1,3) = SolidBoundaryPoints(1,1,1) !x min value		
	
	ZCrossSectionsData(1,1,4) = SolidBoundaryPoints(1,1,2) !y max value
	ZCrossSectionsData(1,1,5) = SolidBoundaryPoints(1,1,2) !y min value

	
	! Now cycle through all the points and fill the cross section data
	intZSectIndex = 1
	realPreviousZ = SolidBoundaryPoints(1,1,3)
	realCurrentZ = 0.0
	DO 20 intI = 1,SolidBoundaryPointsSize
	realCurrentZ = SolidBoundaryPoints(1, intI, 3)
	intStatus = CompareZPoints(1, realPreviousZ, realCurrentZ)
	IF (intStatus .EQ. 0) THEN
		! If the status integer flag is 0 then that means that 
		! the two numbers aren't equal. So, update the previousZVariable
		realPreviousZ = realCurrentZ

		! Create a new entry into the Z cross section array for this new
		! section's data
		
		intZSectIndex = intZSectIndex + 1
		! the z value
		ZCrossSectionsData(1, intZSectIndex, 1) = 
     . 	SolidBoundaryPoints(1,intI, 3)
		
		ZCrossSectionsData(1,intZSectIndex,2) = 
     . 	SolidBoundaryPoints(1, intI, 1) !x max value
		ZCrossSectionsData(1,intZSectIndex,3) = 
     . 	SolidBoundaryPoints(1,intI,1) !x min value		
	
		ZCrossSectionsData(1,intZSectIndex,4) = 
     . 	SolidBoundaryPoints(1,intI,2) !y max value
		ZCrossSectionsData(1,intZSectIndex,5) = 
     . 	SolidBoundaryPoints(1,intI,2) !y min value		
		
	ELSE
		! The numbers are equal so accordingly update the properties
		! of the cross section

		! check x max and min
		realXPoint = SolidBoundaryPoints(1, intI, 1)
		IF (realXPoint .GT. 
     . 	ZCrossSectionsData(1,intZSectIndex, 2)) THEN
			ZCrossSectionsData(1, intZSectIndex, 2) = realXPoint
		ENDIF		
		IF (realXPoint .LT. ZCrossSectionsData(1, intZSectIndex, 3)) THEN
			ZCrossSectionsData(1, intZSectIndex, 3) = realXPoint
		ENDIF

		! check y max and min
		realYPoint = SolidBoundaryPoints(1, intI, 2)
		IF (realYPoint .GT. ZCrossSectionsData(1, intZSectIndex, 4)) THEN
			ZCrossSectionsData(1,intZSectIndex,4) = realYPoint
		ENDIF
		
		IF (realYPoint .LT. ZCrossSectionsData(1, intZSectIndex, 5)) THEN
			ZCrossSectionsData(1,intZSectIndex,5) = realYPoint
		ENDIF


	ENDIF
					
  20	CONTINUE


	! Write the data into a file for seeing the properties of each cross section
!	OPEN (UNIT = 14, FILE = "ZCrossSectionData.txt")
!	DO 30 intI = 1, ZCrossSectionsSize
	! Write all the 4 points for the cross section
	! xmax, ymax, z
	! xmax, ymin, z
	! xmin, ymax, z
	! xmin, ymin, z
!	WRITE(14,*) ZCrossSectionsData(1,intI,2), ",",
!     .	ZCrossSectionsData(1,intI,4),",", ZCrossSectionsData(1,intI,1)
!	WRITE(14,*) ZCrossSectionsData(1,intI,2), ",",
!     . 	ZCrossSectionsData(1,intI,5),",", ZCrossSectionsData(1,intI,1)
!	WRITE(14,*) ZCrossSectionsData(1,intI,3), ",",
!     . 	ZCrossSectionsData(1,intI,4),",", ZCrossSectionsData(1,intI,1)
!	WRITE(14,*) ZCrossSectionsData(1,intI,3), ",",
!     . 	ZCrossSectionsData(1,intI,5), ",", ZCrossSectionsData(1,intI,1)


 ! 30	CONTINUE

!	CLOSE(14)
	

	! Allocate the space needed for the FFD box
	ALLOCATE(FFDPoints(1,NXFFD,NYFFD,NZFFD,3))

	
	! Make a subroutine for placing an original mesh. This mesh will have 
	! No tolerance values for the epsilon x and y. These tolerance values
	! will then be adjusted when computng the local coordinates to 
	! ensure that all points on the wing are contained in the box. Thus, in 
	! this subroutine will call attachffdnewton at each ffd box placement
	! iteration.
	
	CALL CREATEFFDMESH()
	
	END


! The subroutine used for creating the initial FFD mesh
	SUBROUTINE CREATEFFDMESH()
	USE VAR
	
	!Calculate the exact seperation of the Z Cross sections
	!By using the wings minimum and maximum z values
	WRITE(*,*) "MaxZ: ", WingMaxZ
	WRITE(*,*) "MinZ: ", WingMinZ
	
	realExactdz = (WingMaxZ - WingMinZ)/(Real(NZFFD)-1.0)
	
	FFDZMin = WingMinZ
	FFDZMax = WingMaxZ

	ALLOCATE(FFDBoxProperties(NZFFD,5))
	
		
	
	WRITE(*,*) "realExactdx: ", realExactdz	

	! Fill first the z values of each cross section
	DO 210 intl = 0, (NZFFD-1)
	realZvalue = FFDZMin + Real(intl)*realExactdz
	FFDBoxProperties(intl+1, 1) = realZValue

  210	CONTINUE
	
	!Go through each exact cross section and use the data from
	! the closest physical cross section to fill in the information
	! about the exact cross section. So for each cross section,
	! we will have to search for the physical cross section 
	! that is closest. To do this, iterate through all the cross
	! sections and find the one the index of the one that is closest
	! to the cross section

	DO 220 intl = 1, NZFFD
	!Find the z value of the exact FFD cross section of interest
	realFFDCrossSectZ = FFDBoxProperties(intl,1)
	
	WRITE(*,*) "realFFDCrossSectZ: ", realFFDCrossSectZ
	WRITE(*,*) "intl: ", intl
	

	!Now, iterate through all the cross section data. keep the 
	! absolute value of the distance between the cross sections
	! in a variable. Also, hold the index of the cross section
	! that is closest. When a cross section is found whose absolute
	! distance is smaller, update this index


	
	realZCrossMin = realFFDCrossSectZ - 0.25*(realExactdz)
	realZCrossMax = realFFDCrossSectZ + 0.25*(realExactdz)
	
		!At one end of the object (minimum z value)
	IF (intl .EQ. 1) THEN
		realZCrossMin = realFFDCrossSectZ
		realZCrossMax = realFFDCrossSectZ + 0.25*realExactdz
	ENDIF
		!At the other end of the object (max z value)
	IF (intl .EQ. NZFFD) THEN
		realZCrossMax = realFFDCrossSectZ
		realZCrossMin = realFFDCrossSectZ - 0.25*realExactdz
	ENDIF
	
	realAverageXMax = 0.0
	realAverageXMin = 0.0
	realAverageYMax = 0.0
	realAverageYMin = 0.0
	intNumValues = 0

	WRITE(*,*) " "

	!loop through all the cross sections to get the average data
	DO 351 intm = 1, ZCrossSectionsSize
	realPhysCrossZValue = ZCrossSectionsData(1,intm,1)		
	
	IF ((realPhysCrossZValue .LT. realZCrossMax) .AND. 
     . 	(realPhysCrossZValue .GT. realZCrossMin)) THEN
	
	
	IF(intl .EQ. 7) THEN
		WRITE(*,*) "- realPhysCrossZValue: ", realPhysCrossZValue
		WRITE(*,*) "- 	XMax: ", ZCrossSectionsData(1,intm,2)	
		WRITE(*,*) "- 	XMin: ", ZCrossSectionsData(1,intm,3)	
		WRITE(*,*) "- 	YMax: ", ZCrossSectionsData(1,intm,4)	
		WRITE(*,*) "- 	YMin: ", ZCrossSectionsData(1,intm,5)	
		WRITE(*,*) "- 	num: ", (intNumValues + 1)
			
	ENDIF




	!This cross section is in the average range
	realAverageXMax = realAverageXMax + ZCrossSectionsData(1,intm,2)	
	realAverageXMin = realAverageXMin + ZCrossSectionsData(1,intm,3)	
	realAverageYMax = realAverageYMax + ZCrossSectionsData(1,intm,4)	
	realAverageYMin = realAverageYMin + ZCrossSectionsData(1,intm,5)		
	intNumValues = intNumValues + 1	
	ENDIF

  351	CONTINUE

	realAverageXMax = realAverageXMax/REAL(intNumValues) 
	realAverageXMin = realAverageXMin/REAL(intNumValues)
	realAverageYMax = realAverageYMax/REAL(intNumValues)
	realAverageYMin = realAverageYMin/REAL(intNumValues)
	
	!Now, with the data from the closest physical cross section
	! fill the data for the exact cross section
	
	FFDBoxProperties(intl,2) = realAverageXMax
	FFDBoxProperties(intl,3) = realAverageXMin
 	FFDBoxProperties(intl,4) = realAverageYMax
 	FFDBoxProperties(intl,5) = realAverageYMin

  220	CONTINUE	
	
	!Create the FFD rects now for the exact z cross sections
	DO 240 intm = 1, NZFFD
		CALL CREATEFFDRECTEXACT(intm)			
  240	CONTINUE

	
	DO 241 intl = 1, NZFFD
	WRITE(*,*) "z: ", FFDBoxProperties(intl,1)
	WRITE(*,*) "xMax: ", FFDBoxProperties(intl,2)
	WRITE(*,*) "xMin: ", FFDBoxProperties(intl,3)
	WRITE(*,*) "yMax: ", FFDBoxProperties(intl,4)
	WRITE(*,*) "yMin: ", FFDBoxProperties(intl,5)
  241	CONTINUE	






!	CALL UPDATEFFDBOX(0.0,0.0,0.0,0.0,0.0,0.0)

	!Create the FFD rects now for the exact z cross sections
!	DO 250 intm = 1, NZFFD
!		CALL CREATEFFDRECTEXACT(intm)			
!  250	CONTINUE


	!The magnitude of the jump when adjusting the box
	realEpsilon = 0.025

	! Now, the algorithm must run the attachffdnewton subroutine
	! This subroutine should be changed so that it now
	! stops running if a point is found whose t,u,v values are
	! not in the range [0,1]. If this is the case, the box must be 
	! adjusted accordingly to enclose the point


	OPEN(UNIT = 32, FILE = "OptimizationHistory.txt")	

	intStepSize = 5

	!Update rest of the box now that the cross sections are placed
	! properly	
	DO WHILE(.TRUE.)	

	!REMOVE. USED ONLY FOR PLOTTING INITIAL BOX
!	EXIT
	
	intStatusIndex = -1

	realTmin = 0.0
	realTmax = 0.0
	realUmin = 0.0
	realUmax = 0.0
	realVmin = 0.0
	realVmax = 0.0



	CALL ATTACHFFDNEWTON(intStatusIndex,realTmin,realTmax, 
     . 	realUmin, realUmax, realVmin, realVmax, intStepSize)
	
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
	WRITE(32,*) "z: ", FFDBoxProperties(intl,1)
	WRITE(32,*) "xMax: ", FFDBoxProperties(intl,2)
	WRITE(32,*) "xMin: ", FFDBoxProperties(intl,3)
	WRITE(32,*) "yMax: ", FFDBoxProperties(intl,4)
	WRITE(32,*) "yMin: ", FFDBoxProperties(intl,5)
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
     . 	realYMinE, realZMaxE, realZMinE)


	!Update the FFD rects now for the exact z cross sections
	DO 280 intm = 1, NZFFD
		CALL CREATEFFDRECTEXACT(intm)			
  280	CONTINUE
	
	DO 720 intl = 1, NZFFD
	WRITE(32,*) "z: ", FFDBoxProperties(intl,1)
	WRITE(32,*) "xMax: ", FFDBoxProperties(intl,2)
	WRITE(32,*) "xMin: ", FFDBoxProperties(intl,3)
	WRITE(32,*) "yMax: ", FFDBoxProperties(intl,4)
	WRITE(32,*) "yMin: ", FFDBoxProperties(intl,5)
  720	CONTINUE	
	WRITE(32,*) " "

	
	ENDDO

	CLOSE(32)	

	!CALL IMPOSECONSTRAINTS()




	END


	SUBROUTINE IMPOSECONSTRAINTS()
	
	USE VAR

	realEpsilon = 0.025
			
	!Now, refine the box so that constrained points are removed
	
	!For the wing tip and root, z values will be changed. 
	! for the wing's leading and trailing edge, the the x 
	! values must be changed.	
	
	!The leading edge constraint
	intLeadingEdgeLabel = 1
	DO WHILE(.TRUE.)
	!Exit only if all the wing's leading edge points are removed
		
	intStatus = 0
	CALL ATTACHFFDNEWTONCONSTRAINT(intStatus,intLeadingEdgeLabel)

	!All leading edge points outside box
	IF (intStatus .EQ. 0) THEN
		EXIT
	ENDIF
	
	realXMaxE = 0.0
	realXMinE = realEpsilon	
	realYMaxE = 0.0
	realYMinE = 0.0
	realZMaxE = 0.0
	realZMinE = 0.0
	
	CALL UPDATEFFDBOX(realXMaxE, realXMinE, realYMaxE,
     . 	realYMinE,realZMaxE,realZMinE)

	!Update the FFD rects now for the exact z cross sections
	DO 490 intm = 1, NZFFD
		CALL CREATEFFDRECTEXACT(intm)			
  490	CONTINUE
	

	ENDDO

		
	!The trailing edge constraint
	intTrailingEdgeLabel = 2
	DO WHILE(.TRUE.)
	!Exit only if all the wing's leading edge points are removed
		
	intStatus = 0
	CALL ATTACHFFDNEWTONCONSTRAINT(intStatus,intTrailingEdgeLabel)

	!All leading edge points outside box
	IF (intStatus .EQ. 0) THEN
		EXIT
	ENDIF
	
	realXMaxE = (-1.0)*realEpsilon
	realXMinE = 0.0	
	realYMaxE = 0.0
	realYMinE = 0.0
	realZMaxE = 0.0
	realZMinE = 0.0
	
	CALL UPDATEFFDBOX(realXMaxE, realXMinE, realYMaxE,
     . 	realYMinE,realZMaxE,realZMinE)

	!Update the FFD rects now for the exact z cross sections
	DO 510 intm = 1, NZFFD
		CALL CREATEFFDRECTEXACT(intm)			
  510	CONTINUE
	ENDDO


	! Calculate the new t,u,v values
	intStatus = 0
	CALL ATTACHFFDNEWTONCONSTRAINT(intStatus, -1)


	END


! The subroutine for updating the FFDBoxProperties. It takes the epsilon
! values for the x,y and z limits. The epsilon values will be negative
! themselves so just add them to update the z values
	SUBROUTINE UPDATEFFDBOX(realXMaxE, realXMinE, 
     . 	realYMaxE, realYMinE, realZMaxE, realZMinE)
	
	USE VAR
	
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

	realNewZMin = FFDBoxProperties(1, 1)
	realNewZMax = FFDBoxProperties(NZFFD, 1)
	
	realNewZMin = realNewZMin + realZMinE
	realNewZMax = realNewZMax + realZMaxE
		
	
	realExactdz = (realNewZMax - realNewZMin)/(Real(NZFFD)-1.0)
	
		! Fill first the z values of each cross section
	DO 310 intl = 0, (NZFFD-1)
	realZvalue = realNewZMin + Real(intl)*realExactdz
	FFDBoxProperties(intl+1, 1) = realZValue
  310	CONTINUE

	DO 340 intl = 1, NZFFD
		!Find the z value of the exact FFD cross section of interest
	realFFDCrossSectZ = FFDBoxProperties(intl,1)

	!Take the average of the data for the cross sections that are 
	! around the cross section. Let the distance between each cross section
	! be dz. Then, look at the cross sections and take the average of the data
	! for the cross sections that are at +- dz/2 from the cross section
	! we are working with. So, find all cross section data between z-dz/2 
	! and z+dz/2 and take the average. For points at the ends of the object,
	! take the average of the data for dz in the direction of the object's 
	! body.

	
	realZCrossMin =  realFFDCrossSectZ - 0.25*(realExactdz)
	realZCrossMax = realFFDCrossSectZ + 0.25*(realExactdz)
	
		!At one end of the object (minimum z value)
	IF (intl .EQ. 1) THEN
		realZCrossMin = realFFDCrossSectZ
		realZCrossMax = realFFDCrossSectZ + 0.25*realExactdz
	ENDIF
		!At the other end of the object (max z value)
	IF (intl .EQ. NZFFD) THEN
		realZCrossMax = realFFDCrossSectZ
		realZCrossMin = realFFDCrossSectZ - 0.25*realExactdz
	ENDIF
	
	realAverageXMax = 0.0
	realAverageXMin = 0.0
	realAverageYMax = 0.0
	realAverageYMin = 0.0
	intNumValues = 0

	!loop through all the cross sections to get the average data
	DO 351 intm = 1, ZCrossSectionsSize
	realPhysCrossZValue = ZCrossSectionsData(1,intm,1)		
	
	IF ((realPhysCrossZValue .LT. realZCrossMax) .AND. 
     . 	(realPhysCrossZValue .GT. realZCrossMin)) THEN
	!This cross section is in the average range
	realAverageXMax = realAverageXMax + ZCrossSectionsData(1,intm,2)	
	realAverageXMin = realAverageXMin + ZCrossSectionsData(1,intm,3)	
	realAverageYMax = realAverageYMax + ZCrossSectionsData(1,intm,4)	
	realAverageYMin = realAverageYMin + ZCrossSectionsData(1,intm,5)		
	intNumValues = intNumValues + 1	
	ENDIF

  351	CONTINUE

	realAverageXMax = realAverageXMax/REAL(intNumValues) 
	realAverageXMin = realAverageXMin/REAL(intNumValues)
	realAverageYMax = realAverageYMax/REAL(intNumValues)
	realAverageYMin = realAverageYMin/REAL(intNumValues)
	
	!Now, with the data from the closest physical cross section
	! fill the data for the exact cross section
	
	FFDBoxProperties(intl,2) = realAverageXMax
	FFDBoxProperties(intl,3) = realAverageXMin
 	FFDBoxProperties(intl,4) = realAverageYMax
 	FFDBoxProperties(intl,5) = realAverageYMin

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
	FFDBoxProperties(intl,2) = FFDBoxProperties(intl,2) + realXMaxE
	FFDBoxProperties(intl,3) = FFDBoxProperties(intl,3) + realXMinE
	FFDBoxProperties(intl,4) = FFDBoxProperties(intl,4) + realYMaxE
	FFDBoxProperties(intl,5) = FFDBoxProperties(intl,5) + realYMinE
  320	CONTINUE	


	END



! The subroutine used for creating the rectangular at the given
! z cross section. It uses  the data from the FFDBoxProperties 
! to find how big to make the cross section 
	SUBROUTINE CREATEFFDRECTEXACT(intZCrossIndex)
	
	USE VAR
	
	INTEGER :: intZCrossIndex

	realXMax = FFDBoxProperties(intZCrossIndex,2)
	realXMin = FFDBoxProperties(intZCrossIndex,3)	
	realYMax = FFDBoxProperties(intZCrossIndex,4)
	realYMin = FFDBoxProperties(intZCrossIndex,5)
	
	realFFDdx = (realXMax - realXMin)/(NXFFD-1)
	realFFDdy = (realYMax - realYMin)/(NYFFD-1)
	realZValue = FFDBoxProperties(intZCrossIndex,1)	
	intK = intZCrossIndex



	DO 60 intI=1,NXFFD
	DO 60 intJ=1,NYFFD
		realX = realXMin + realFFDdx*(intI-1)
		realY = realYMin + realFFDdy*(intJ-1)
		FFDPoints(1,intI,intJ,intK,1) = realX
		FFDPoints(1,intI,intJ,intK,2) = realY
		FFDPoints(1,intI,intJ,intK,3) = realZValue
  60	CONTINUE	
	
	END





! The subroutine used for creating the rectangular at the given
! z cross section. 
	SUBROUTINE CREATEFFDRECT(intInput, intK)
	
	USE VAR
	
	INTEGER :: intInput, intK
!	realCONSTxEpsilon = 0.025
	realCONSTxEpsilon = -0.070	
	realCONSTyEpsilon = 0.025


	realXMax = ZCrossSectionsData(1,intInput,2)
	realXMin = ZCrossSectionsData(1,intInput,3)
	realYMax = ZCrossSectionsData(1,intInput,4)
	realYMin = ZCrossSectionsData(1,intInput,5)
	
	realFFDdx = (realXMax +2*realCONSTxEpsilon  - realXMin)/(NXFFD-1)
	realFFDdy = (realYMax+2*realCONSTyEpsilon - realYMin)/(NYFFD-1)

	realZValue = ZCrossSectionsData(1,intInput,1)
	
	! The k =0/1 index is for the cross section near 0
	IF (intK .EQ. NZFFD) THEN
		realZValue = realZValue + 0.025
	ENDIF

	IF (intK .EQ. 1) THEN
		realZValue = - 0.1
	ENDIF

!	WRITE(*,*) "intK: ", intK
!	WRITE(*,*) "realZValue: ", realZValue


	DO 60 intI=1,NXFFD
	DO 60 intJ=1,NYFFD
		realX = realXMin-realCONSTxEpsilon + realFFDdx*(intI-1)
		realY = realYMin-realCONSTyEpsilon + realFFDdy*(intJ-1)
		FFDPoints(1,intI,intJ,intK,1) = realX
		FFDPoints(1,intI,intJ,intK,2) = realY
		FFDPoints(1,intI,intJ,intK,3) = realZValue
  60	CONTINUE	
	
	END

! The function that is used for comapring two z points. It will check to see
! whether the points, to their second decimal place, are identical.
!Have the function return 1 if the numbers are identical to the second
!decimal place and 0 otherwise
	INTEGER FUNCTION CompareZPoints(intNumDecimal, realZ1, realZ2)
	intMultiplier = 10**intNumDecimal
	intZ1 = int(realZ1*intMultiplier)
	intZ2 = int(realZ2*intMultiplier)
	
!	WRITE(*,*) "intZ1_3digit: ", intZ1_3decimal
!	WRITE(*,*) "intZ2_3digit: ", intZ2_3decimal
	intStatus = 0
	IF (intZ1 .EQ. intZ2) THEN
		intStatus = 1
	ENDIF
	CompareZPoints = intStatus
	END






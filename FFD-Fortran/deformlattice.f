	SUBROUTINE DEFORMLATTICE()
	USE VAR
	

	DO 30 intH = 1,NumElements
	! The subroutine for deforming the lattice. This
	! subroutine will move the lattice points and print
	! them into a new file called deformedLattice.txt

	DO 10 intI = 1,NXFFD(intH)
	DO 10 intJ = 1,NYFFD(intH)
	DO 10 intK = 1,NZFFD(intH)

	!Deform the faring points down by 100
	IF(intH .EQ. 1) THEN
	IF (intJ .NE. NYFFD(intH)) THEN
	IF (intI .NE. 1) THEN
	IF (intI .NE. NXFFD(intH)) THEN
	realNewY = FFDPoints(intH,intI,intJ,intK,2)
	realNewY = realNewY - 100
	FFDPoints(intH,intI,intJ,intK,2) = realNewY

	! If this point is connected to another one
	IF (FFDPoints(intH,intI,intJ,intK,4) .NE. -1) THEN
	intElem2 = FFDPoints(intH,intI,intJ,intK,4)
	intI2 = FFDPoints(intH,intI,intJ,intK,5)
	intJ2 = FFDPoints(intH,intI,intJ,intK,6)
	intK2 = FFDPoints(intH,intI,intJ,intK,7)

	realNewY = FFDPoints(intElem2,intI2,intJ2,intK2,2)
	realNewY = realNewY - 100
	FFDPoints(intElem2,intI2,intJ2,intK2,2) = realNewY	
	ENDIF

	ENDIF
	ENDIF
	ENDIF
	ENDIF

	! Deform the wing points too by 100
!	IF(intH .EQ. 2) THEN
!	IF (intK .EQ. 1) THEN
!	realNewZ = FFDPoints(intH,intI,intJ,intK,3)
!	realNewZ = realNewZ + 100
!	FFDPoints(intH,intI,intJ,intK,3) = realNewZ
!	ENDIF
!	ENDIF



	! Deform the fuselage now by increasing the z value by 
	! 10% of the fuselage length
!	IF (intH .EQ. 2) THEN
!	realDeltaZ = MaxZ(intH,1) - MinZ(intH,1)
	
	
!	realdz = 0.2*realDeltaZ

!	IF (intK .GE. (NZFFD-2)) THEN
!	realZValue = FFDPoints(intH,intI,intJ,intK,3)
!	realNewZ = realZValue + 250

!	FFDPoints(intH,intI,intJ,intK,3) = realNewZ
!
!	ENDIF



!	ENDIF 


!		IF (intJ .EQ. 1) THEN
!		IF ((intI .GE. 3) .AND. (intI .LE. (NXFFD-2))) THEN
!			realYValue = FFDPoints(intH,intI,intJ,intK,2)
!			realYValue = realYValue + 0.3
!			FFDPoints(intH,intI,intJ,intK,2) = realYValue
!		ENDIF 
!		ENDIF

!		IF (intJ .EQ. NYFFD) THEN
!		IF ((intI .GE. 3) .AND. (intI .LE. (NXFFD-2))) THEN
!			realYValue = FFDPoints(1,intI,intJ,intK,2)
!			realYValue = realYValue + 0.1
!			FFDPoints(1,intI,intJ,intK,2) = realYValue
!		ENDIF 
!		ENDIF

		! Shift only the last row of ffd points up by 0.3	
!		IF (intK .EQ. NZFFD) THEN
!			realYValue = FFDPoints(intH,intI,intJ,intK,2)
!			realYValue = realYValue + 0.3
!			FFDPoints(intH,intI,intJ,intK,2) = realYValue
!		ENDIF

		! Shift only the last row of ffd points up by 0.3
!		IF ((intK .NE. NZFFD) .AND. (intK .NE. 1)) THEN
!		IF (intI .EQ. NXFFD) THEN
!			realYValue = FFDPoints(1,intI,intJ,intK,2)
!			realYValue = realYValue - 0.25
!			FFDPoints(1,intI,intJ,intK,2) = realYValue
!		ENDIF
!		ENDIF


!Move all the interior FFD points up a distance of 0.3
!		IF((intI .NE. 1) .AND. (intI .NE. NXFFD)) THEN
!		IF((intI .NE. 2) .AND. (intI .NE. (NXFFD-1))) THEN
!		IF((intK .NE. 1) .AND. (intK .NE. NZFFD)) THEN
!			realYValue = FFDPoints(1,intI,intJ,intK,2)
!			realYValue = realYValue + 0.05
!			FFDPoints(1,intI,intJ,intK,2) = realYValue
!		ENDIF
!		ENDIF
!		ENDIF




	!SWEEP
! Make the sweep deformation on the FFD wing (move all the points
! back using a linear perturbation

!	realTheta = 0.261799 !pi/12 angle
!	realx = FFDPoints(intH, intI,intJ,intK,1)
!	realy = FFDPoints(intH, intI,intJ,intK,2)
!	realz = FFDPoints(intH, intI,intJ,intK,3)

	! If the plane of interest is not the wing root, then only deform
!	IF (intK .NE. 1) THEN
!	realdz = realz - FFDPoints(intH,intI,intJ,0,3)	!get the distance to the first plane
!	realdx = TAN(realTheta) * realdz
!	realNewx = realx + realdx
	
	! Set the new points for the ffd
!	FFDPoints(intH,intI,intJ,intK,1) = realNewx
		! don't change y
		! don't change z 

!	ENDIF	

	!DIHEDRAL
! Make the sweep deformation on the FFD wing (move all the points
! back using a linear perturbation

!	realTheta = 0.6666*0.261799 !pi/24 angle
!	realx = FFDPoints(intH, intI,intJ,intK,1)
!	realy = FFDPoints(intH, intI,intJ,intK,2)
!	realz = FFDPoints(intH, intI,intJ,intK,3)

	! If the plane of interest is not the wing root, then only deform
!	IF (intK .NE. 1) THEN
!	realdz = realz - FFDPoints(intH,intI,intJ,0,3)	!get the distance to the first plane
!	realdy = TAN(realTheta) * realdz
!	realNewy = realy + realdy
	
	! Set the new points for the ffd:
	! don't change x
!	FFDPoints(intH,intI,intJ,intK,2) = realNewy
	! don't change z 	
!	ENDIF

	! GEOMETRIC TWIST
! Keep the k=1 and k=2 set of FFD points fixed. Gradually increase
! the rotation angle starting from the 3rd set of points. Rotate the 
! points using the following rotation matrix:
! T = | cos(a) -sin(a)| * (x,y) = (x', y')
!     | sin(a)	cos(a)|

! The axis of rotation
!	realXAxis = 2.0
!	realYAxis = 0.0
	
!	IF (intK .GT. 2) THEN

!	realTheta = 0.50 * (intK - 2)/(NZFFD-2)

	! The point of interest
!	realx = FFDPoints(intH, intI,intJ,intK,1)
!	realy = FFDPoints(intH, intI,intJ,intK,2)
!	realz = FFDPoints(intH, intI,intJ,intK,3)

	! find the vector from the axis to the point
!	realdx = (realx - realXAxis)
!	realdy = (realy - realYAxis)

	! apply the rotation to the point
!	realdxPrime = COS(realTheta)*realdx - 
!     . 	SIN(realTheta)*realdy
!	realdyPrime = SIN(realTheta)*realdx + 
!     . 	COS(realTheta)*realdy
	
!	realNewx = realdxPrime + realXAxis
!	realNewy = realdyPrime + realYAxis

	! Set the new values of the FFD points
!	FFDPoints(intH, intI,intJ,intK,1) = realNewx
!	FFDPoints(intH, intI,intJ,intK,2) = realNewy
!	ENDIF

	!WINGLET
! Create the winglet on the wing. To do this, move the last cross section of 
! FFD points up and perhaps move the second last cross section down by a small 
! amount

	
	! Shift only the last row of ffd points up by 0.3	
!	IF (intK .EQ. NZFFD) THEN
!		realYValue = FFDPoints(intH,intI,intJ,intK,2)
!		realYValue = realYValue + 0.3
!		FFDPoints(intH,intI,intJ,intK,2) = realYValue
!	ENDIF
!
!	! Shift only the second last row of ffd points down by 0.3	
!	IF (intK .EQ. (NZFFD-1)) THEN
!		realYValue = FFDPoints(intH,intI,intJ,intK,2)
!		realYValue = realYValue - 0.15
!		FFDPoints(intH,intI,intJ,intK,2) = realYValue
!	ENDIF


	
	


	
  10	CONTINUE
  30	CONTINUE

	END















	! The subroutine used for filling the FFDBoxProperties data for an element.
	! It also takes as a parameter the index of the element in order to know 
	! which element's box is being set
	
	SUBROUTINE FILLFFDDATAAXIS(H)

	USE VAR

	! The element's index
	INTEGER :: H

	IF (AxisDirection(H) .EQ. 1) THEN
	DO 20 intm = 1,NXFFD(H)
	CALL CREATEFFDPLANE(H,intm,AxisDirection(H))
  20	CONTINUE
	ENDIF
	
	IF (AxisDirection(H) .EQ. 2) THEN
	DO 30 intm = 1,NYFFD(H)
	CALL CREATEFFDPLANE(H,intm,AxisDirection(H))
  30	CONTINUE
	ENDIF

	! If we are building along the z axis
	IF(AxisDirection(H) .EQ. 3) THEN	
	DO 10 intm = 1,NZFFD(H)
	CALL CREATEFFDPLANE(H, intm, AxisDirection(H))
  10	CONTINUE
	ENDIF

	END


	! The subroutine that is used for creating the the plane of FFD
	! points for the given element. It will take the index of the 
	! plane of interest, and the axis direction and will then place
	! the points on the plane of interest.

	SUBROUTINE CREATEFFDPLANE(Elem, CrossIndex, axis)
	USE VAR

	INTEGER :: Elem, CrossIndex, axis

	! Going along the x axis
	IF (axis .EQ. 1) THEN
	realZMax = FFDVolProperties(Elem,CrossIndex,6)
        realZMin = FFDVolProperties(Elem,CrossIndex,7)
        realYMax = FFDVolProperties(Elem,CrossIndex,4)
        realYMin = FFDVolProperties(Elem,CrossIndex,5)

        realFFDdz = (realZMax - realZMin)/(NZFFD(Elem)-1)
        realFFDdy = (realYMax - realYMin)/(NYFFD(Elem)-1)
        realX = FFDVolProperties(Elem,CrossIndex,1)
        intI = CrossIndex

	DO 70 intK=1,NZFFD(Elem)
        DO 70 intJ=1,NYFFD(Elem)
                realZ = realZMin + realFFDdz*(intK-1)
                realY = realYMin + realFFDdy*(intJ-1)
                FFDPoints(Elem,intI,intJ,intK,1) = realX
                FFDPoints(Elem,intI,intJ,intK,2) = realY
                FFDPoints(Elem,intI,intJ,intK,3) = realZ
  70    CONTINUE
	
	ENDIF


	! Going along the y axis
	IF (axis .EQ. 2) THEN
	realZMax = FFDVolProperties(Elem,CrossIndex,6)
        realZMin = FFDVolProperties(Elem,CrossIndex,7)
        realXMax = FFDVolProperties(Elem,CrossIndex,2)
        realXMin = FFDVolProperties(Elem,CrossIndex,3)

        realFFDdz = (realZMax - realZMin)/(NZFFD(Elem)-1)
        realFFDdx = (realXMax - realXMin)/(NXFFD(Elem)-1)
        realY = FFDVolProperties(Elem,CrossIndex,1)
        intJ = CrossIndex

	DO 80 intK=1,NZFFD(Elem)
        DO 80 intI=1,NXFFD(Elem)
                realZ = realZMin + realFFDdz*(intK-1)
                realX = realXMin + realFFDdx*(intI-1)
                FFDPoints(Elem,intI,intJ,intK,1) = realX
                FFDPoints(Elem,intI,intJ,intK,2) = realY
                FFDPoints(Elem,intI,intJ,intK,3) = realZ
  80    CONTINUE
	
	ENDIF



	IF (axis .EQ. 3) THEN
	realXMax = FFDVolProperties(Elem,CrossIndex,2)
        realXMin = FFDVolProperties(Elem,CrossIndex,3)
        realYMax = FFDVolProperties(Elem,CrossIndex,4)
        realYMin = FFDVolProperties(Elem,CrossIndex,5)

        realFFDdx = (realXMax - realXMin)/(NXFFD(Elem)-1)
        realFFDdy = (realYMax - realYMin)/(NYFFD(Elem)-1)
        realZ = FFDVolProperties(Elem,CrossIndex,1)
        intK = CrossIndex

	DO 60 intI=1,NXFFD(Elem)
        DO 60 intJ=1,NYFFD(Elem)
                realX = realXMin + realFFDdx*(intI-1)
                realY = realYMin + realFFDdy*(intJ-1)
                FFDPoints(Elem,intI,intJ,intK,1) = realX
                FFDPoints(Elem,intI,intJ,intK,2) = realY
                FFDPoints(Elem,intI,intJ,intK,3) = realZ
  60    CONTINUE
	
	ENDIF

	END




! The subroutine used for creating the rectangular at the given
! z cross section. It uses  the data from the FFDBoxProperties 
! to find how big to make the cross section. It also takes as a 
! parameter the index of the element in order to know which element's
! box is being set
	SUBROUTINE CREATEFFDRECTEXACT(intElem, intZCrossIndex)
	
	USE VAR
	
	INTEGER :: intZCrossIndex, intElem

	realXMax = FFDBoxProperties(intElem,intZCrossIndex,2)
	realXMin = FFDBoxProperties(intElem,intZCrossIndex,3)	
	realYMax = FFDBoxProperties(intElem,intZCrossIndex,4)
	realYMin = FFDBoxProperties(intElem,intZCrossIndex,5)
	
	realFFDdx = (realXMax - realXMin)/(NXFFD-1)
	realFFDdy = (realYMax - realYMin)/(NYFFD-1)
	realZValue = FFDBoxProperties(intElem,intZCrossIndex,1)	
	intK = intZCrossIndex

	DO 60 intI=1,NXFFD
	DO 60 intJ=1,NYFFD
		realX = realXMin + realFFDdx*(intI-1)
		realY = realYMin + realFFDdy*(intJ-1)
		FFDPoints(intElem,intI,intJ,intK,1) = realX
		FFDPoints(intElem,intI,intJ,intK,2) = realY
		FFDPoints(intElem,intI,intJ,intK,3) = realZValue
  60	CONTINUE	
	
	END





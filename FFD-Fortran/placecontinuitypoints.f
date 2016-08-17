
! This subroutine is for placing the FFD points that
! have been placed already by previous elements that
! this element has a continuity relationship with. All
! FFD points will also have a variables 4-7 be the information
! that tells us what other point this one must coincide with.
! If the values are -1, then there are coincident relationships for
! the point. Otherwise, 4 = other element index, 5-7 = I,J,K value
! of other point

! Also, note that for element's that are ranked higher (have their points 
! placed earlier), the cotinuity info isn't placed now. It will be placed
! when the next element (the one whose points must be placed while one set
! of points are fixed) is being worked on
	SUBROUTINE PLACECONTINUITYPOINTS(H, R)
	USE VAR
	INTEGER :: H,R

	intH = H
	intR = R
	
	! First, intialize all the FFD data to be -1
	DO 10 intI = 1, NXFFD(intH)
	DO 10 intJ = 1, NYFFD(intH)
	DO 10 intK = 1, NZFFD(intH)
	DO 10 intL = 1,7
	FFDPoints(intH,intI,intJ,intK,intL) = -1 
  10	CONTINUE

	!Check the connectivity information for the element
		! loop over the faces
	DO 20 intF = 1,6
	IF(ConnectivityInfo(intH,intF,1) .NE. -1) THEN
	! We have a face with a connection to another face
	intElem2 = ConnectivityInfo(intH,intF,1)
	intFace2 = ConnectivityInfo(intH,intF,2)

	WRITE(*,*) "elem 2: ", intElem2
	WRITE(*,*) "face 2: ", intFace2
	WRITE(*,*) "elem 1: ", intH
	WRITE(*,*) "face 1: ", intF
	! If this element (intElem2) has already been worked on
	! then only place the continuity lattice points. To check
	! this, see the earlier ranks and whether the element has 
	! been processed
	
	DO 30 intM = 1,NumElements
	! Checking all the elements that came before this one
	IF(intM .LT. intR) THEN
	IF(Rank(intM) .EQ. intElem2) THEN
	! The element was processed before so place
	! the points.
	
	CALL PLACEPOINTPLANE(intH, intF, intElem2,intFace2)	
	
	ENDIF
	ENDIF
  30	CONTINUE	
	
		
	ENDIF	
  20	CONTINUE
	END




	! The subroutine that is used for placing the plane of 
	! points from element 2 and its face 2 onto element 1 and
	! its face 1
	SUBROUTINE PLACEPOINTPLANE(E1,F1,E2,F2)
	USE VAR
	
	INTEGER :: E1,F1,E2,F2

	! Place the points for the X Plus face
	IF (F1 .EQ. 1) THEN
		! The other element's X Minus face is touching
	IF (F2 .EQ. 2) THEN

	I1 = NXFFD(E1)
	I2 = 1
	DO 11 intK = 1,NZFFD(E1)
	DO 11 intJ = 1,NYFFD(E1)

	! Set the x,y,z values of the point to be the same
	FFDPoints(E1,I1,intJ,intK,1) = FFDPoints(E2,I2,intJ,intK,1)
	FFDPoints(E1,I1,intJ,intK,2) = FFDPoints(E2,I2,intJ,intK,2)
	FFDPoints(E1,I1,intJ,intK,3) = FFDPoints(E2,I2,intJ,intK,3)	

		! The element index of the neighboring element
	FFDPoints(E1,I1,intJ,intK,4) = E2
		! The I,J,K value of the neighboring element's point
	FFDPoints(E1,I1,intJ,intK,5) = I2
	FFDPoints(E1,I1,intJ,intK,6) = intJ
	FFDPoints(E1,I1,intJ,intK,7) = intK	

	! In addition, set the continuity information for the element
	! that was already made
	FFDPoints(E2,I2,intJ,intK,4) = E1

	FFDPoints(E2,I2,intJ,intK,5) = I1
	FFDPoints(E2,I2,intJ,intK,6) = intJ
	FFDPoints(E2,I2,intJ,intK,7) = intK	
  11	CONTINUE 
	ENDIF
	ENDIF


	! Place the points for the X Minus face
	IF (F1 .EQ. 2) THEN
		! The other element's X Plus face is touching
	IF (F2 .EQ. 1) THEN

	I1 = 1
	I2 = NXFFD(E2)
	DO 12 intK = 1,NZFFD(E1)
	DO 12 intJ = 1,NYFFD(E1)

	! Set the x,y,z values of the point to be the same
	FFDPoints(E1,I1,intJ,intK,1) = FFDPoints(E2,I2,intJ,intK,1)
	FFDPoints(E1,I1,intJ,intK,2) = FFDPoints(E2,I2,intJ,intK,2)
	FFDPoints(E1,I1,intJ,intK,3) = FFDPoints(E2,I2,intJ,intK,3)	

		! The element index of the neighboring element
	FFDPoints(E1,I1,intJ,intK,4) = E2
		! The I,J,K value of the neighboring element's point
	FFDPoints(E1,I1,intJ,intK,5) = I2
	FFDPoints(E1,I1,intJ,intK,6) = intJ
	FFDPoints(E1,I1,intJ,intK,7) = intK	

	! In addition, set the continuity information for the element
	! that was already made
	FFDPoints(E2,I2,intJ,intK,4) = E1

	FFDPoints(E2,I2,intJ,intK,5) = I1
	FFDPoints(E2,I2,intJ,intK,6) = intJ
	FFDPoints(E2,I2,intJ,intK,7) = intK	
  12	CONTINUE 
	ENDIF
	ENDIF


	! Place the points for the Y Plus face
	IF (F1 .EQ. 3) THEN
		! The other element's Y Minus face is touching
	IF (F2 .EQ. 4) THEN

	J1 = NYFFD(E1)
	J2 = 1
	DO 13 intK = 1,NZFFD(E1)
	DO 13 intI = 1,NXFFD(E1)

	! Set the x,y,z values of the point to be the same
	FFDPoints(E1,intI,J1,intK,1) = FFDPoints(E2,intI,J2,intK,1)
	FFDPoints(E1,intI,J1,intK,2) = FFDPoints(E2,intI,J2,intK,2)
	FFDPoints(E1,intI,J1,intK,3) = FFDPoints(E2,intI,J2,intK,3)	

		! The element index of the neighboring element
	FFDPoints(E1,intI,J1,intK,4) = E2
		! The I,J,K value of the neighboring element's point
	FFDPoints(E1,intI,J1,intK,5) = intI
	FFDPoints(E1,intI,J1,intK,6) = J2
	FFDPoints(E1,intI,J1,intK,7) = intK	

	! In addition, set the continuity information for the element
	! that was already made
	FFDPoints(E2,intI,J2,intK,4) = E1

	FFDPoints(E2,intI,J2,intK,5) = intI
	FFDPoints(E2,intI,J2,intK,6) = J1
	FFDPoints(E2,intI,J2,intK,7) = intK	
  13	CONTINUE 
	ENDIF
	ENDIF



	! Place the points for the Y Minus face
	IF (F1 .EQ. 4) THEN
		! The other element's Y Plus face is touching
	IF (F2 .EQ. 3) THEN

	J1 = 1
	J2 = NYFFD(E2)
	DO 14 intK = 1,NZFFD(E1)
	DO 14 intI = 1,NXFFD(E1)

	! Set the x,y,z values of the point to be the same
	FFDPoints(E1,intI,J1,intK,1) = FFDPoints(E2,intI,J2,intK,1)
	FFDPoints(E1,intI,J1,intK,2) = FFDPoints(E2,intI,J2,intK,2)
	FFDPoints(E1,intI,J1,intK,3) = FFDPoints(E2,intI,J2,intK,3)	

		! The element index of the neighboring element
	FFDPoints(E1,intI,J1,intK,4) = E2
		! The I,J,K value of the neighboring element's point
	FFDPoints(E1,intI,J1,intK,5) = intI
	FFDPoints(E1,intI,J1,intK,6) = J2
	FFDPoints(E1,intI,J1,intK,7) = intK	

	! In addition, set the continuity information for the element
	! that was already made
	FFDPoints(E2,intI,J2,intK,4) = E1

	FFDPoints(E2,intI,J2,intK,5) = intI
	FFDPoints(E2,intI,J2,intK,6) = J1
	FFDPoints(E2,intI,J2,intK,7) = intK	
  14	CONTINUE 
	ENDIF
	ENDIF


	! Place the points for the Z Plus face
	IF (F1 .EQ. 5) THEN
		! The other element's Z Minus face is touching
	IF (F2 .EQ. 6) THEN
	! The K = min(1) plane of element 1 (wing)  is touching the
	! K = max plane of element 2 (faring). The i,j values of the 
	! touching points are the same still though
	K1 = NZFFD(E1)
	K2 = 1
	DO 15 intI = 1,NXFFD(E1)
	DO 15 intJ = 1,NYFFD(E1)
	! The nxffd and nyffd (points along the xy plane)
	! numbers must be the same for both elements. Add a check later
	! to verify this.
	
	! Set the x,y,z values of the point to be the same
	FFDPoints(E1,intI,intJ,K1,1) = FFDPoints(E2,intI,intJ,K2,1)
	FFDPoints(E1,intI,intJ,K1,2) = FFDPoints(E2,intI,intJ,K2,2)
	FFDPoints(E1,intI,intJ,K1,3) = FFDPoints(E2,intI,intJ,K2,3)	

		! The element index of the neighboring element
	FFDPoints(E1,intI,intJ,K1,4) = E2
		! The I,J,K value of the neighboring element's point
	FFDPoints(E1,intI,intJ,K1,5) = intI
	FFDPoints(E1,intI,intJ,K1,6) = intJ
	FFDPoints(E1,intI,intJ,K1,7) = K2	

	! In addition, set the continuity information for the element
	! that was already made
	FFDPoints(E2,intI,intJ,K2,4) = E1

	FFDPoints(E2,intI,intJ,K2,5) = intI
	FFDPoints(E2,intI,intJ,K2,6) = intJ
	FFDPoints(E2,intI,intJ,K2,7) = K1	
  15	CONTINUE 
	ENDIF
	ENDIF


	! Place the points for the Z Minus face
	IF (F1 .EQ. 6) THEN
		! The other element's Z Plus face is touching
	IF (F2 .EQ. 5) THEN
	! The K = min(1) plane of element 1 (wing)  is touching the
	! K = max plane of element 2 (faring). The i,j values of the 
	! touching points are the same still though
	K1 = 1
	K2 = NZFFD(E2)
	DO 16 intI = 1,NXFFD(E1)
	DO 16 intJ = 1,NYFFD(E1)
	! The nxffd and nyffd (points along the xy plane)
	! numbers must be the same for both elements. Add a check later
	! to verify this.
	
	! Set the x,y,z values of the point to be the same
	FFDPoints(E1,intI,intJ,K1,1) = FFDPoints(E2,intI,intJ,K2,1)
	FFDPoints(E1,intI,intJ,K1,2) = FFDPoints(E2,intI,intJ,K2,2)
	FFDPoints(E1,intI,intJ,K1,3) = FFDPoints(E2,intI,intJ,K2,3)	

		! The element index of the neighboring element
	FFDPoints(E1,intI,intJ,K1,4) = E2
		! The I,J,K value of the neighboring element's point
	FFDPoints(E1,intI,intJ,K1,5) = intI
	FFDPoints(E1,intI,intJ,K1,6) = intJ
	FFDPoints(E1,intI,intJ,K1,7) = K2	

	! In addition, set the continuity information for the element
	! that was already made
	FFDPoints(E2,intI,intJ,K2,4) = E1

	FFDPoints(E2,intI,intJ,K2,5) = intI
	FFDPoints(E2,intI,intJ,K2,6) = intJ
	FFDPoints(E2,intI,intJ,K2,7) = K1	
  16	CONTINUE 
	ENDIF
	ENDIF
	
	END


! Points are always placed with i,j and k increasing the same 
! direction as the coordinate axes. Now, it is also important to note
! that the only faces that can join are for faces perpindicular to the 
! same coordinate axes. So, even though the faring has points being placed
! along the x axis, its Z + face is coincident with the Z - 
! plane for the wing (which has its points placed in the z direction).  







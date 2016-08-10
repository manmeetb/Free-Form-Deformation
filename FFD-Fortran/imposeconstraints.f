
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






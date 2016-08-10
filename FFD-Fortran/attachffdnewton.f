! The subroutine that will be used for finding the initial T,U,V
! values of the the solid boundary points. The intT, intU and
! int V values are for specifcying when to stop the iterations.
! that is, if intT is set to 1, then that means that if a t value
! is out of range, then stop the algorithm. However, if it is set to
! 0, then in that case the code will not stop if a t value is out of
! range. Also, intH is the element index
	
	SUBROUTINE ATTACHFFDNEWTON(intStatusIndex, realTmin,realTmax, 
     . 	realUmin, realUmax, realVmin, realVmax, intStepSize, intH)
	
	USE VAR

	
	INTEGER :: intStatusIndex, intStepSize, intH
	REAL :: realTmin, realTmax, realUmin, realUmax,
     .  realVmin, realVmax
	
	intStatusIndex = -1
	
	

	!to make execution faster, check every 4 points of object
	DO 100 iIndex = 1,NumSolidBoundaryPoints(intH,1),intStepSize

!	iIndex = 29038
	
!	iIndex = 21411
		

	WRITE(*,*) "Index: ", iIndex
	!Get the x,y,z values of the solid boundary point in question
	realX11 = SolidBoundaryPoints(intH,iIndex,1)
	realY11 = SolidBoundaryPoints(intH,iIndex,2)
	realZ11 = SolidBoundaryPoints(intH,iIndex,3)	

!	WRITE(*,*) "x: ", realX11
!	WRITE(*,*) "y: ", realY11
!	WRITE(*,*) "z: ", realZ11
	

	realAns1 = 0.0
	realAns2 = 0.0
	realAns3 = 0.0
	
	intStatus = 1

	CALL NEWTONSOLVE(0.5,0.5,0.5,realX11,realY11,realZ11,
     .	realAns1,realAns2,realAns3, intStatus, intH)

	! Newton Raphson Failure using 0.5 so try numbers
	! starting from 0.05 and increment by 0.05 each time
	! for the initial guess
	IF (intStatus .NE. 1) THEN
	! we are going to range our guesses from 0.05
	! to 0.95 with increments of 0.05
	DO 25 intk=0,18
	realGuess = 0.05 + REAL(intk)*0.05
	CALL NEWTONSOLVE(realGuess,realGuess,realGuess,realX11,
     . 	realY11,realZ11, realAns1,realAns2,realAns3, intStatus, 
     . 	intH)
	
	IF (intStatus .EQ. 1) THEN
	EXIT
	ENDIF
		
  25	CONTINUE
	
	! Tried varying initial conditions to no success
	IF (intStatus .NE. 1) THEN
		WRITE(*,*) "failed Newton Raphson"
		STOP
	ENDIF
	ENDIF	


	!Fill the T,U,V information into the solid boundary point array
	SolidBoundaryPoints(intH,iIndex,4) = realAns1 !T
	SolidBoundaryPoints(intH,iIndex,5) = realAns2 !U
	SolidBoundaryPoints(intH,iIndex,6) = realAns3 !V

!	WRITE(*,*) "T: ", realAns1
!	WRITE(*,*) "U: ", realAns2
!	WRITE(*,*) "V: ", realAns3

	! setup the T,U,V min/max values using what is at the first index
	IF (iIndex .EQ. 1) THEN

	realTmin = realAns1
	realTmax = realAns1
	realUmin = realAns2
	realUmax = realAns2
	realVmin = realAns3
	realVmax = realAns3

	ENDIF

	! update the T,U,V min/max values based on the newly computed
	! values

	IF (realAns1 .GT. realTmax) THEN
	realTmax = realAns1
	ENDIF	
	
	IF (realAns1 .LT. realTmin) THEN
	realTmin = realAns1
	ENDIF	
	
	IF (realAns2 .GT. realUmax) THEN
	realUmax = realAns2
	ENDIF	
	
	IF (realAns2 .LT. realUmin) THEN
	realUmin = realAns2
	ENDIF	
	
	IF (realAns3 .GT. realVmax) THEN
	realVmax = realAns3
	ENDIF	
	
	IF (realAns3 .LT. realVmin) THEN
	realVmin = realAns3
	ENDIF	
	

!	IF ((realAns1 .GT. 1.0) .OR. (realAns1 .LT. 0.0) 
!     . 	.OR. (realAns2 .GT. 1.0) .OR. (realAns2 .LT. 0.0) 
!     .  .OR. (realAns3 .GT. 1.0) .OR. (realAns3 .LT. 0.0)) THEN
!		WRITE(*,*) "index: ", iIndex
!		WRITE(*,*) "	T: ", realAns1
!		WRITE(*,*) "	U: ", realAns2
!		WRITE(*,*) "	V: ", realAns3
			
!		intStatusIndex = iIndex
!		GOTO 200
!	ENDIF


  100	CONTINUE


!  200 	realw = 0.0

	intStatusIndex = 0
	IF ((realTmin .GE. 0.0) .AND. (realTmax .LE. 1.0) 
     . 	.AND. (realUmin .GE. 0.0) .AND. (realUmax .LE. 1.0) 
     .  .AND. (realVmin .GE. 0.0) .AND. (realVmax .LE. 1.0)) THEN
		!all points are inside the box. so set the flag to -1
		intStatusIndex = -1
	ENDIF
 
	
	END

!This is the subroutine that will be responsible for using the Newton Raphson
!method to find the T,U and V parameters of the solid boundary points.
!This subroutine will take as input the initial guesses for T,U and V, the x,y and z 
! values of the point in question, and will return as output the final computed values.
! Also, intH is the index of the element  
	SUBROUTINE NEWTONSOLVE(initT, initU, initV, xElem, yElem, 
     . 	zElem, Ans1, Ans2, Ans3, intStatus, intH)
	REAL :: initT, initU, initV, xElem, yElem, zElem, Ans1, Ans2, Ans3
	REAL :: FindDet
	INTEGER :: intStatus,intH	
	!create a 3x3 matrix for the jacobian
	REAL Jacobian(3,3)
	!create an X vector that will hold the T,U,V values
	REAL TUVVector(3)
	!Create an perturbed X Vector for after dx change is implemented
	REAL TUVVectorPerturb(3)
	!Create a vector for holding the negative function values at X0
	REAL MinusFX0(1,3)	
	INTEGER Pivot(3)
	
!	WRITE(*,*) "	NewtonSolve"
	
	TUVVector(1) = initT
	TUVVector(2) = initU
	TUVVector(3) = initV

	TUVVectorPerturb(1) = 0.0
	TUVVectorPerturb(2) = 0.0
	TUVVectorPerturb(3) = 0.0
		
	!the dx difference used for the partial derivative calculations
	realConst_dx = 0.001
	realConst_tolerance = 0.00001

	realNorm = 0.0
	realJacobianValue = 0.0	
	intInfo = -1
	
	intNumIterations = 0
	intNumIterCutoff = 300
	DO WHILE (.TRUE.)
	!Calculate the Jacobian Matrix
		
	
		!compute the entries for the jacobian using forward difference
		!for the partial derivative

	DO 30 intr = 1,3
	DO 30 intc = 1,3
		! Based on the column in the vector, perturb one of the values in the vector	
		DO 50 intVector = 1,3
			TUVVectorPerturb(intVector) = TUVVector(intVector)
  50		CONTINUE
			TUVVectorPerturb(intc) = TUVVectorPerturb(intc) + realConst_dx
		
			
	
		!Now, check to see what row the loop is in for the jacobian
		!Based on the row, choose which function to use to calculate the 
		!the partial derivative values
		IF (intr .EQ. 1) THEN
			realJacobianValue = xFunction(xElem,TUVVectorPerturb,
     . 	intH)
			realJacobianValue = realJacobianValue - xFunction(xElem, 
     .	TUVVector,intH)
			realJacobianValue = realJacobianValue/realConst_dx
		ENDIF	
		IF (intr .EQ. 2) THEN
			realJacobianValue = yFunction(yElem,TUVVectorPerturb,
     . 	intH)
			realJacobianValue = realJacobianValue - yFunction(yElem, 
     .	TUVVector,intH)
			realJacobianValue = realJacobianValue/realConst_dx
		ENDIF	
		IF (intr .EQ. 3) THEN
			realJacobianValue = zFunction(zElem,TUVVectorPerturb,
     . 	intH)
			realJacobianValue = realJacobianValue - zFunction(zElem, 
     .	TUVVector, intH)
			realJacobianValue = realJacobianValue/realConst_dx
		ENDIF	
	
		Jacobian(intr, intc) = realJacobianValue
  30   	CONTINUE	

	!Input the values into the MinusFX0 vector
	MinusFX0(1,1) = -1*xFunction(xElem, TUVVector,intH)
	MinusFX0(1,2) = -1*yFunction(yElem, TUVVector,intH)
	MinusFX0(1,3) = -1*zFunction(zElem, TUVVector,intH)
		
	!Solve the matrix system using lapack
	CALL SGESV(3,1,Jacobian,3,Pivot, MinusFX0, 3, intInfo)	

	!the MinusFX0 vector now is the deltaX (or deltaTUV vector). Add it to 
	!X0 vector just used (the TUV vector) to get the vector used for the next iteration
		
	!Also, Compute the norm of the difference vector to find when to quit the loop
	realNorm = 0.0
	DO 70 intI =1,3
		TUVVector(intI) = TUVVector(intI) + MinusFX0(1,intI)
		realNorm = realNorm + MinusFX0(1,intI)**2

!		WRITE(*,*) " 	FX0: ", intI, MinusFX0(1,intI)
  70	CONTINUE	
	realNorm = realNorm**(0.5)

!	WRITE(*,*) "	realNorm: ", realNorm

	IF (realNorm .LT. realConst_tolerance) THEN
		intStatus = 1
	ENDIF
	
	IF (intNumIterations .GT. intNumIterCutoff) THEN
		intStatus = 0 !Failure
	ENDIF



	IF (realNorm .LT. realConst_tolerance) THEN
		!intStatus = 1 !Success
		EXIT
	ENDIF	

	IF (intNumIterations .GT. intNumIterCutoff) THEN
		!intStatus = 0 Failure
		EXIT
	ENDIF

	intNumIterations = intNumIterations + 1
!	WRITE(*,*) "intNumIterations: ", intNumIterations

	ENDDO	
	
	!Returning the newly computed T,U,V values
	Ans1 = TUVVector(1)
	Ans2 = TUVVector(2)
	Ans3 = TUVVector(3)
	END


!Gamma is given by the following expression
!sum(i=0 to n) sum(j=0 to m) sum(k=0 to l) 
	![B(i,n,t)*B(j,m,u)*B(k,l,v)*Pijk]
!As input the subroutine will take T,U and V as variables
	SUBROUTINE GAMMAFUNCTION(T,U,V, G1, G2, G3, intH)

	USE VAR

	INTEGER :: FACT, intH
	REAL :: BERNS, T,U,V,G1,G2,G3

	!The n,m and l values for the bezier curve sum
	intn = NXFFD-1
	intm = NYFFD-1
	intl = NZFFD-1
	
	!initializing the parameters that will be output by the subroutine
	G1 = 0.0
	G2 = 0.0
	G3 = 0.0
	
	
	!Perform the triple sum for finding the values G1, G2, G3
	DO 10 i=0,intn
	DO 10 j=0,intm
	DO 10 k=0,intl
		realCoefficient = BERNS(i,intn,T)*BERNS(j,intm,U)*BERNS(k,intl,V)
		G1 = G1 + realCoefficient*FFDPoints(intH,i+1, j+1, k+1,1)
		G2 = G2 + realCoefficient*FFDPoints(intH,i+1, j+1, k+1,2)
		G3 = G3 + realCoefficient*FFDPoints(intH,i+1, j+1, k+1,3)
		
   10	CONTINUE

	END
	
!This function will be analagous to the x function lambda in python. It will be the first 
!row of the vector function that is the [x,y,z] value vector of the point - the gamma
!vector. The roots of these functions will have to be found (t,u,v) for a given x,y and z
	REAL FUNCTION xFunction(xElem, TUVVector,intH)
	INTEGER :: intH
	REAL :: xElem,T,U,V
	REAL :: TUVVector(3)
	realG1 = 0.0
	realG2 = 0.0
	realG3 = 0.0
	T = TUVVector(1)
	U = TUVVector(2)
	V = TUVVector(3)
	CALL GAMMAFUNCTION(T,U,V,realG1,realG2,realG3,intH)
	
	xFunction = xElem - realG1
	
	END

!This function will be the same as the y lambda function in python
	REAL FUNCTION yFunction(yElem, TUVVector,intH)
	INTEGER :: intH
	REAL :: yElem,T,U,V
	REAL :: TUVVector(3)
	realG1 = 0.0
	realG2 = 0.0
	realG3 = 0.0
	T = TUVVector(1)
	U = TUVVector(2)
	V = TUVVector(3)
	CALL GAMMAFUNCTION(T,U,V,realG1,realG2,realG3,intH)
	
	yFunction = yElem - realG2
	
	END


!This function will be the same as the z lambda function in python
	REAL FUNCTION zFunction(zElem, TUVVector, intH)
	INTEGER :: intH
	REAL :: zElem,T,U,V
	REAL :: TUVVector(3)
	realG1 = 0.0
	realG2 = 0.0
	realG3 = 0.0
	T = TUVVector(1)
	U = TUVVector(2)
	V = TUVVector(3)
	CALL GAMMAFUNCTION(T,U,V,realG1,realG2,realG3,intH)
	
	zFunction = zElem - realG3
	
	END


!The Bernstein function takes Integer I, Integer N and Real T as inputs
	REAL FUNCTION BERNS(I,N,T)
	INTEGER :: FACT, I,N
	REAL :: T
	
	rNChooseI = REAL(FACT(N))/REAL((FACT(I)*FACT(N-I)))
	rMultiplier = ((1-T)**(N-I))*((T**I))

	BERNS = rNChooseI*rMultiplier
	END
			

! Takes an integer iI and returns its factorial
	INTEGER FUNCTION FACT(iI)
	ANS = 1
	!WRITE(*,*) "INT(I): ", INT(iI)
	DO 20 k = 1, INT(iI)
		ANS = ANS*k	
   20	CONTINUE
	!WRITE(*,*) "Ans: ", ANS
	FACT = ANS
	END


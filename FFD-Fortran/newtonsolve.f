    !This is the subroutine that will be responsible for using the Newton Raphson
    !method to find the T,U and V parameters of the solid boundary points.
    !This subroutine will take as input the initial guesses for T,U and V, the x,y and z
    ! values of the point in question, and will return as output the final computed values
    SUBROUTINE NEWTONSOLVE(initT, initU, initV, xElem, yElem,
    . 	zElem, Ans1, Ans2, Ans3)
    REAL :: initT, initU, initV, xElem, yElem, zElem, Ans1, Ans2, Ans3
    REAL :: FindDet
    !create a 3x3 matrix for the jacobian
    REAL Jacobian(3,3)
    !create an X vector that will hold the T,U,V values
    REAL TUVVector(3)
    !Create an perturbed X Vector for after dx change is implemented
    REAL TUVVectorPerturb(3)
    !Create a vector for holding the negative function values at X0
    REAL MinusFX0(1,3)
    INTEGER Pivot(3)

    TUVVector(1) = initT
    TUVVector(2) = initU
    TUVVector(3) = initV

    TUVVectorPerturb(1) = 0.0
    TUVVectorPerturb(2) = 0.0
    TUVVectorPerturb(3) = 0.0

    !the dx difference used for the partial derivative calculations
    realConst_dx = 0.001
    realConst_tolerance = 0.001
    realNorm = 0.0
    realJacobianValue = 0.0
    intInfo = -1

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
    realJacobianValue = xFunction(xElem,TUVVectorPerturb)
    realJacobianValue = realJacobianValue - xFunction(xElem,
    .	TUVVector)
    realJacobianValue = realJacobianValue/realConst_dx
    ENDIF
    IF (intr .EQ. 2) THEN
    realJacobianValue = yFunction(yElem,TUVVectorPerturb)
    realJacobianValue = realJacobianValue - yFunction(yElem,
    .	TUVVector)
    realJacobianValue = realJacobianValue/realConst_dx
    ENDIF
    IF (intr .EQ. 3) THEN
    realJacobianValue = zFunction(zElem,TUVVectorPerturb)
    realJacobianValue = realJacobianValue - zFunction(zElem,
    .	TUVVector)
    realJacobianValue = realJacobianValue/realConst_dx
    ENDIF

    Jacobian(intr, intc) = realJacobianValue
    30   	CONTINUE

    !Input the values into the MinusFX0 vector
    MinusFX0(1,1) = -1*xFunction(xElem, TUVVector)
    MinusFX0(1,2) = -1*yFunction(yElem, TUVVector)
    MinusFX0(1,3) = -1*zFunction(zElem, TUVVector)

    !Solve the matrix system using lapack
    CALL SGESV(3,1,Jacobian,3,Pivot, MinusFX0, 3, intInfo)

    !the MinusFX0 vector now is the deltaX (or deltaTUV vector). Add it to
    !X0 vector just used (the TUV vector) to get the vector used for the next iteration

    !Also, Compute the norm of the difference vector to find when to quit the loop
    realNorm = 0.0
    DO 70 intI =1,3
    TUVVector(intI) = TUVVector(intI) + MinusFX0(1,intI)
    realNorm = realNorm + MinusFX0(1,intI)**2
    70	CONTINUE
    realNorm = realNorm**(0.5)
    IF (realNorm < realConst_tolerance) THEN
    EXIT
    ENDIF

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
    SUBROUTINE GAMMAFUNCTION(T,U,V, G1, G2, G3)

    USE VAR

    INTEGER :: FACT
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
    G1 = G1 + realCoefficient*FFDPoints(1,i+1, j+1, k+1,1)
    G2 = G2 + realCoefficient*FFDPoints(1,i+1, j+1, k+1,2)
    G3 = G3 + realCoefficient*FFDPoints(1,i+1, j+1, k+1,3)

    10	CONTINUE

    END

    !This function will be analagous to the x function lambda in python. It will be the first
    !row of the vector function that is the [x,y,z] value vector of the point - the gamma
    !vector. The roots of these functions will have to be found (t,u,v) for a given x,y and z
    REAL FUNCTION xFunction(xElem, TUVVector)
    REAL :: xElem,T,U,V
    REAL :: TUVVector(3)
    realG1 = 0.0
    realG2 = 0.0
    realG3 = 0.0
    T = TUVVector(1)
    U = TUVVector(2)
    V = TUVVector(3)
    CALL GAMMAFUNCTION(T,U,V,realG1,realG2,realG3)

    xFunction = xElem - realG1

    END

    !This function will be the same as the y lambda function in python
    REAL FUNCTION yFunction(yElem, TUVVector)
    REAL :: yElem,T,U,V
    REAL :: TUVVector(3)
    realG1 = 0.0
    realG2 = 0.0
    realG3 = 0.0
    T = TUVVector(1)
    U = TUVVector(2)
    V = TUVVector(3)
    CALL GAMMAFUNCTION(T,U,V,realG1,realG2,realG3)

    yFunction = yElem - realG2

    END


    !This function will be the same as the z lambda function in python
    REAL FUNCTION zFunction(zElem, TUVVector)
    REAL :: zElem,T,U,V
    REAL :: TUVVector(3)
    realG1 = 0.0
    realG2 = 0.0
    realG3 = 0.0
    T = TUVVector(1)
    U = TUVVector(2)
    V = TUVVector(3)
    CALL GAMMAFUNCTION(T,U,V,realG1,realG2,realG3)

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


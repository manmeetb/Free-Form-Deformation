
import numpy
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import math


CONST_DATAFILEInitial = "DataFiles/FortranPointDataWingZInitial.txt"
CONST_DATAFILEFinal = "DataFiles/FortranPointDataWingZFinal.txt"
#CONST_BodyType = "halfFuselage"
CONST_BodyType = " "

"""
CONST_DATAFILEInitial = "DataFiles/FortranPointDataCRMInitial.txt"
CONST_DATAFILEFinal = "DataFiles/FortranPointDataCRMFinal.txt"
CONST_BodyType = "CRMWing"
"""

CONST_nXFDD = 10
CONST_nYFDD = 3
CONST_nZFDD = 8


"""
    A method that will be used to solve a nonlinear system of equations using the Newton Raphson method.
    So, F(x) is a vector with the follwing components [f1(X), f2(X), ...] , where X is the vector of variables.
    Using Taylor's theorem, the following statement can be obtained:
    
    F(X) = F(X0) + DF(X0)*(delta X), where DF(X0) is the jacobian.
    
    In this module, we will take a variable n which will say how many equations and variables have to be sovled.
    It will then take the functions as a vector (which will lambda expressions or defined
    functions). It is on the user that the functions from the user take in the same number of arguments as the number n
    given. An iterative process using the numpy libraries then, the deltaX vector will be solved for until it becomes close
    to machine 0 which means that the solution has been found.
    
    """
#The dx variable used for calculating the jacobian
CONST_dx = 0.001

CONST_Tolerance = 0.01

#args holds the functions
def NewtonSolve(N, initialGuess , systemOfFunctions):
    
    counter = 0
    X0 = initialGuess
    while(True):
        Jacobian = calculateJacobian(N,systemOfFunctions, X0)
        
        #create the system of equations to solve. DF(X0) * DeltaX = - F(X0). From this,
        #DeltaX can be solved for. DF(X0) is the Jacobian variable from the line before.
        #Now, create the vector F(X0)
        size = (N,1)
        MinusFX0 = numpy.zeros(size)
        
        for i in range(N):
            MinusFX0[i] = -systemOfFunctions[i](X0)
    
        deltaXVector = numpy.linalg.solve(Jacobian, MinusFX0)
        
        #deltaXVector = X1 - X0. Calculate X1 and use it for the next iteration
        #deltaXVector + X0 = X1
        for i in range(N):
            X0[i] = deltaXVector[i][0] + X0[i]

        counter+=1
        #to make sure an infinite loop isnt entered
        if(counter>100):
            break
        
        #if the delta x vector's norm is sufficiently small then quit the loop:
        norm = 0
        for i in range(N):
            norm += deltaXVector[i]**2
        
        norm = norm**(1./float(N))
        if(norm < CONST_Tolerance):
            break

    #returning the solution vector
    return X0


#Each function has to take a vector as input. the vector is the vector of variables X
#X0 is the vector about which the jacobian is calculated
def calculateJacobian(N,systemOfFunctions, X0):
    
    #create an N*N matrix of zeros for the jacobian
    size = (N,N)
    Jacobian = numpy.zeros(size)
    
    for r in range(N):
        for c in range(N):
            #r is the row of interest and c is the column of interest. The loop will go through one row at a time
            
            #the column value will dictate which element in the X vector to perturb. Perturb this x position at the
            #beginning of the loop and then remove the perturbation after calculate an approximation of the partial
            delfrBydelXcAtX0 = -systemOfFunctions[r](X0)/CONST_dx
            X0[c] = X0[c] + CONST_dx
            delfrBydelXcAtX0 += systemOfFunctions[r](X0)/CONST_dx
            
            Jacobian[r][c] = delfrBydelXcAtX0
            X0[c] = X0[c] - CONST_dx
    
    return Jacobian





                #Classes

#A class that will hold all the data for each boundary point.
class solidBoundaryPoint(object):
    
    def __init__(self, xInitial, yInitial, zInitial):
        self.x = xInitial
        self.y = yInitial
        self.z = zInitial
        
        self.t = 0
        self.u = 0
        self.v = 0
    
    #The getters
    def getX(self):
        return self.x
    
    def getY(self):
        return self.y
    
    def getZ(self):
        return self.z
    
    def getT(self):
        return self.t
    
    def getU(self):
        return self.u
    
    def getV(self):
        return self.v
    
    #The setters
    def setT(self, t):
        self.t = t
    
    def setU(self, u):
        self.u = u
    
    def setV(self, v):
        self.v = v
    
    def setX(self, x):
        self.x = x
    
    def setY(self, y):
        self.y = y
    
    def setZ(self, z):
        self.z = z




class FFDPointElement(object):
    def __init__(self, xInitial, yInitial, zInitial):
        self.x = xInitial
        self.y = yInitial
        self.z = zInitial
    
    def getX(self):
        return self.x
    
    def getY(self):
        return self.y
    
    def getZ(self):
        return self.z
    
    def setX(self, x):
        self.x = x
    
    def setY(self, y):
        self.y = y
    
    def setZ(self, z):
        self.z = z




                    #Methods

#The methods used to parse a string of 3 numbers with commas
#in between them.
def ParseLine(line):
    x = 0
    y = 0
    z = 0
    for i in range(3):
        Comma = line.find(",")
        if (Comma == -1):
            # on last number
            Num = line[0: len(line)]
            z = float(Num)
            break
        Num = line[0:Comma]
        line = line[Comma + 1:len(line)]
        if (i == 0):
            x = float(Num)
        elif (i == 1):
            y = float(Num)

    return (x,y,z)

#deform the FFD points arbitrarily
def deformFFDPoints(FFDPointArray):    
    for i in range(CONST_nXFDD):
        for j in range(CONST_nYFDD):
            for k in range(CONST_nZFDD):
                #shift on row of ffd points by 0.3 in the y direction
                if(k==CONST_nZFDD-1):
                    element = FFDPointArray[i][j][k]
                    newYValue = element.getY()+0.3
                    element.setY(newYValue)


def modifyShape(solidBoundaryPointArray, FFDPointArray):
    
    #compute the new coordinates of all the solid boundary points using the FFD points
    
    # do a sum from i=0 to n, where n is the number of spaces between FFD points (so if there are n xFFD points then
    # there are n-1 spaces).
    n = CONST_nXFDD - 1
    m = CONST_nYFDD - 1
    l = CONST_nZFDD - 1
    
    for solidElement in solidBoundaryPointArray:
        
        xNew = 0
        yNew = 0
        zNew = 0
        
        t = solidElement.getT()
        u = solidElement.getU()
        v = solidElement.getV()
        
        for i in range(CONST_nXFDD):
            for j in range(CONST_nYFDD):
                for k in range(CONST_nZFDD):
                    xNew = xNew + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getX()
                    yNew = yNew + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getY()
                    zNew = zNew + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getZ()
        solidElement.setX(xNew)
        solidElement.setY(yNew)
    	solidElement.setZ(zNew)

def AttachFFDNewton(SolidBoundaryPointArray):
    for i in range(len(solidBoundaryPointArray)):
    #for i in range(10):
        print "Attach FFD i: " + str(i)
        if(i<11):
            element = SolidBoundaryPointArray[i]
            
            xElem = element.getX()
            yElem = element.getY()
            zElem = element.getZ()
            
            #capture the xElem, yElem and zElem values (these change in the loop so capture these
            #values for what they are when the lambda function is created)
            testFunction_1 = (lambda X, xElem = xElem: xElem - Gamma(X)[0])
            testFunction_2 = (lambda X, yElem = yElem: yElem - Gamma(X)[1])
            testFunction_3 = (lambda X, zElem = zElem: zElem - Gamma(X)[2])
            
            testFunctionArray = [testFunction_1, testFunction_2, testFunction_3]
            AnswerArray = NewtonSolve(3, [0.5, 0.5, 0.5], testFunctionArray)
            element.setT(AnswerArray[0])
            element.setU(AnswerArray[1])
            element.setV(AnswerArray[2])
        else:
            element = SolidBoundaryPointArray[i]
            element.setT(0)
            element.setU(0)
            element.setV(0)


def B(i,n,t):
    return (math.factorial(n)/(math.factorial(i)*math.factorial(n-i)))*((1-t)**(n-i))*(t**i)

#Gamma is given by the following expression
#sum(i=0 to n) sum(j=0 to m) sum(k=0 to l) [B(i,n,t)*B(j,m,u)*B(k,l,v)*Pijk]
#the X vector has the following components: X[t,u,v]
def Gamma(X):
    t = X[0]
    u = X[1]
    v = X[2]
    
    sum = [0,0,0]
    
    # do a sum from i=0 to n, where n is the number of spaces between FFD points (so if there are n xFFD points then
    # there are n-1 spaces).
    n = CONST_nXFDD-1
    m = CONST_nYFDD-1
    l = CONST_nZFDD-1
    
    for i in range(CONST_nXFDD):
        for j in range(CONST_nYFDD):
            for k in range(CONST_nZFDD):
                #so now we have the Pijk element (i,j,k are all from 0 to n, (there are n+1 points))
                sum[0] = sum[0] + B(i,n,t)*B(j,m,u)*B(k,l,v)*FFDPointArray[i][j][k].getX()
                sum[1] = sum[1] + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getY()
                sum[2] = sum[2] + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getZ()

    return sum

def PlotData():
    # read the data from the file and fill the data structures
    initializeDataFromFile(solidBoundaryPointArray, FFDPointArray)
    
    #solidBoundaryPointArray.sort(key=lambda x: x.getZ(), reverse=True)
    fillInitialArrays(solidBoundaryPointArray, FFDPointArray, xsolidInitial,
                      ysolidInitial, zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial)

    fillFinalArrays(FFDPointArray, solidBoundaryPointArray)
    
    """
    plotInitialState(xsolidInitial,ysolidInitial, zsolidInitial, xFFDInitial,
                     yFFDInitial, zFFDInitial)
    """
    
    plotFFDandSolidBNDAndInitialFFDandSolidBND(FFDPointArray, \
                    solidBoundaryPointArray, xsolidInitial,ysolidInitial, \
                    zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial)



def fillFinalArrays(FFDPointArray, solidBoundaryPointArray):
    #clear the elements of the list
    del solidBoundaryPointArray[:]

    fileData = open(CONST_DATAFILEFinal, "r")
    
    # first read the FFD point data
    lineFFD = fileData.readline()
    while(True):
        
        #read the IJK data line
        lineIJK = fileData.readline()
        
        if("Solid Boundary Point Data" in lineIJK):
            break
    
        #otherwise, there are now two lines of FFD data to read. Read the second line too
        
        lineXYZ = fileData.readline()
        
        #parse the IJK data line
        I,J,K = ParseLine(lineIJK)
        #convert the I,J,K data into integers
        I = int(I)
        J = int(J)
        K = int(K)
        X,Y,Z = ParseLine(lineXYZ)
        #create an FFD point element and store the data in the object. place the object in the FFD Point list
        FFDElement = FFDPointElement(X,Y,Z)
        FFDPointArray[I][J][K] = FFDElement


    #Now read the solid boundary point data
    while(True):
        lineXYZSolid = fileData.readline()
        #the end of the solid boundary point data, and the file, has been reached
        if(lineXYZSolid == ""):
            break

        #Loop didn't break so there is more data to read
        #read the T,U,V line
        lineTUVSolid = fileData.readline()
        
        Xsolid, Ysolid, Zsolid = ParseLine(lineXYZSolid)
        T, U, V = ParseLine(lineTUVSolid)
        
        #create the solidBoundaryPoint element and store the data into the object. Then add the object to the
        # solidBoundaryPoint array
        solidBndElement = solidBoundaryPoint(Xsolid,Ysolid,Zsolid)
        solidBndElement.setT(T)
        solidBndElement.setU(U)
        solidBndElement.setV(V)
        solidBoundaryPointArray.append(solidBndElement)

    fileData.close()

def printTUVData(solidBoundaryPointArray):
    # Iterate through all the solid boundary points and print out the T, U, V and the coordinate data
    for i in range(len(solidBoundaryPointArray)):
        elementi = solidBoundaryPointArray[i]
        print "i + 1: " + str(i + 1)
        print "T, U, V: " + str(elementi.getT()) + "  " + str(elementi.getU()) + "  " + str(elementi.getV())
        print "x, y ,z: " + str(elementi.getX()) + "  " + str(elementi.getY()) + "  " + str(elementi.getZ())

def printFFDAndSolidBndData(FFDPointArray, solidBoundaryPointArray):
    print "FFD: "
    for i in range(CONST_nXFDD):
        for j in range(CONST_nYFDD):
            for k in range(CONST_nZFDD):
                element = FFDPointArray[i][j][k]
                print "i,j,k: " + str(i) + ", " + str(j) + ", " + str(k)
                print "x,y,z: " + str(element.getX()) + ", " + str(element.getY()) + ", " + str(element.getZ())

    print "solid boundary point data: "
    printTUVData(solidBoundaryPointArray)

def FFDSolve():
    # read the data from the file and fill the data structures
    initializeDataFromFile(solidBoundaryPointArray, FFDPointArray)
    solidBoundaryPointArray.sort(key=lambda x: x.getZ(), reverse=True)
    fillInitialArrays(solidBoundaryPointArray, FFDPointArray, xsolidInitial,
                      ysolidInitial, zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial)
                      
    """
    AttachFFDNewton(solidBoundaryPointArray)
    printFFDAndSolidBndData(FFDPointArray, solidBoundaryPointArray)
    printTUVData(solidBoundaryPointArray)
    
    
   
    plotFiguresTemp(xsolidInitial, ysolidInitial, zsolidInitial,
        xFFDInitial, yFFDInitial, zFFDInitial)
       """
  
    deformFFDPoints(FFDPointArray)
    modifyShape(solidBoundaryPointArray, FFDPointArray)
    
    
    plotFFDandSolidBNDAndInitialFFDandSolidBND(FFDPointArray, \
        solidBoundaryPointArray, xsolidInitial,ysolidInitial, \
        zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial)
   



#For reading the data into the data structures from the file
def initializeDataFromFile(solidBoundaryPointArray, FFDPointArray):
    fileData = open(CONST_DATAFILEInitial, "r")
    
    # first read the FFD point data
    lineFFD = fileData.readline()
    while(True):
        
        #read the IJK data line
        lineIJK = fileData.readline()

        if("Solid Boundary Point Data" in lineIJK):
            break
    
        #otherwise, there are now two lines of FFD data to read. Read the second line too
        
        lineXYZ = fileData.readline()
        
        #parse the IJK data line
        I,J,K = ParseLine(lineIJK)
        #convert the I,J,K data into integers
        I = int(I)
        J = int(J)
        K = int(K)
        X,Y,Z = ParseLine(lineXYZ)
        #create an FFD point element and store the data in the object. place the object in the FFD Point list
    	FFDElement = FFDPointElement(X,Y,Z)
    	FFDPointArray[I][J][K] = FFDElement
    
    
    #Now read the solid boundary point data
    while(True):
        lineXYZSolid = fileData.readline()
        #the end of the solid boundary point data, and the file, has been reached
        if(lineXYZSolid == ""):
            break
    
        #Loop didn't break so there is more data to read
        #read the T,U,V line
        lineTUVSolid = fileData.readline()
        
        Xsolid, Ysolid, Zsolid = ParseLine(lineXYZSolid)
        T, U, V = ParseLine(lineTUVSolid)
    
        #create the solidBoundaryPoint element and store the data into the object. Then add the object to the
    	# solidBoundaryPoint array
    	solidBndElement = solidBoundaryPoint(Xsolid,Ysolid,Zsolid)
    	solidBndElement.setT(T)
    	solidBndElement.setU(U)
    	solidBndElement.setV(V)    
    	solidBoundaryPointArray.append(solidBndElement)
    
    fileData.close()

def plotFiguresTemp(xSolid, ySolid, zSolid, xFFDDeform, yFFDDeform, zFFDDeform):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    
    # Axes3D.plot_wireframe(ax, z, x, y)
    ax.set_xlabel('Z axis')
    ax.set_ylabel('X axis')
    ax.set_zlabel('Y axis')
    
    Axes3D.plot_wireframe(ax, zSolid, xSolid, ySolid, rstride = 1, cstride = 1, color="b")
    Axes3D.scatter(ax, zFFDDeform, xFFDDeform, yFFDDeform, s=30, c='r')
    
    #Axes3D.scatter(ax, zPointArray, xPointArray, yPointArray, s=30, c = 'r')
    
    
    # Axes3D.set_ylim(ax, [-0.5,4.5])
    # Axes3D.set_xlim(ax, [-0.5,4.5])
    Axes3D.set_zlim(ax, [-0.7, 0.7])
    
    plt.show(block=True)


def fillInitialArrays(solidBoundaryPointArray, FFDPointArray, xsolidInitial,
                      ysolidInitial, zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial):
    # filling the object's solid point arrays
    for element in solidBoundaryPointArray:
        xsolidInitial.append(element.getX())
        ysolidInitial.append(element.getY())
        zsolidInitial.append(element.getZ())
    
    # filling the FFD arrays
    for i in range(CONST_nXFDD):
        for j in range(CONST_nYFDD):
            for k in range(CONST_nZFDD):
                element = FFDPointArray[i][j][k]
                xFFDInitial.append(element.getX())
                yFFDInitial.append(element.getY())
                zFFDInitial.append(element.getZ())


def plotFFDandSolidBNDAndInitialFFDandSolidBND(FFDPointArray, solidBoundaryPointArray, xsolidInitial,
                                               ysolidInitial, zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial):
    # Set up the data structures
    xsolid = []
    ysolid = []
    zsolid = []
    
    # For the FFD Points
    xFFD = []
    yFFD = []
    zFFD = []
    
    #filling the object's solid point arrays
    for element in solidBoundaryPointArray:
        xsolid.append(element.getX())
        ysolid.append(element.getY())
        zsolid.append(element.getZ())
    
    # filling the FFD arrays
    for i in range(CONST_nXFDD):
        for j in range(CONST_nYFDD):
            for k in range(CONST_nZFDD):
                element = FFDPointArray[i][j][k]
                xFFD.append(element.getX())
                yFFD.append(element.getY())
                zFFD.append(element.getZ())

    plotFigures(xsolid, ysolid, zsolid, xFFD, yFFD, zFFD, xsolidInitial,
            ysolidInitial, zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial)


def plotInitialState(xsolidInitial,ysolidInitial, zsolidInitial,
                     xFFDInitial, yFFDInitial, zFFDInitial):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    
    # Axes3D.plot_wireframe(ax, z, x, y)
    ax.set_xlabel('Z axis')
    ax.set_ylabel('X axis')
    ax.set_zlabel('Y axis')
    
    Axes3D.scatter(ax, zsolidInitial, xsolidInitial, ysolidInitial, s=10, c='b')
    #Axes3D.plot_wireframe(ax, zsolidInitial, xsolidInitial, ysolidInitial,rstride = 1, cstride = 1, color="y")
    Axes3D.scatter(ax, zFFDInitial, xFFDInitial, yFFDInitial, s=30, c='r')
    
    
    #Axes3D.set_ylim(ax, [-0.5,4.5])
    #Axes3D.set_xlim(ax, [-0.5,4.5])
    Axes3D.set_zlim(ax, [-0.7, 0.7])
    
    plt.show(block=True)



def plotFigures(xSolid,ySolid,zSolid,xFFDDeform,yFFDDeform,zFFDDeform, xsolidInitial,
               ysolidInitial, zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')


    # Axes3D.plot_wireframe(ax, z, x, y)
    ax.set_xlabel('Z axis')
    ax.set_ylabel('X axis')
    ax.set_zlabel('Y axis')

#Axes3D.scatter(ax, zSolid, xSolid, ySolid, s=10, c='b')
    #Axes3D.plot_wireframe(ax, zSolid, xSolid, ySolid, rstride = 1, cstride = 1, color="b")
#Axes3D.scatter(ax, zFFDDeform, xFFDDeform, yFFDDeform, s=30, c='r')

    Axes3D.scatter(ax, zsolidInitial, xsolidInitial, ysolidInitial, s=10, c='b')
    #Axes3D.plot_wireframe(ax, zsolidInitial, xsolidInitial, ysolidInitial,rstride = 1, cstride = 1, color="y")
    Axes3D.scatter(ax, zFFDInitial, xFFDInitial, yFFDInitial, s=30, c='r')


    if (CONST_BodyType == "halfFuselage"):
        Axes3D.set_zlim(ax, [0, 4])
        Axes3D.set_ylim(ax, [0, 4])
        Axes3D.set_xlim(ax, [0, 12])
    else:
        #Axes3D.set_ylim(ax, [-0.5,4.5])
        #Axes3D.set_xlim(ax, [-0.5,4.5])
        Axes3D.set_zlim(ax, [-1.2, 1.2])

    plt.show(block=True)





                    #Main Method

#The lists that will hold the solid boundary point and FFD point objects
solidBoundaryPointArray = []
FFDPointArray = []
for i in range(CONST_nXFDD):
    rowj = []
    for j in range(CONST_nYFDD):
        rowk = []
        for k in range(CONST_nZFDD):
            rowk.append(FFDPointElement(0,0,0))
        rowj.append(rowk)
    FFDPointArray.append(rowj)


#create arrays that will hold the initial object's solid points and the initial FFD points
# For the initial solid boundary points
xsolidInitial = []
ysolidInitial = []
zsolidInitial = []

# For the initial FFD Points
xFFDInitial = []
yFFDInitial = []
zFFDInitial = []


# filling the FFD arrays
for i in range(CONST_nXFDD):
    for j in range(CONST_nYFDD):
        for k in range(CONST_nZFDD):
            element = FFDPointArray[i][j][k]


#FFDSolve()
PlotData()
"""
for element in solidBoundaryPointArray:
    if(element.getT()>1 or element.getU()>1 or element.getV()>1 or element.getT()<0
       or element.getU()<0 or element.getV()<0):
        print "T, U, V: " + str(element.getT()) + "  " + str(element.getU()) + "  " + str(element.getV())
        print "x, y ,z: " + str(element.getX()) + "  " + str(element.getY()) + "  " + str(element.getZ())
"""








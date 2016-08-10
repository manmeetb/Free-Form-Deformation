

"""

This python script will be responsible for studying the movement of the points on the wing that want
to be constrained.

TO DO:
(done) - find the leading edge points and place them into a list data structure.
    (done)- plot the points in red along with the other plane points.
(done) - find the trailing edge points and place them into a list.
    (done)- plot the points in red along with the other plane points.
- find the wing tip points and place them into a list.
    - plot the points in red along with the other plane points.
(done) - plot the isoparametric lines for the planform of the wing. Also plot the lines for the
    other dimensions also.
- study the effect of the placement of the isoparametric lines on the movement of the leading edge
    control points.
    - To plot the data, for each z cross section, plot the largest displacement

"""


import numpy
import matplotlib.pyplot as plt
#from scipy.spatial import ConvexHull
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import math



#The data file with the initial data points
CONST_DATAFILEInitial = "DataFiles/FortranPointDataCRMInitial.txt"
CONST_DATAFILEFinal ="DataFiles/FortranPointDataCRMFinal.txt"


CONST_nXFDD = 10
CONST_nYFDD = 3
CONST_nZFDD = 8

#A class that will hold all the data for each boundary point.
class solidBoundaryPoint(object):
    
    def __init__(self, xInitial, yInitial, zInitial):
        self.x = xInitial
        self.y = yInitial
        self.z = zInitial
        
        self.initialX = xInitial
        self.initialY = yInitial
        self.initialZ = zInitial
        
        self.t = 0
        self.u = 0
        self.v = 0
    
        #by default make the element label '-'
        self.label = "-"
    
    #The getters
    def getX(self):
        return self.x
    
    def getY(self):
        return self.y
    
    def getZ(self):
        return self.z
    
    def getXInitial(self):
        return self.initialX
    def getYInitial(self):
        return self.initialY
    def getZInitial(self):
        return self.initialZ
    
    def getT(self):
        return self.t
    
    def getU(self):
        return self.u
    
    def getV(self):
        return self.v
    
    def getLabel(self):
        return self.label
    
    def getLabelInteger(self):
        if (self.label == "-"):
            return 0
        if (self.label == "LeadingEdge"):
            return 1
        if (self.label == "TrailingEdge"):
            return 2
        if (self.label == "WingTip"):
            return 3
        if (self.label == "WingRoot"):
            return 4

    
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

    def setLabel(self,labelString):
        self.label = labelString


class FFDPointElement(object):
    def __init__(self, xInitial, yInitial, zInitial):
        self.x = xInitial
        self.y = yInitial
        self.z = zInitial
    
        self.initialX = xInitial
        self.initialY = yInitial
        self.initialZ = zInitial
    
    #The getters:
    def getX(self):
        return self.x
    
    def getY(self):
        return self.y
    
    def getZ(self):
        return self.z

    def getXInitial(self):
        return self.initialX
    def getYInitial(self):
        return self.initialY
    def getZInitial(self):
        return self.initialZ
    
    
    #The setters:
    def setX(self, x):
        self.x = x
    
    def setY(self, y):
        self.y = y
    
    def setZ(self, z):
        self.z = z















        #Methods

#Takes two floats N1 and N2 and compares them to their second decimal place.
# returns true if the numbers are the same and false otherwise
def compareDecimalPlace(N1, N2, numDecimalPlaces):
    intN1 = int(N1*(10**numDecimalPlaces))
    intN2 = int(N2*(10**numDecimalPlaces))
    if(intN1 == intN2):
        return True
    else:
        return False


def AnalyzeData():
    LoadData()
    LeadingEdgePointsList = []
    TrailingEdgePointsList = []
    WingTipPointsList = []
    WingRootPointsList = []
    FindLeadingEdgePoints(LeadingEdgePointsList)
    FindTrailingEdgePoints(TrailingEdgePointsList)
    FindWingTipPoints()
    FindWingRootPoints()
    
    #Plot the data
    
    #fill the LeadingEdgePoint Data into arrays to be plotted
    LeadingEdgeX = []
    LeadingEdgeY = []
    LeadingEdgeZ = []
    
    for element in LeadingEdgePointsList:
        LeadingEdgeX.append(element.getX())
        LeadingEdgeY.append(element.getY())
        LeadingEdgeZ.append(element.getZ())
    
    #for the leading edge, just remove some points near the wing root (for some reason
    # points not on the leading edge are being thought of as leading edge points)
    # So, for bigger than about z = 0.4, remove the leading  edge label
    
    for element in solidBoundaryPointArray:
        if (element.getLabel() == "LeadingEdge"):
            if (element.getZ() < 0.4):
                element.setLabel("-")
    
    
    """
    plotFFDPointsAndPlane(xsolidInitial,ysolidInitial, zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial,
                          LeadingEdgeX, LeadingEdgeY, LeadingEdgeZ)
    """
    plotLeadingEdgeData()
    plotFizedCrossSectionData()


#Go through all the points and find the wing tip points. This will be the
# cross section with the largest z value
def FindWingTipPoints():
    ZCrossSectionsList = []
    ZCrossSectionsListRow = []
    numCrossSections = 0
    previousZValue = solidBoundaryPointArray[0].getZInitial()
    for element in solidBoundaryPointArray:
        if(compareDecimalPlace(element.getZ(), previousZValue, 2) == False):
            previousZValue = element.getZInitial()
            ZCrossSectionsList.append(ZCrossSectionsListRow)
            numCrossSections = numCrossSections + 1
            ZCrossSectionsListRow = []
        
        #print "different Z Value: " + str(element.getZ())
        
        else:
            # we are still in the same cross section
            ZCrossSectionsListRow.append(element)

    for element in ZCrossSectionsList[0]:
        element.setLabel("WingTip")



def FindWingRootPoints():
    ZCrossSectionsList = []
    ZCrossSectionsListRow = []
    numCrossSections = 0
    previousZValue = solidBoundaryPointArray[0].getZInitial()
    for element in solidBoundaryPointArray:
        if(compareDecimalPlace(element.getZ(), previousZValue, 2) == False):
            previousZValue = element.getZInitial()
            ZCrossSectionsList.append(ZCrossSectionsListRow)
            numCrossSections = numCrossSections + 1
            ZCrossSectionsListRow = []
        
        #print "different Z Value: " + str(element.getZ())
        
        else:
            # we are still in the same cross section
            ZCrossSectionsListRow.append(element)

    for element in ZCrossSectionsList[numCrossSections-1]:
        element.setLabel("WingRoot")


def plotFizedCrossSectionData():
    ZCrossSectionsList = []
    numCrossSections = 0
    ZCrossSectionsListRow = []
    previousZValue = solidBoundaryPointArray[0].getZInitial()
    for element in solidBoundaryPointArray:
        if(compareDecimalPlace(element.getZ(), previousZValue, 2) == False):
            previousZValue = element.getZInitial()
            ZCrossSectionsList.append(ZCrossSectionsListRow)
            ZCrossSectionsListRow = []
            numCrossSections = numCrossSections + 1
        #print "different Z Value: " + str(element.getZ())
        
        else:
            # we are still in the same cross section
            ZCrossSectionsListRow.append(element)

    ZCrossSectionsList[int(numCrossSections/2)].sort(key=lambda x: x.getZInitial(), reverse=False)

    xInitialCross = []
    yInitialCross = []
    
    xFinalCross = []
    yFinalCross = []

    #plot the data for the middle cross section
    for element in ZCrossSectionsList[int(numCrossSections/2)]:
        xInitialCross.append(element.getXInitial())
        yInitialCross.append(element.getYInitial())
        xFinalCross.append(element.getX())
        yFinalCross.append(element.getY())

    plt.scatter(xInitialCross, yInitialCross, s=25, c="b")
    plt.scatter(xFinalCross, yFinalCross, s=25, c="r")



#plt.plot(xInitialCross, yInitialCross)
#plt.plot(xFinalCross, yFinalCross)
    plt.show(block = True)


def plotLeadingEdgeData():
    
    LeadingEdgePointsList = []
    
    for element in solidBoundaryPointArray:
        if(element.getLabel() == "LeadingEdge"):
            LeadingEdgePointsList.append(element)
    
    LeadingEdgePointsList.sort(key=lambda x: x.getZInitial(), reverse=False)

    # plot the difference in the y value vs the z coordinate
    zInitialVector = []
    differenceVector = []

    for element in LeadingEdgePointsList:
        
        print "z: " + str(element.getZInitial())
        zInitialVector.append(element.getZInitial())
        print "     y1: " + str(element.getYInitial())
        print "     y2: " + str(element.getY())
        differenceVector.append(element.getY() - element.getYInitial())

    plt.scatter(zInitialVector, differenceVector)
    plt.show(block = True)


#The method that is used for finding the leading edge points. We will define the leading edge
#to be the points with the 5 smallest x values (the way the wing is set up it faces in the
# minus x direction)
def FindTrailingEdgePoints(TrailingEdgePointsList):
    
    #Iterate through all the solid boundary point array cross sections.
    #Based on how the points coordinates are placed, the different z cross sections
    # can be defined as being different by comparing the z values up to the 2nd decimal
    # place.
    
    ZCrossSectionsList = []
    numElementsInCrossSection = 0
    ZCrossSectionsListRow = []
    previousZValue = solidBoundaryPointArray[0].getZInitial()
    for element in solidBoundaryPointArray:
        if(compareDecimalPlace(element.getZ(), previousZValue, 2) == False):
            previousZValue = element.getZInitial()
            ZCrossSectionsList.append(ZCrossSectionsListRow)
            ZCrossSectionsListRow = []
        #print "different Z Value: " + str(element.getZ())
        
        else:
            # we are still in the same cross section
            ZCrossSectionsListRow.append(element)

    #Now to make sure only proper cross sections are studied
    # ignore cross sections that are too small (with less than
    # 50 points in it). Go through each row and store the 5 smallest
    # points in each cross section.

    for row in ZCrossSectionsList:
        if(len(row)>50 and len(row)<150):
            tempRow = []
            for element in row:
                # Find the 5 smallest x value points in the row
                
                # Not sure what the effects of sorting the row will be
                #So for now put everything into a temporary row and use that
                tempRow.append(element)
            
            tempRow.sort(key=lambda x: x.getXInitial(), reverse=True)
            # The elements are now sorted from smallest to
            # largest x in the tempRow. Put the first 5 elements
            # into the array
            
            for i in range(5):
                element = tempRow[i]
                element.setLabel("TrailingEdge")
                TrailingEdgePointsList.append(element)




#The method that is used for finding the leading edge points. We will define the leading edge
#to be the points with the 5 smallest x values (the way the wing is set up it faces in the
# minus x direction)
def FindLeadingEdgePoints(LeadingEdgePointsList):
    
    #Iterate through all the solid boundary point array cross sections.
    #Based on how the points coordinates are placed, the different z cross sections
    # can be defined as being different by comparing the z values up to the 2nd decimal
    # place.
    
    ZCrossSectionsList = []
    numElementsInCrossSection = 0
    ZCrossSectionsListRow = []
    previousZValue = solidBoundaryPointArray[0].getZInitial()
    for element in solidBoundaryPointArray:
        if(compareDecimalPlace(element.getZ(), previousZValue, 2) == False):
            previousZValue = element.getZInitial()
            ZCrossSectionsList.append(ZCrossSectionsListRow)
            ZCrossSectionsListRow = []
            #print "different Z Value: " + str(element.getZ())
        
        else:
            # we are still in the same cross section
            ZCrossSectionsListRow.append(element)

    #Now to make sure only proper cross sections are studied
    # ignore cross sections that are too small (with less than
    # 50 points in it). Go through each row and store the 5 smalles
    # points in each cross section.

    for row in ZCrossSectionsList:
        if(len(row)>50 and len(row)<150):
            tempRow = []
            for element in row:
                # Find the 5 smallest x value points in the row

                # Not sure what the effects of sorting the row will be
                #So for now put everything into a temporary row and use that
                tempRow.append(element)

            tempRow.sort(key=lambda x: x.getXInitial(), reverse=False)
            # The elements are now sorted from smallest to
            # largest x in the tempRow. Put the first 5 elements
            # into the array
            
            for i in range(5):
                element = tempRow[i]
                element.setLabel("LeadingEdge")
                LeadingEdgePointsList.append(element)



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


def B(i,n,t):
    return (math.factorial(n)/(math.factorial(i)*math.factorial(n-i)))*((1-t)**(n-i))*(t**i)

def computeXYZfromBernsteinSumInitial(t,u,v):
    x = 0.
    y = 0.
    z = 0.
    
    # do a sum from i=0 to n, where n is the number of spaces between FFD points (so if there are n xFFD points then
    # there are n-1 spaces).
    n = CONST_nXFDD - 1
    m = CONST_nYFDD - 1
    l = CONST_nZFDD - 1
    
    for i in range(CONST_nXFDD):
        for j in range(CONST_nYFDD):
            for k in range(CONST_nZFDD):
                x = x + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getXInitial()
                y = y + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getYInitial()
                z = z + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getZInitial()
    return (x,y,z)


def computeXYZfromBernsteinSum(t,u,v):
    x = 0.
    y = 0.
    z = 0.
    
    # do a sum from i=0 to n, where n is the number of spaces between FFD points (so if there are n xFFD points then
    # there are n-1 spaces).
    n = CONST_nXFDD - 1
    m = CONST_nYFDD - 1
    l = CONST_nZFDD - 1

    for i in range(CONST_nXFDD):
        for j in range(CONST_nYFDD):
            for k in range(CONST_nZFDD):
                x = x + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getX()
                y = y + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getY()
                z = z + B(i, n, t) * B(j, m, u) * B(k, l, v) * FFDPointArray[i][j][k].getZ()
    return (x,y,z)

def LoadData():
    # read the data from the file and fill the data structures
    initializeDataFromFile()
    fillFinalData()
    fillInitialArrays(solidBoundaryPointArray, FFDPointArray, xsolidInitial,
                      ysolidInitial, zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial)




def fillFinalData():
    
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
        
        #Fill the final data
        FFDPointArray[I][J][K].setX(X)
        FFDPointArray[I][J][K].setY(Y)
        FFDPointArray[I][J][K].setZ(Z)

    solidBoundaryPointArrayIndex = 0
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
        
        #fill the final data
        solidBndElement = solidBoundaryPointArray[solidBoundaryPointArrayIndex]
        solidBndElement.setX(Xsolid)
        solidBndElement.setY(Ysolid)
        solidBndElement.setZ(Zsolid)
        solidBoundaryPointArrayIndex = solidBoundaryPointArrayIndex + 1

    fileData.close()


#For reading the data into the data structures from the file
def initializeDataFromFile():
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


def fillInitialArrays(solidBoundaryPointArray, FFDPointArray, xsolidInitial,
                      ysolidInitial, zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial):
    # filling the object's solid point arrays
    for element in solidBoundaryPointArray:
        xsolidInitial.append(element.getXInitial())
        ysolidInitial.append(element.getYInitial())
        zsolidInitial.append(element.getZInitial())
    
    # filling the FFD arrays
    for i in range(CONST_nXFDD):
        for j in range(CONST_nYFDD):
            for k in range(CONST_nZFDD):
                element = FFDPointArray[i][j][k]
                xFFDInitial.append(element.getXInitial())
                yFFDInitial.append(element.getYInitial())
                zFFDInitial.append(element.getZInitial())



def plotFFDPointsAndPlane(xsolidInitial,ysolidInitial, zsolidInitial, xFFDInitial, yFFDInitial, zFFDInitial,
                          LeadingEdgeX, LeadingEdgeY, LeadingEdgeZ):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    
    
    # Axes3D.plot_wireframe(ax, z, x, y)
    ax.set_xlabel('Z axis')
    ax.set_ylabel('X axis')
    ax.set_zlabel('Y axis')
    
    Axes3D.scatter(ax, zsolidInitial, xsolidInitial, ysolidInitial, s=10, c='y')
    #Axes3D.plot_wireframe(ax, zsolidInitial, xsolidInitial, ysolidInitial, rstride = 1, cstride = 1, color='y')
    #Axes3D.scatter(ax, LeadingEdgeZ, LeadingEdgeX, LeadingEdgeY, s=10, c='y')
    #Axes3D.plot_wireframe(ax, zSolid, xSolid, ySolid, rstride = 1, cstride = 1, color="b")
    Axes3D.scatter(ax, zFFDInitial, xFFDInitial, yFFDInitial, s=10, c='r')
    
    LeadingEdgeX1 = []
    LeadingEdgeY1 = []
    LeadingEdgeZ1 = []
    
    #Find all the leading edge points:
    for element in solidBoundaryPointArray:
        if((element.getLabel() == "LeadingEdge") or (element.getLabel() == "TrailingEdge")
           or (element.getLabel() == "WingTip") or (element.getLabel() == "WingRoot")):
            LeadingEdgeX1.append(element.getX())
            LeadingEdgeY1.append(element.getY())
            LeadingEdgeZ1.append(element.getZ())
    #Axes3D.scatter(ax, LeadingEdgeZ1, LeadingEdgeX1, LeadingEdgeY1, s=100, c='r')
    
    #plot the deformed wing and FFD point data
    
    xDeformed = []
    yDeformed = []
    zDeformed = []
    
    xFFDDeformed = []
    yFFDDeformed = []
    zFFDDeformed = []
    
    for element in solidBoundaryPointArray:
        xDeformed.append(element.getX())
        yDeformed.append(element.getY())
        zDeformed.append(element.getZ())
    
    # filling the FFD arrays
    for i in range(CONST_nXFDD):
        for j in range(CONST_nYFDD):
            for k in range(CONST_nZFDD):
                element = FFDPointArray[i][j][k]
                xFFDDeformed.append(element.getX())
                yFFDDeformed.append(element.getY())
                zFFDDeformed.append(element.getZ())
    """
        Axes3D.plot_wireframe(ax, zDeformed, xDeformed, yDeformed, rstride =1, cstride = 1, color='b')
        """
    Axes3D.scatter(ax, zDeformed, xDeformed, yDeformed, s=10, color='b')
    Axes3D.scatter(ax, zFFDDeformed, xFFDDeformed, yFFDDeformed, s=10, c='g')
    
    #takes a value for t,u,v. When a value is set to -1, then that parameter is free
    # to change while the one that is not set to -1 is the plane that needs to be drawn
    tuplesArray = plotIsoparametricLine(-1,-1,-1)
    xArray = []
    yArray = []
    zArray = []

    for a in tuplesArray:
        xArray.append(a[0])
        yArray.append(a[1])
        zArray.append(a[2])

    tuplesArray2 = plotIsoparametricLine(-1,-1,-1)
    xArray2 = []
    yArray2 = []
    zArray2 = []
    
    for a in tuplesArray2:
        xArray2.append(a[0])
        yArray2.append(a[1])
        zArray2.append(a[2])

    
    #Axes3D.scatter(ax, zArray, xArray, yArray, s=10, c='y')
    Axes3D.plot_wireframe(ax, zArray, xArray, yArray, rstride=1, cstride =1, color='g')
    Axes3D.plot_wireframe(ax, zArray2, xArray2, yArray2, rstride=1, cstride =1, color='g')

    #Axes3D.set_ylim(ax, [-0.5,4.5])
    #Axes3D.set_xlim(ax, [-0.5,4.5])
    Axes3D.set_zlim(ax, [-0.9, 0.9])
    plt.show(block=True)


def plotIsoparametricLine(t,u,v):

    numU = 80
    numV = 80
    numT = 80
    pointsArray = []
    if(t>=0):
        #Need to plot a constant t line
        uMin = 0.0
        vMin = 0.0

        du = (1. - 0.0)/float(numU-1)
        dv = (1. - 0.0)/float(numV-1)
        
        for i in range(numU):
            print "i: " + str(i)
            for j in range(numV):
                u = uMin + i*du
                v = vMin + j*dv
                point = computeXYZfromBernsteinSumInitial(t,u,v)
                pointsArray.append(point)
    elif(u>=0):
        #Need to plot a constant u line
        tMin = 0.0
        vMin = 0.0
        
        dt = (1. - 0.0)/float(numT-1)
        dv = (1. - 0.0)/float(numV-1)
        
        for i in range(numT):
            print "i: " + str(i)
            for j in range(numV):
                t = tMin + i*dt
                v = vMin + j*dv
                point = computeXYZfromBernsteinSumInitial(t,u,v)
                pointsArray.append(point)
    elif(v>=0):
        #Need to plot a constant v line
        tMin = 0.0
        uMin = 0.0
        
        dt = (1. - 0.0)/float(numT-1)
        du = (1. - 0.0)/float(numU-1)
        
        for i in range(numT):
            print "i: " + str(i)
            for j in range(numU):
                t = tMin + i*dt
                u = uMin + j*du
                point = computeXYZfromBernsteinSumInitial(t,u,v)
                pointsArray.append(point)


    pointsArray.sort(key=lambda x: x[2], reverse=True)
    return pointsArray


def PrintDataWithLabels():
    f = open(CONST_SORTEDFILE, "w")
    for element in solidBoundaryPointArray:
        f.write( str(element.getX()) + ", " + str(element.getY()) + ", " + str(element.getZ()) + ", " + str(element.getLabelInteger()) + "\n")







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


#THE GLOBAL VARIABLES:
# They are going to be the FFD Points, the solid boundary point array and the
# arrays holding the x,y,z values of the plane and FFD Points

#create arrays that will hold the initial object's solid points and the initial FFD points
# For the initial solid boundary points
xsolidInitial = []
ysolidInitial = []
zsolidInitial = []

# For the initial FFD Points
xFFDInitial = []
yFFDInitial = []
zFFDInitial = []



AnalyzeData()

for element in solidBoundaryPointArray:
    if(element.getT()>1 or element.getU()>1 or element.getV()>1 or element.getT() < 0 or element.getU() < 0 or element.getV() < 0):
        print "T, U, V: " + str(element.getT()) + "  " + str(element.getU()) + "  " + str(element.getV())
        print "x, y ,z: " + str(element.getX()) + "  " + str(element.getY()) + "  " + str(element.getZ())

#PrintDataWithLabels()









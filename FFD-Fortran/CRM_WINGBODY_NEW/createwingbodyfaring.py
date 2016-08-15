import numpy
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import math

"""
The file that is used for creating the speerate parts of the plane
and outputing the data so that the fortran code can read it and
place the FFD box.

The limits:

Faring:
     870 <= x <= 1700
     0   <= y <= 210
    -20  <= z <= 180

Fuselage Top:
    870 <= x <= 1700
    210 < y <= 1000
    -20 <= z <= 180

Fuselage Front:
    -1000 <= x < 870
    0 <= y <= 1000
    -20 <= z <= 180
    
Fuselage Back:
    1700 <= x <= 4000
    0 <= y <= 1000
    -20 <= z <= 180

Wing:
    0 <= x <= 4000
    0 <= y <= 1000
    180 <= z <= 3000

"""

CONST_Files = ["sb.0.dat", "sb.1.dat"]

CONST_FileType = "fuselage"

def inFaring(point):
    x = point[0]
    y = point[1]
    z = point[2]
    
    if(x<= 1700 and x >= 870):
        if (y<= 210 and y>= 0):
            if(z<= 180 and z>= -20):
                return True


def inFuselageTop(point):
    x = point[0]
    y = point[1]
    z = point[2]
    
    if(x<= 1700 and x >= 870):
        if (y<= 1000 and y> 210):
            if(z<= 180 and z>= -20):
                return True

def inFuselageFront(point):
    x = point[0]
    y = point[1]
    z = point[2]
    
    if(x< 870 and x >= -1000):
        if (y<= 1000 and y> 0):
            if(z<= 180 and z>= -20):
                return True


def inFuselageBack(point):
    x = point[0]
    y = point[1]
    z = point[2]
    
    if(x< 4000 and x > 1700):
        if (y<= 1000 and y> 0):
            if(z<= 180 and z>= -20):
                return True

def inWing(point):
    x = point[0]
    y = point[1]
    z = point[2]
    
    if(x< 4000 and x > 0):
        if (y<= 1000 and y> 0):
            if(z<= 3000 and z> 180):
                return True


# Read all the files and place all the points into
# the big plane points array
def parseData(planePoints):
    for fileString in CONST_Files:
        fileInput = open(fileString, 'r')
        while(True):
            line = fileInput.readline()
            if(line == ""):
                break
            lineStringList = line.split()
            numArray = []
            for string in lineStringList:
                numArray.append(float(string))
            planePoints.append(numArray)
        fileInput.close()

def plotData(tuplesArray,fig,ax,c1):
    
    x = []
    y = []
    z = []
    
    for point in tuplesArray:
        x.append(point[0])
        y.append(point[1])
        z.append(point[2])
    
    
    if (CONST_FileType == "fuselage"):
        Axes3D.scatter(ax, z,x,y, s=30, c =c1)
        
        Axes3D.set_zlim(ax, [0, 1000])
        Axes3D.set_ylim(ax, [0, 3000])
        Axes3D.set_xlim(ax, [0, 3000])
        
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    else:
        Axes3D.scatter(ax, z,x,y, s=30, c =c1)
        
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    
def printPoints(PlaneCollection):
    for i in range(len(PlaneCollection)):
        Part = PlaneCollection[i]
        fileString = "sepsb." + str(i) + ".dat"
        file = open(fileString, 'w')
        for point in Part:
            lineString = ""
            for coord in point:
                lineString = lineString + str(coord) + " "
            file.write(lineString + "\n")
        
        file.close()



def main():
    planePoints = []
    parseData(planePoints)
    
    # The seperate plane components
    FuselageTop = []
    FuselageFront = []
    FuselageBack = []
    Faring = []
    Wing = []
    
    for point in planePoints:
        if(inFaring(point)):
            Faring.append(point)
        if(inFuselageTop(point)):
            FuselageTop.append(point)
        if(inFuselageFront(point)):
            FuselageFront.append(point)
        if(inFuselageBack(point)):
            FuselageBack.append(point)
        if(inWing(point)):
            Wing.append(point)

#print "total Points: " + str(len(planePoints))

    totalSeperate = len(FuselageTop) + len(FuselageFront) \
        + len(FuselageBack) + len(Faring) + len(Wing)

#print "total seperate: " + str(totalSeperate)


    # print all the points out into seperate
    # files

    PlaneCollection = [Faring, Wing, FuselageTop, \
                       FuselageFront, FuselageBack]

    printPoints(PlaneCollection)


    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')

    plotData(FuselageTop, fig,ax, 'b')
    plotData(FuselageFront, fig,ax, 'g')
    plotData(FuselageBack, fig,ax, 'r')
    plotData(Faring, fig,ax, '0.75')
    plotData(Wing, fig,ax, 'y')



    plt.show(block=True)


main()





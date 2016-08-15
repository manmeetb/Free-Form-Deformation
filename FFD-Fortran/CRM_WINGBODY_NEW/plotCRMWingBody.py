import numpy
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import math

# the file used for plotting the data
CONST_InputFile1 = "sb.1.dat"
CONST_InputFile2 = "sb.0.dat"

def plotData(tuplesArray):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    
    
    x = []
    y = []
    z = []
    
    for point in tuplesArray:
        x.append(point[0])
        y.append(point[1])
        z.append(point[2])
    

    if (FILEInQuestion == "newsbp.fuselageZ.dat"):
        Axes3D.scatter(ax, x,y,z, s=30, c ='b')
        
        Axes3D.set_zlim(ax, [0, 1000])
        Axes3D.set_ylim(ax, [0, 3000])
        Axes3D.set_xlim(ax, [0, 3000])
        
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    else:
        Axes3D.scatter(ax, z,x,y, s=30, c ='b')
        
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')

    plt.show(block=True)

def plotData2files(tuplesArray1, tuplesArray2):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    
    
    x1 = []
    y1 = []
    z1 = []
    
    for point in tuplesArray1:
        x1.append(point[0])
        y1.append(point[1])
        z1.append(point[2])
    
    x2 = []
    y2 = []
    z2 = []
    
    for point in tuplesArray2:
        x2.append(point[0])
        y2.append(point[1])
        z2.append(point[2])
    
    Axes3D.scatter(ax, x1,y1,z1, s=30, c ='b')
    Axes3D.scatter(ax, x2,y2,z2, s=30, c ='r')

    Axes3D.set_xlim(ax, [0, 2000])
    Axes3D.set_ylim(ax, [0, 3000])
    Axes3D.set_zlim(ax, [0, 1000])

    plt.show(block=True)


def plotOneFile(file):
    fileInput = open(file, 'r')
    planePoints = []
    while(True):
        line = fileInput.readline()
        if(line == ""):
            break
        planePoints.append(ParseLine(line))
    fileInput.close()

    plotData(planePoints)

def plotTwoFiles(file1, file2):
    fileInput1 = open(file1, 'r')
    planePoints1 = []
    while(True):
        line = fileInput1.readline()
        if(line == ""):
            break
        lineStringList = line.split()
        numArray = []
        for string in lineStringList:
            numArray.append(float(string))
        planePoints1.append(numArray)
    fileInput1.close()

    fileInput2 = open(file2, 'r')
    planePoints2 = []
    while(True):
        line = fileInput2.readline()
        if(line == ""):
            break
        lineStringList = line.split()
        numArray = []
        for string in lineStringList:
            numArray.append(float(string))
        planePoints2.append(numArray)
    fileInput2.close()

    plotData2files(planePoints1, planePoints2)



#plotData(planePoints)
#FILEInQuestion = CONST_InputFile9
#plotOneFile(FILEInQuestion)
plotTwoFiles(CONST_InputFile1, CONST_InputFile2)









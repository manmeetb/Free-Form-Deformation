import numpy
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import math

# the file used for plotting the data
CONST_InputFile = "sbp.bothwingfuselage.dat"
CONST_InputFile1 = "sbp.wing.dat"
CONST_InputFile2 = "sbp.fuselage.dat"
CONST_InputFile3 = "sbp.fuselageMirror.dat"
CONST_InputFile4 = "sbp.bothwingfuselageMirror.dat"
CONST_InputFile5 = "sbp.fuselageZ.dat"
CONST_InputFile6 = "sbp.fuselageMirrorZ.dat"
CONST_InputFile7 = "newsbp.wing.dat"
CONST_InputFile8 = "newsbp.fuselage.dat"
CONST_InputFile9 = "../DataFiles/crmwingdesignSBP.dat"

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
    

    if (FILEInQuestion == "sbp.fuselage.dat"):
        Axes3D.scatter(ax, z,x,y, s=30, c ='b')
        Axes3D.set_xlim(ax, [0, 4])
        Axes3D.set_ylim(ax, [0, 12])
        Axes3D.set_zlim(ax, [0, 4])
        
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    elif (FILEInQuestion == "sbp.bothwingfuselage.dat"):
        Axes3D.scatter(ax, z,x,y, s=30, c ='b')
        Axes3D.set_xlim(ax, [0, 8])
        Axes3D.set_ylim(ax, [-2, 6])
        Axes3D.set_zlim(ax, [0, 5])
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    elif (FILEInQuestion == "sbp.fuselageZ.dat"):
        Axes3D.scatter(ax, z,x,y, s=30, c ='b')
        Axes3D.set_zlim(ax, [0, 4])
        Axes3D.set_ylim(ax, [0, 4])
        Axes3D.set_xlim(ax, [0, 12])
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    elif (FILEInQuestion == "sbp.bothwingfuselageMirror.dat"):
        Axes3D.scatter(ax, z,x,y, s=30, c ='b')
        Axes3D.set_xlim(ax, [-8, 8])
        Axes3D.set_ylim(ax, [-2, 6])
        Axes3D.set_zlim(ax, [0, 5])
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    elif (FILEInQuestion == "sbp.fuselageMirrorZ.dat"):
        Axes3D.scatter(ax, z,x,y, s=30, c ='b')
        Axes3D.set_zlim(ax, [0, 4])
        Axes3D.set_ylim(ax, [-3, 3])
        Axes3D.set_xlim(ax, [0, 12])
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    elif (FILEInQuestion == "sbp.fuselageMirror.dat"):
        
        Axes3D.scatter(ax, z,x,y, s=30, c ='b')
        Axes3D.set_xlim(ax, [-3, 3])
        Axes3D.set_ylim(ax, [-2, 6])
        Axes3D.set_zlim(ax, [0, 5])
        
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    elif (FILEInQuestion == "sbp.wing.dat"):
        Axes3D.scatter(ax, z,x,y, s=30, c ='b')
        
        Axes3D.set_xlim(ax, [0, 8])
        Axes3D.set_ylim(ax, [0, 12])
        Axes3D.set_zlim(ax, [-2, 3])
        
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    elif (FILEInQuestion == "newsbp.SOME.dat"):
        Axes3D.scatter(ax, z,x,y, s=30, c ='b')
        
        Axes3D.set_xlim(ax, [0, 1000])
        Axes3D.set_ylim(ax, [800, 2000])
        Axes3D.set_zlim(ax, [120, 270])
        
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    elif (FILEInQuestion == "newsbp.fuselage.dat"):
        Axes3D.scatter(ax, x,y,z, s=30, c ='b')
        
        Axes3D.set_xlim(ax, [0, 1000])
        Axes3D.set_ylim(ax, [0, 3000])
        Axes3D.set_zlim(ax, [0, 1000])
        
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    elif (FILEInQuestion == "newsbp.fuselageZ.dat"):
        Axes3D.scatter(ax, x,y,z, s=30, c ='b')
        
        Axes3D.set_zlim(ax, [0, 1000])
        Axes3D.set_ylim(ax, [0, 3000])
        Axes3D.set_xlim(ax, [0, 3000])
        
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
    else:
        Axes3D.scatter(ax, z,x,y, s=30, c ='b')
        
        Axes3D.set_xlim(ax, [0, 8])
        Axes3D.set_ylim(ax, [0, 12])
        Axes3D.set_zlim(ax, [-2, 3])
        
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
        planePoints1.append(ParseLine(line))
    fileInput1.close()

    fileInput2 = open(file2, 'r')
    planePoints2 = []
    while(True):
        line = fileInput2.readline()
        if(line == ""):
            break
        planePoints2.append(ParseLine(line))
    fileInput2.close()

    plotData2files(planePoints1, planePoints2)



#plotData(planePoints)
FILEInQuestion = CONST_InputFile9
plotOneFile(FILEInQuestion)
#plotTwoFiles(CONST_InputFile7, CONST_InputFile8)









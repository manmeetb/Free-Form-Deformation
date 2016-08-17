
import numpy
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import math



"""
    This file will be able to plot the data from the FFD processing 
    algorithm when multiple elements are involved
"""
CONST_DATAFILEInitial = "FPD_CRMWingBodyInitialAug16.txt"
CONST_DATAFILEFinal = "FPD_CRMWingBodyFinalAug16.txt"



CONST_nXFDD = 10
CONST_nYFDD = 3
CONST_nZFDD = 8

#CONST_ElementTag = "WingLess"
CONST_ElementTag = ""

# The method used for loading the data from the files
# into the data structures
def loadData(ElementsSbpData, ElementsFFDData):
    file = open(CONST_DATAFILEInitial, 'r')
    
    #the first line in the file tells us about
    # the number of elements in question
    
    line = file.readline()
    numElementsList = line.split()
    CONST_NumElements = int(numElementsList[len(numElementsList)-1])

    
    # now that we know how many elements there are,
    # we are going to create seperate lists withing the
    # elements list for each element
    for i in range(CONST_NumElements):
        rowArray1 = []
        rowArray2 = []
        ElementsSbpData.append(rowArray1)
        ElementsFFDData.append(rowArray2)

    # read the file now and fill all the data into lists and tuples and then
    # into the big lists
    for i in range(CONST_NumElements):
        lineElementNumber = file.readline() # the element number
        
            #The FFD points:
        lineFFDHeader = file.readline() # the title for the FFD points
        FFDHeaderList = lineFFDHeader.split()
        #the number of ffd points
        numFFDPoints = int(FFDHeaderList[len(FFDHeaderList)-1])
        
        #read the FFD data
        for j in range(numFFDPoints):
            lineIndex = file.readline()
            lineCoordinate = file.readline()
            
            IndexListString = lineIndex.split()
            CoordinatesListString = lineCoordinate.split()
            
            #convert the lists into floats
            IndexListFloat = []
            CoordinatesListFloat = []
            
            for str in IndexListString:
                IndexListFloat.append(float(str))
            for str in CoordinatesListString:
                CoordinatesListFloat.append(float(str))
            
            
            #store the index and point data which are in lists
            # into a tuple
            FFDData = (IndexListFloat, CoordinatesListFloat)
            ElementsFFDData[i].append(FFDData) #add the tuple data
        
            # The Solid Body Points
        lineSBPHeader = file.readline()
        SBPHeaderList = lineSBPHeader.split()
        # The number of solid body points
        numSBP = int(SBPHeaderList[len(SBPHeaderList)-1])
        for k in range(numSBP):
            lineXYZ = file.readline()
            lineTUV = file.readline()
            
            XYZListString = lineXYZ.split()
            TUVListString = lineTUV.split()
            
            #convert the lists into floats
            XYZListFloat = []
            TUVListFloat = []
            
            for str in XYZListString:
                XYZListFloat.append(float(str))
            for str in TUVListString:
                TUVListFloat.append(float(str))
            

            
            #store the index and point data which are in lists
            # into a tuple
            SBPData = (XYZListFloat, TUVListFloat)
            ElementsSbpData[i].append(SBPData) #add the tuple data

    return CONST_NumElements

# The method used for loading the data from the files
# into the data structures
def loadDataFinal(ElementsSbpData, ElementsFFDData):
    file = open(CONST_DATAFILEFinal, 'r')
    
    #the first line in the file tells us about
    # the number of elements in question
    
    line = file.readline()
    numElementsList = line.split()
    CONST_NumElements = int(numElementsList[len(numElementsList)-1])
    
    
    # now that we know how many elements there are,
    # we are going to create seperate lists withing the
    # elements list for each element
    for i in range(CONST_NumElements):
        rowArray1 = []
        rowArray2 = []
        ElementsSbpData.append(rowArray1)
        ElementsFFDData.append(rowArray2)

    # read the file now and fill all the data into lists and tuples and then
    # into the big lists
    for i in range(CONST_NumElements):
        lineElementNumber = file.readline() # the element number
        
        #The FFD points:
        lineFFDHeader = file.readline() # the title for the FFD points
        FFDHeaderList = lineFFDHeader.split()
        #the number of ffd points
        numFFDPoints = int(FFDHeaderList[len(FFDHeaderList)-1])
        
        #read the FFD data
        for j in range(numFFDPoints):
            lineIndex = file.readline()
            lineCoordinate = file.readline()
            
            IndexListString = lineIndex.split()
            CoordinatesListString = lineCoordinate.split()
            
            #convert the lists into floats
            IndexListFloat = []
            CoordinatesListFloat = []
            
            for string in IndexListString:
                IndexListFloat.append(float(string))
            for string in CoordinatesListString:
                CoordinatesListFloat.append(float(string))
            
            
            #store the index and point data which are in lists
            # into a tuple
            FFDData = (IndexListFloat, CoordinatesListFloat)
            ElementsFFDData[i].append(FFDData) #add the tuple data
    
        # The Solid Body Points
        lineSBPHeader = file.readline()
        SBPHeaderList = lineSBPHeader.split()
        # The number of solid body points
        numSBP = int(SBPHeaderList[len(SBPHeaderList)-1])
        for k in range(numSBP):
            lineXYZ = file.readline()
            lineTUV = file.readline()
            XYZListString = lineXYZ.split()
            TUVListString = lineTUV.split()
            
            #convert the lists into floats
            XYZListFloat = []
            TUVListFloat = []
            #print k
            #print XYZListString
            for string in XYZListString:
                XYZListFloat.append(float(string))
            for string in TUVListString:
                TUVListFloat.append(float(string))
            
            
            
            #store the index and point data which are in lists
            # into a tuple
            SBPData = (XYZListFloat, TUVListFloat)
            ElementsSbpData[i].append(SBPData) #add the tuple data

    return CONST_NumElements



def plotElements(fig, ax, ElementsSbpData, ElementsFFDData, NumElements,c1,c2,\
                 c3,c4):
    
    xTest = [1472.573,1486.63]
    yTest = [153.9531,167.740]
    zTest = [213.63327, 313.408]
    
    #Axes3D.scatter(ax, zTest,xTest, yTest, s=300, c='k')
    
    for i in range(NumElements):
        xSolid = []
        ySolid = []
        zSolid = []
        
        xFFD = []
        yFFD = []
        zFFD = []
        
        #fill the Solid body points


        for SBPTuple in ElementsSbpData[i]:
            xSolid.append(SBPTuple[0][0])
            ySolid.append(SBPTuple[0][1])
            zSolid.append(SBPTuple[0][2])
        
        
        for FFDTuple in ElementsFFDData[i]:
            xFFD.append(FFDTuple[1][0])
            yFFD.append(FFDTuple[1][1])
            zFFD.append(FFDTuple[1][2])

        # element 1 is the fuselage whose x and z values were switched
        # to make z cross sections. So switch these back when plotting
        
        #ax.plot_trisurf(zSolid, xSolid, ySolid, cmap=cm.jet, linewidth=0.2)
        Axes3D.scatter(ax, zSolid,xSolid, ySolid, s=20, c=c1)
        Axes3D.scatter(ax, zFFD, xFFD, yFFD, s=30, c= c2)
        #Axes3D.plot_wireframe(ax, zFFD, xFFD, yFFD)
        
        """
        if(i == 0):
            #ax.plot_trisurf(zSolid, xSolid, ySolid, cmap=cm.jet, linewidth=0.2)
            Axes3D.scatter(ax, zSolid,xSolid, ySolid, s=20, c=c1)
            Axes3D.scatter(ax, zFFD, xFFD, yFFD, s=30, c= c2)
            #Axes3D.plot_wireframe(ax, zFFD, xFFD, yFFD)
        if(i == 1):
            #ax.plot_trisurf(xSolid, zSolid, ySolid, cmap=cm.jet, linewidth=0.2)
            Axes3D.scatter(ax, zSolid,xSolid, ySolid, s=10, c=c3)
            Axes3D.scatter(ax, zFFD, xFFD, yFFD, s=30, c= c4)
            #Axes3D.plot_wireframe(ax, xFFD, zFFD, yFFD)
        """

def B(i,n,t):
    return (math.factorial(n)/(math.factorial(i)*math.factorial(n-i)))*((1-t)**(n-i))*(t**i)

def computeXYZfromBernsteinSum(t,u,v,FFDPointArray):
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
                for data in FFDPointArray:
                    
                    if(int(data[0][0])==i and int(data[0][1])==j \
                       and int(data[0][2]) == k):
                       x = x + B(i, n, t) * B(j, m, u) * B(k, l, v) * data[1][0]
                       y = y + B(i, n, t) * B(j, m, u) * B(k, l, v) * data[1][1]
                       z = z + B(i, n, t) * B(j, m, u) * B(k, l, v) * data[1][2]
    return (x,y,z)

def plotIsoparametricLine(ElementsFFDDataInit,fig,ax):
    
    i = 0
    FFDPointArray = ElementsFFDDataInit[i]
        
    print "point0: " + str(computeXYZfromBernsteinSum(0.733534455, 0.543679595, 1.24942482, FFDPointArray))
    numU = 30
    numV = 30
    numT = 30
    
    t = -1
    u = -1
    v = -1
    
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
                point = computeXYZfromBernsteinSum(t,u,v,FFDPointArray)
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
                point = computeXYZfromBernsteinSum(t,u,v,FFDPointArray)
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
                point = computeXYZfromBernsteinSum(t,u,v,FFDPointArray)
                pointsArray.append(point)


    pointsArray.sort(key=lambda x: x[2], reverse=True)
    x = []
    y = []
    z = []
    for point in pointsArray:
        x.append(point[0])
        y.append(point[1])
        z.append(point[2])

    Axes3D.scatter(ax, z,x,y, s=20, c='b')

    i = 1
    FFDPointArray = ElementsFFDDataInit[i]
   
    """
    Range:
    FFD MaxX:    1724.89734
    FFD MinX:    845.035400
    FFD MaxY:    234.982056
    FFD MinY:    65.9979019
    FFD MaxZ:    229.873810
    FFD MinZ:   -25.0000000
    
    """

    """
        x,y,z:    1477.94958       157.023026       237.454636
        t,u,v:   -9.79601443E-02  0.167819858      -3.21748948
        
        x,y,z:    1480.30420       158.571213       250.766724
        t,u,v:   0.423591226      0.589504182     -0.665205836
        
        x,y,z:    1482.83533       160.261246       265.041901
        t,u,v:   0.421875060      0.589127183     -0.672547698
        
        x,y,z:    1477.87891       156.767914       237.449570
        t,u,v:   0.425251901      0.589796484     -0.658110559
        
        x,y,z:    1480.11804       158.466248       250.763016
        t,u,v:   0.423578471      0.589478552     -0.665200531
        
        x,y,z:    1471.54419       152.690903       203.111908
        t,u,v:   0.204102591      0.466491759      -1.60756850
        
        x,y,z:    1477.51489       156.792114       237.446045
        t,u,v:   -9.79608446E-02  0.167815223      -3.21752191
        
        x,y,z:    1491.00244       166.164597       313.359436
        t,u,v:   0.205004781      0.467159122      -1.60412931
        
        x,y,z:    1475.00281       155.279877       225.069717
        t,u,v:   0.204276904      0.466621190      -1.60688579
       
    """

    print "point1: " + str(computeXYZfromBernsteinSum(0.423591226, 0.589504182, -0.665205836, FFDPointArray))


    numU = 30
    numV = 30
    numT = 30
    
    t = -1
    u = -1
    v = -1
    
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
                point = computeXYZfromBernsteinSum(t,u,v,FFDPointArray)
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
                point = computeXYZfromBernsteinSum(t,u,v,FFDPointArray)
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
                point = computeXYZfromBernsteinSum(t,u,v,FFDPointArray)
                pointsArray.append(point)


    pointsArray.sort(key=lambda x: x[2], reverse=True)
    x = []
    y = []
    z = []
    for point in pointsArray:
        x.append(point[0])
        y.append(point[1])
        z.append(point[2])

    Axes3D.scatter(ax, z,x,y, s=20, c='y')






# the main method
def main():
    # the list that will hold the solid boundary point data
    # for the elements.
    ElementsSbpDataInit = []
    
    # the list that will hold the FFD data for the elements.
    ElementsFFDDataInit = []
    
    CONST_NumElements = loadData(ElementsSbpDataInit, ElementsFFDDataInit)
    
    # the list that will hold the final solid boundary point data
    # for the elements.
    ElementsSbpDataFinal = []
    
    # the list that will hold the final FFD data for the elements.
    ElementsFFDDataFinal = []
    
    CONST_NumElements = loadDataFinal(ElementsSbpDataFinal, ElementsFFDDataFinal)
    
    
    
    # Plotting the elements
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    
    plotElements(fig, ax, ElementsSbpDataInit,ElementsFFDDataInit, \
                 CONST_NumElements, 'b', 'r', 'b', 'r')
     #plotElements(fig,ax, ElementsSbpDataFinal, ElementsFFDDataFinal, \
      #           CONST_NumElements, 'y', 'g', 'y', 'g')


    ax.set_xlabel('Z axis')
    ax.set_ylabel('X axis')
    ax.set_zlabel('Y axis')

    Axes3D.set_xlim(ax, [0, 1900])
    Axes3D.set_ylim(ax, [0, 3000])
    Axes3D.set_zlim(ax, [0, 1000])

    #Axes3D.set_xlim(ax, [-1, 8])
    #Axes3D.set_ylim(ax, [-2, 6])
    #Axes3D.set_zlim(ax, [0, 5])
    
    if (CONST_ElementTag == "WingLess"):
        # Axes3D.plot_wireframe(ax, z, x, y)
        ax.set_xlabel('Z axis')
        ax.set_ylabel('X axis')
        ax.set_zlabel('Y axis')
        
        Axes3D.set_xlim(ax, [0, 6])
        Axes3D.set_ylim(ax, [-1.5, 2.5])
        Axes3D.set_zlim(ax, [0, 2])
    #plotIsoparametricLine(ElementsFFDDataInit,fig,ax)
    plt.show(block=True)




main()






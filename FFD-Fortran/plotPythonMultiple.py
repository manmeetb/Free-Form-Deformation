
import numpy
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import math



"""
    This file will be able to plot the data from the FFD processing 
    algorithm when multiple elements are involved
"""
CONST_DATAFILEInitial = "FPD_CRMWingBodyInitialAug15_2.txt"
CONST_DATAFILEFinal = "FPD_CRMWingBodyFinalAug15_2.txt"



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



def plotElements(fig, ax, ElementsSbpData, ElementsFFDData, NumElements,c1,c2,\
                 c3,c4):
    
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
        
        if(i == 0):
            #ax.plot_trisurf(zSolid, xSolid, ySolid, cmap=cm.jet, linewidth=0.2)
            Axes3D.scatter(ax, zSolid,xSolid, ySolid, s=20, c=c1)
            Axes3D.scatter(ax, zFFD, xFFD, yFFD, s=30, c= c2)
            #Axes3D.plot_wireframe(ax, zFFD, xFFD, yFFD)
        if(i == 1):
            #ax.plot_trisurf(xSolid, zSolid, ySolid, cmap=cm.jet, linewidth=0.2)
            Axes3D.scatter(ax, zSolid,xSolid, ySolid, s=20, c=c3)
            Axes3D.scatter(ax, zFFD, xFFD, yFFD, s=30, c= c4)
            #Axes3D.plot_wireframe(ax, xFFD, zFFD, yFFD)
    


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
                 #CONST_NumElements, 'y', 'g', 'y', 'g')


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

    plt.show(block=True)




main()






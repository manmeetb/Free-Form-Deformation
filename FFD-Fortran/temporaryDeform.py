
import math



CONST_DATAFILEInitial = "FPD_CRMWingBodyInitialXZrestored.txt"
CONST_DATAFILEFinal = "FPD_CRMWingBodyFinalXZrestored.txt"


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



# apply a temporary deformation to the plane
def temporaryDeform(ElementsSbpDataInit):
    # move the y value up for all x points smaller than 1250 linearly
    
    # only deform the second element:
    for i in range(len(ElementsSbpDataInit[1])):
        SBPTuple = ElementsSbpDataInit[1][i]
        xSolid = SBPTuple[0][0]
        ySolid = SBPTuple[0][1]
        zSolid= SBPTuple[0][2]
    
        t = SBPTuple[1][0]
        u = SBPTuple[1][1]
        v = SBPTuple[1][2]
    
        if(xSolid <= 1250):
            dx = (1250.0-xSolid)
            dy = math.tan(3.1415/12)*dx
            ySolid = ySolid + dy
            
            newXYZ = [xSolid, ySolid, zSolid]
            newTUV = [t,u,v]
            newTuple = (newXYZ, newTUV)
            ElementsSbpDataInit[1][i] = newTuple


def printFinalData(ElementsSbpDataInit,ElementsFFDDataInit, CONST_NumElements):
    file = open(CONST_DATAFILEFinal, 'w')
    file.write("NumElements: " + str(CONST_NumElements) + "\n")

    for i in range(CONST_NumElements):
        file.write("Element: " + str(i+1) + "\n")
        file.write("FFD Points " + str(len(ElementsFFDDataInit[i])) + "\n")
        
        for FFDTuple in ElementsFFDDataInit[i]:
            file.write(str(FFDTuple[0][0]) + " " + str(FFDTuple[0][1]) + \
                       " " + str(FFDTuple[0][2]) + "\n")
            file.write(str(FFDTuple[1][0]) + " " + str(FFDTuple[1][1]) + \
                        " " + str(FFDTuple[1][2]) + "\n")
        
        
        file.write("SolidBoundaryPointData " + str(len(ElementsSbpDataInit[i])) + "\n")
        for SBPTuple in ElementsSbpDataInit[i]:
            file.write(str(SBPTuple[0][0]) + " " + str(SBPTuple[0][1]) + \
                       " " + str(SBPTuple[0][2]) + "\n")
            file.write(str(SBPTuple[1][0]) + " " + str(SBPTuple[1][1]) + \
                       " " + str(SBPTuple[1][2]) + "\n")

    file.close()



def main():
    # the list that will hold the solid boundary point data
    # for the elements.
    ElementsSbpDataInit = []
    
    # the list that will hold the FFD data for the elements.
    ElementsFFDDataInit = []
    
    CONST_NumElements = loadData(ElementsSbpDataInit, ElementsFFDDataInit)

    temporaryDeform(ElementsSbpDataInit)

    printFinalData(ElementsSbpDataInit, ElementsFFDDataInit, CONST_NumElements)



main()






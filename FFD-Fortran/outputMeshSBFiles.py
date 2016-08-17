
# This file reads in the final fortran output and it creates the
# sbfinal point files that are needed by the mesh file to create
# the deformed mesh file


CONST_DATAFILEFinal = "FPD_CRMWingBodyFinalAug16.txt"


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


# the list that will hold the final solid boundary point data
# for the elements.
ElementsSbpDataFinal = []
    
# the list that will hold the final FFD data for the elements.
ElementsFFDDataFinal = []

loadDataFinal(ElementsSbpDataFinal, ElementsFFDDataFinal)

# Now, split all the final points up into the two different
# solid point types:

SP1 = []
SP2 = []

for Part in ElementsSbpDataFinal:
    for dataTuple in Part:
        if(int(dataTuple[0][7]) == 1):
            SP1.append(dataTuple[0])
        if(int(dataTuple[0][7]) == 2):
            SP2.append(dataTuple[0])

# Sort the SP arrays according to their index

SP1.sort(key=lambda data: data[8])
SP2.sort(key=lambda data: data[8])
SP = [SP1,SP2]

# output the final files
for i in range(len(SP)):
    fileString = "sb." + str(i) + "Final.dat"
    file = open(fileString, 'w')
    for point in SP[i]:
        string = ""
        for k in range(3):
            string = string + str(point[k]) + " "
        string = string + "\n"
        file.write(string)
    file.close()












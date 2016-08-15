
"""
A file used for placing more than one element into a file. So,
for instance the CRM wing and fuselage can be supplied as two files
and then these are printed into one file. The printing format is the 
following.

- Num Elements
- element with maximum number of points (for fortran when creating arrays)
- Num Points Element 1
    (list points)
- Num Points Element 2
    (list points)
...

Moreover, a runcard file will also be used which will
have all the information for each element. This information
will include the dimension of FFD points, the direction
along which to place the cross sections, etc ...

"""

CONST_NumElements = 2
CONST_FileStringsList = ["sb.0.dat", "sb.1.dat"]

CONST_OutputFile = "INPUT_CRMWINGBODYUn.txt"


# the function used for reading in the data from each
# file and loading it into the list
def loadData(ElementsSolidBodyPoints):
    
    for i in range(len(CONST_FileStringsList)):
        file = open(CONST_FileStringsList[i], 'r')
        
        while(True):
            line = file.readline()
            if (line == ""):
                break
            lineString = line.split()
            numList = []
            for string in lineString:
                numList.append(float(string))
            
            ElementsSolidBodyPoints[i].append(numList)

        file.close()


# The function that is used for outputing the data
# into the output file in the way specified at the beginning
# of the file
def outputData(ElementsSolidBodyPoints):
    
    outputFile = open(CONST_OutputFile, 'w')
    # write the number of elements
    outputFile.write(str(CONST_NumElements) + "\n")
    
    maxPts = 0

    #find the maximum number of points
    for i in range(CONST_NumElements):
        if (len(ElementsSolidBodyPoints[i]) > maxPts):
            maxPts = len(ElementsSolidBodyPoints[i])

    outputFile.write(str(maxPts) + "\n")
    
    #print the data for each element
    for i in range(CONST_NumElements):
        outputFile.write(str(len(ElementsSolidBodyPoints[i])) + "\n")
        for point in ElementsSolidBodyPoints[i]:
            outputFile.write(str(point[0]) + ", " + str(point[1]) + ", " + str(point[2]) + ", " + str(0) + "\n")
    
    outputFile.close()

def main():
    
    # A 2D list that will hold all the solid body points
    ElementsSolidBodyPoints = []
    
    #initialize the data structure. Here, make the number of rows
    # for the List which corresponds to the number of elements
    
    for i in range(len(CONST_FileStringsList)):
        rowArray = []
        ElementsSolidBodyPoints.append(rowArray)

    loadData(ElementsSolidBodyPoints)
    outputData(ElementsSolidBodyPoints)


main()



















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
CONST_FileStringsList = ["sortednewsbp.wing.dat", "sortednewsbp.fuselageZ.dat"]

CONST_OutputFile = "newCRMWingFuselageCaseZ.txt"


#The methods used to parse a string of 4 numbers with commas
#in between them.
def ParseLine(line):
    x = 0
    y = 0
    z = 0
    label = 0
    
    for i in range(4):
        Comma = line.find(",")
        if (Comma == -1):
            # on last number
            Num = line[0: len(line)]
            label = float(Num)
            break
        Num = line[0:Comma]
        line = line[Comma + 1:len(line)]
        if (i == 0):
            x = float(Num)
        elif (i == 1):
            y = float(Num)
        elif (i ==2):
            z = float(Num)

    return (x,y,z,label)


# the function used for reading in the data from each
# file and loading it into the list
def loadData(ElementsSolidBodyPoints):
    
    for i in range(len(CONST_FileStringsList)):
        file = open(CONST_FileStringsList[i], 'r')
        
        while(True):
            line = file.readline()
            if (line == ""):
                break
            ElementsSolidBodyPoints[i].append(ParseLine(line))

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
            outputFile.write(str(point[0]) + ", " + str(point[1]) + ", " + str(point[2]) + ", " + str(point[3]) + "\n")
    
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


















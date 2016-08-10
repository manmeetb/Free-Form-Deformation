

# this file is for reading the CRM-wingbody case files
# and removing the 4th number as well as printing everything
# out with commas
CONST_fileName = "newsbp.2.dat"   #input file
CONST_outputFile = "newsbp.fuselageZ.dat"


def ParseLine(line):
    x = 0
    y = 0
    z = 0
    a = 0
    
    numArray = line.split()
    
    x = float(numArray[0])
    y = float(numArray[1])
    z = float(numArray[2])
    
    return (x,y,z)


file = open(CONST_fileName, 'r')
lineNumPoints  = file.readline()


pointsArray = [] #The array holding the point tuples

while(True):
    line = file.readline()
    
    #end of file reached
    if(line == ""):
        break

    pointsArray.append(ParseLine(line))

file.close()

# print the points into the xyz file
fileOutput = open(CONST_outputFile, 'w')

for point in pointsArray:
    fileOutput.write(str(point[2]) + ", " + str(point[1]) + ", " + \
                     str(point[0]) + "\n")

fileOutput.close()










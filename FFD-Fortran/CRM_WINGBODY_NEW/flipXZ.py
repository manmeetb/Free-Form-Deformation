
"""
Reads all the point data and flips the x and z values so that
the fortran code can place the lattice around the object
"""

# the fuselage file
CONST_File = "sb.1.dat"

CONST_OutputFile = "sb.1XZ.dat"

file = open(CONST_File, 'r')


SolidPts = []
while(True):
    line = file.readline()
    if(line == ""):
        break
    lineStringList = line.split()
    numArray = []
    for string in lineStringList:
        numArray.append(float(string))
    SolidPts.append(numArray)

file.close()

# print all the points out with the x and z flipped
fileOutput = open(CONST_OutputFile, 'w')
for pt in SolidPts:
    string = str(pt[2]) + " " + str(pt[1]) + " " + str(pt[0])
    fileOutput.write(string + "\n")

fileOutput.close()






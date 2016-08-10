
CONST_UNSORTEDFILE = "FPD_crmdesignInitSPB.txt"
#CONST_SORTEDFILE = "sortedZPlanePointsCRM.txt"
CONST_SORTEDFILELABEL = "FPD_crmdesignInitSPBSort.txt"

CONST_nXFDD = 10
CONST_nYFDD = 3
CONST_nZFDD = 8


                    #Main Method
solidBoundaryPointArray = []

file = open(CONST_UNSORTEDFILE, 'r')
file.readline()

while(True):
    lineXYZ = file.readline()
    if (lineXYZ == ""):
        break
    lineTUV = file.readline()

    stringLineXYZ = lineXYZ.split()
    numList = []
    for string in stringLineXYZ:
        numList.append(float(string))

    solidBoundaryPointArray.append(numList)

file.close()

#sort the data in terms of the z values
solidBoundaryPointArray.sort(key=lambda x: x[2])

fileOutput = open(CONST_SORTEDFILELABEL, 'w')
for pt in solidBoundaryPointArray:
    string = ""
    for ptDigit in pt:
        string = string + str(ptDigit) + " "
    fileOutput.write(string + "\n")

print len(solidBoundaryPointArray)
fileOutput.close()









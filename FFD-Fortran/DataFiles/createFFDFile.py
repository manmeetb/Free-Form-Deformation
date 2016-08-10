



# read the file

CONST_Filename = "sortedZPlanePointsLblsCRM.txt"
CONST_OutputFile = "INPUT_CrmWingLessPts.txt"
file = open(CONST_Filename, 'r')

numList = []
for line in file:
    numArray = line.split()
    numList.append(numArray)
file.close()


fileOutput = open(CONST_OutputFile, 'w')

fileOutput.write(str(1) + "\n")
fileOutput.write(str(len(numList)) + "\n")
fileOutput.write(str(len(numList)) + "\n")
for point in numList:
    stringOuput = ""
    for str in point:
        stringOuput = stringOuput + str + " "
    stringOuput = stringOuput + "\n"
    fileOutput.write(stringOuput)

fileOutput.close()





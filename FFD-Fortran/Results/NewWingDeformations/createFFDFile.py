



# read the file

CONST_Filename = "FPD_crmdesignInitSPBSort.txt"
CONST_OutputFile = "InputFortran_crmwingDesign.dat"
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





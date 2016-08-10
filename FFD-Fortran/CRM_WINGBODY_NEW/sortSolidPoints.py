
CONST_UNSORTEDFILE = "sb.1XZ.dat"
CONST_SORTEDFILELABEL = "sb.1XZSorted.dat"


solidBoundaryPointArray = []

# Read all the solid boundary points from the file
fileInput = open(CONST_UNSORTEDFILE, 'r')

# for keeping track of the arrangement of the points
index = 0
while(True):
    line = fileInput.readline()
    if (line == ""):
        break
    
    stringArray = line.split()
    numArray = []
    for string in stringArray:
        numArray.append(float(string))
    numArray.append(index)
    solidBoundaryPointArray.append(numArray)
    index = index + 1

fileInput.close()


#sort the data in terms of the z values
solidBoundaryPointArray.sort(key=lambda x: x[2])


#write the data into the file sorted

fileOutput = open(CONST_SORTEDFILELABEL, 'w')
for pt in solidBoundaryPointArray:
    string = ""
    for ptValue in pt:
        string = string + str(ptValue) + " "
    
    fileOutput.write(string + "\n")


fileOutput.close()









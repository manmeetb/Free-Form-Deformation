

# a file that will be used for combining the wing and fuselage
# together into one file.

CONST_OutputFile = "newsbp.bothwingfuselage.dat"
CONST_input1 = "newsbp.fuselageZ.dat"
CONST_input2 = "newsbp.wing.dat"


#The methods used to parse a string of 3 numbers with commas
#in between them.
def ParseLine(line):
    x = 0
    y = 0
    z = 0
    
    for i in range(3):
        Comma = line.find(",")
        if (Comma == -1):
            # on last number
            Num = line[0: len(line)]
            z = float(Num)
            break
        Num = line[0:Comma]
        line = line[Comma + 1:len(line)]
        if (i == 0):
            x = float(Num)
        elif (i == 1):
            y = float(Num)

    return (x,y,z)


#read everything from one file first
file1 = open(CONST_input1, 'r')
pointsList = []
while (True):
    line = file1.readline()
    if(line == ""):
        break
    
    pointsList.append(ParseLine(line))

file1.close()

#Read and input only the unique points for the
# second file
file2 = open(CONST_input2, 'r')
i = 0
while (True):
    line = file2.readline()
    if (line == ""):
        break

    point = ParseLine(line)

    if(i%50 == 0):
        print str(i)
    i=i+1
    if ((point in pointsList) == False):
        pointsList.append(point)

file2.close()

# print all the points into the output file

fileOutput = open(CONST_OutputFile, 'w')

for point in pointsList:
    fileOutput.write(str(point[0]) + ", " + str(point[1]) + ", " + \
                     str(point[2]) + "\n")

fileOutput.close()

print "list length: " + str(len(pointsList))









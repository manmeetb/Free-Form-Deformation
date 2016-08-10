#for reading the file and data, making the cross sections
# now in terms of z, and printing the points into a file with a 4th
# column of numbers

CONST_inputFile = "sbp.wing.dat"
CONST_outputFile = "sbp.wingZ.dat"

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

SBPts = []

file = open(CONST_inputFile, 'r')

while (True):
    line = file.readline()
    if(line == ""):
        break
    SBPts.append(ParseLine(line))

#change the ordering of the points. That is,
# make the following changes:
# xNew = z
# yNew = y
# zNew = x
# print the points into the xyz file
fileOutput = open(CONST_outputFile, 'w')

for point in SBPts:
    
    #pt[0] = z #new x
    #pt[1] = y #new y
    #pt[2] = x #new z

    fileOutput.write(str(point[0]) + ", " + str(point[1]) + ", " + \
                 str(point[2]) + "\n")

fileOutput.close()


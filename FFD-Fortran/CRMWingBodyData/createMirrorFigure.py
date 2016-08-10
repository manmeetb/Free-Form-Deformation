

CONST_FileInput = "sbp.bothwingfuselage.dat"

CONST_FileOutputMirror = "sbp.bothwingfuselageMirror.dat"

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

pointArray = []

#read all the points into the point array
file1 = open(CONST_FileInput, 'r')
while (True):
    line = file1.readline()
    if(line == ""):
        break
    pointArray.append(ParseLine(line))

file1.close()

#find the mirror of every point by reflecting it about the xy plane (multiply
# z by -1)

mirrorPointArray = []
for point in pointArray:
    x = point[0]
    y = point[1]
    z = -1.0*point[2]

    point2 = (x,y,z)
    mirrorPointArray.append(point)
    mirrorPointArray.append(point2)


#print all the mirror points into the output file

file2 = open(CONST_FileOutputMirror, 'w')
for point in mirrorPointArray:
    file2.write(str(point[0]) + ", " + str(point[1]) + ", " + \
                     str(point[2]) + "\n")
file2.close()















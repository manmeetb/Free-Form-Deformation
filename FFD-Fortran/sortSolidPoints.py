
CONST_UNSORTEDFILE = "DataFiles/unsortedSbp.fuselage.dat"
#CONST_SORTEDFILE = "sortedZPlanePointsCRM.txt"
CONST_SORTEDFILELABEL = "sortedSbp.fuselage.dat.txt"

CONST_nXFDD = 10
CONST_nYFDD = 3
CONST_nZFDD = 8

#A class that will hold all the data for each boundary point.
class solidBoundaryPoint(object):
    
    def __init__(self, xInitial, yInitial, zInitial):
        self.x = xInitial
        self.y = yInitial
        self.z = zInitial
        
        self.initialX = xInitial
        self.initialY = yInitial
        self.initialZ = zInitial
        
        self.t = 0
        self.u = 0
        self.v = 0
        
        #by default make the element label '-'
        self.label = "-"
    
    #The getters
    def getX(self):
        return self.x
    
    def getY(self):
        return self.y
    
    def getZ(self):
        return self.z
    
    def getXInitial(self):
        return self.initialX
    def getYInitial(self):
        return self.initialY
    def getZInitial(self):
        return self.initialZ
    
    def getT(self):
        return self.t
    
    def getU(self):
        return self.u
    
    def getV(self):
        return self.v
    
    def getLabel(self):
        return self.label
    
    def getLabelInteger(self):
        if (self.label == "-"):
            return 0
        if (self.label == "LeadingEdge"):
            return 1
        if (self.label == "TrailingEdge"):
            return 2
        if (self.label == "WingTip"):
            return 3
        if (self.label == "WingRoot"):
            return 4

    #The setters
    def setT(self, t):
        self.t = t
    
    def setU(self, u):
        self.u = u
    
    def setV(self, v):
        self.v = v
    
    def setX(self, x):
        self.x = x
    
    def setY(self, y):
        self.y = y
    
    def setZ(self, z):
        self.z = z
    
    def setLabel(self,labelString):
        self.label = labelString


def readData(solidBoundaryPointArray):
    
    # read from the file
    file = open(CONST_UNSORTEDFILE, 'r')
    solidBoundaryPointCounter = 0
    XMax=0
    XMin=0
    YMax=0
    YMin=0
    ZMax=0
    ZMin=0
    
    for line in file:
        Num1 = 0.0
        Num2 = 0.0
        Num3 = 0.0
        lastComma = 0
        for i in range(3):
            # print "i: " + str(i)
            Comma = line.find(",")
            # print "Comma: " + str(Comma)
            if (Comma == -1):
                # on last number
                Num = line[0: len(line)]
                Num3 = float(Num)
                # print "Num: " + Num
                break
        
            Num = line[0:Comma]
            # print "Num: " + Num
            line = line[Comma + 1:len(line)]
            # print line
            
            if (i == 0):
                Num1 = float(Num)
            elif (i == 1):
                Num2 = float(Num)
        
        solidBoundaryElement = solidBoundaryPoint(Num1, Num2, Num3)
        solidBoundaryElement.setLabel("-") #set all the labels to "none" first
        solidBoundaryPointArray.append(solidBoundaryElement)


    file.close()


#Takes two floats N1 and N2 and compares them to their second decimal place.
# returns true if the numbers are the same and false otherwise
def compareDecimalPlace(N1, N2, numDecimalPlaces):
    intN1 = int(N1*(10**numDecimalPlaces))
    intN2 = int(N2*(10**numDecimalPlaces))
    if(intN1 == intN2):
        return True
    else:
        return False

#Go through all the points and find the wing tip points. This will be the
# cross section with the largest z value
def FindWingTipPoints():
    ZCrossSectionsList = []
    ZCrossSectionsListRow = []
    numCrossSections = 0
    previousZValue = solidBoundaryPointArray[0].getZInitial()
    for element in solidBoundaryPointArray:
        if(compareDecimalPlace(element.getZ(), previousZValue, 2) == False):
            previousZValue = element.getZInitial()
            ZCrossSectionsList.append(ZCrossSectionsListRow)
            numCrossSections = numCrossSections + 1
            ZCrossSectionsListRow = []
        
        #print "different Z Value: " + str(element.getZ())
        
        else:
            # we are still in the same cross section
            ZCrossSectionsListRow.append(element)

    for element in ZCrossSectionsList[0]:
            element.setLabel("WingTip")


def FindWingRootPoints():
    ZCrossSectionsList = []
    ZCrossSectionsListRow = []
    numCrossSections = 0
    previousZValue = solidBoundaryPointArray[0].getZInitial()
    for element in solidBoundaryPointArray:
        if(compareDecimalPlace(element.getZ(), previousZValue, 2) == False):
            previousZValue = element.getZInitial()
            ZCrossSectionsList.append(ZCrossSectionsListRow)
            numCrossSections = numCrossSections + 1
            ZCrossSectionsListRow = []
        
        #print "different Z Value: " + str(element.getZ())
        
        else:
            # we are still in the same cross section
            ZCrossSectionsListRow.append(element)

    for element in ZCrossSectionsList[numCrossSections-1]:
            element.setLabel("WingRoot")

#The method that is used for finding the leading edge points. We will define the leading edge
#to be the points with the 5 smallest x values (the way the wing is set up it faces in the
# minus x direction)
def FindTrailingEdgePoints():
    
    #Iterate through all the solid boundary point array cross sections.
    #Based on how the points coordinates are placed, the different z cross sections
    # can be defined as being different by comparing the z values up to the 2nd decimal
    # place.
    
    ZCrossSectionsList = []
    numElementsInCrossSection = 0
    ZCrossSectionsListRow = []
    previousZValue = solidBoundaryPointArray[0].getZInitial()
    for element in solidBoundaryPointArray:
        if(compareDecimalPlace(element.getZ(), previousZValue, 2) == False):
            previousZValue = element.getZInitial()
            ZCrossSectionsList.append(ZCrossSectionsListRow)
            ZCrossSectionsListRow = []
            #print "different Z Value: " + str(element.getZ())
        
        else:
            # we are still in the same cross section
            ZCrossSectionsListRow.append(element)

    #Now to make sure only proper cross sections are studied
    # ignore cross sections that are too small (with less than
    # 50 points in it). Go through each row and store the 5 smallest
    # points in each cross section.

    for row in ZCrossSectionsList:
        if(len(row)>50 and len(row)<150):
            tempRow = []
            for element in row:
                # Find the 5 smallest x value points in the row
                
                # Not sure what the effects of sorting the row will be
                #So for now put everything into a temporary row and use that
                tempRow.append(element)
    
            tempRow.sort(key=lambda x: x.getXInitial(), reverse=True)
            # The elements are now sorted from smallest to
            # largest x in the tempRow. Put the first 5 elements
            # into the array
            
            for i in range(5):
                element = tempRow[i]
                element.setLabel("TrailingEdge")




#The method that is used for finding the leading edge points. We will define the leading edge
#to be the points with the 5 smallest x values (the way the wing is set up it faces in the
# minus x direction)
def FindLeadingEdgePoints():
    
    #Iterate through all the solid boundary point array cross sections.
    #Based on how the points coordinates are placed, the different z cross sections
    # can be defined as being different by comparing the z values up to the 2nd decimal
    # place.
    
    ZCrossSectionsList = []
    numElementsInCrossSection = 0
    ZCrossSectionsListRow = []
    previousZValue = solidBoundaryPointArray[0].getZInitial()
    for element in solidBoundaryPointArray:
        if(compareDecimalPlace(element.getZ(), previousZValue, 2) == False):
            previousZValue = element.getZInitial()
            ZCrossSectionsList.append(ZCrossSectionsListRow)
            ZCrossSectionsListRow = []
        #print "different Z Value: " + str(element.getZ())
        
        else:
            # we are still in the same cross section
            ZCrossSectionsListRow.append(element)

    #Now to make sure only proper cross sections are studied
    # ignore cross sections that are too small (with less than
    # 50 points in it). Go through each row and store the 5 smalles
    # points in each cross section.

    for row in ZCrossSectionsList:
        if(len(row)>50 and len(row)<150):
            tempRow = []
            for element in row:
                # Find the 5 smallest x value points in the row
                
                # Not sure what the effects of sorting the row will be
                #So for now put everything into a temporary row and use that
                tempRow.append(element)
                
            tempRow.sort(key=lambda x: x.getXInitial(), reverse=False)
            # The elements are now sorted from smallest to
            # largest x in the tempRow. Put the first 5 elements
            # into the array
            
            for i in range(5):
                element = tempRow[i]
                element.setLabel("LeadingEdge")






def writeSortedData(solidBoundaryPointArray):
    f = open(CONST_SORTEDFILELABEL, "w")
    for i in range(len(solidBoundaryPointArray)):
        element = solidBoundaryPointArray[i]
        f.write(str(element.getX()) + ", " + str(element.getY()) + ", " + str(element.getZ()) + ", " + str(element.getLabelInteger()) + "\n")




                    #Main Method
solidBoundaryPointArray = []
readData(solidBoundaryPointArray)
#sort the data in terms of the z values
solidBoundaryPointArray.sort(key=lambda x: x.getZ(), reverse=True)

#Look for constraint locations on wing

#FindLeadingEdgePoints()
#FindTrailingEdgePoints()
#FindWingRootPoints()
#FindWingTipPoints()

#write the data into the file sorted
writeSortedData(solidBoundaryPointArray)










	SUBROUTINE READSOLIDDATA()
c use the global variable module
	USE VAR

		
c Open the file that needs to be read:
	WRITE(*,*) "File: ", SolidBoundaryPointsFile
	
	OPEN(UNIT = 14, FILE = SolidBoundaryPointsFile)

c go through the file and read how many lines there are:
	inumPoints = 0
	DO WHILE(.TRUE.)
		READ(14,*, IOSTAT = io), rx,ry,rz, il
		IF(inumPoints == 0) THEN
			WRITE(*,*) "rx: ", rx
			WRITE(*,*) "ry: ", ry
			WRITE(*,*) "rz: ", rz
			WRITE(*,*) "il: ", il
			
		ENDIF
		If(io/=0) THEN
			!If the end of file has been reached, io will be non 0
			exit
		ENDIF
		inumPoints = inumPoints + 1
	ENDDO
	CLOSE(14) !Close the file after the number of points has now been computed
	
	!Set the size of the array into the global variable holding the number of 
	!solid body points
	SolidBoundaryPointsSize = inumPoints 
	
c now that the number of points has been read, allocate a multidimensional array of that
c size and input all the data. For now, we only have 1 Body element (the NACA wing).

	ALLOCATE(SolidBoundaryPoints(1, inumPoints, 7))
c Now, read through the file again and fill the array with the point data
	OPEN(UNIT=15, FILE = SolidBoundaryPointsFile)
	iBodyPointIndex = 1 !For the index of the body's solid point
	DO WHILE(.TRUE.)
		READ(15, *, IOSTAT = io), rx, ry, rz, il
		IF(io/=0) THEN
			EXIT !io = -1 so end of file reached
		ENDIF
		SolidBoundaryPoints(1,iBodyPointIndex, 1) = rx
		SolidBoundaryPoints(1,iBodyPointIndex, 2) = ry
		SolidBoundaryPoints(1,iBodyPointIndex, 3) = rz
		!T,U,V data arbitrarily set to 0
		SolidBoundaryPoints(1,iBodyPointIndex, 4) = 0
		SolidBoundaryPoints(1,iBodyPointIndex, 5) = 0
		SolidBoundaryPoints(1,iBodyPointIndex, 6) = 0
			
		!Adding the label for the solid boundary point
		SolidBoundaryPoints(1,iBodyPointIndex, 7) = il
		iBodyPointIndex = iBodyPointIndex +1
	ENDDO
	CLOSE(15)
	
	!WRITE(*,*) "I = 1"
	!WRITE(*,*) "x: ", SolidBoundaryPoints(1,1,1)
	!WRITE(*,*) "y: ", SolidBoundaryPoints(1,1,2)
	!WRITE(*,*) "z: ", SolidBoundaryPoints(1,1,3)

	!WRITE(*,*) "I = last"
	!WRITE(*,*) "x: ", SolidBoundaryPoints(1,SolidBoundaryPointsSize,1)
	!WRITE(*,*) "y: ", SolidBoundaryPoints(1,SolidBoundaryPointsSize,2)
	!WRITE(*,*) "z: ", SolidBoundaryPoints(1,SolidBoundaryPointsSize,3)

	! deallocating the array's space
	!DEALLOCATE(SolidBoundaryPoints)
	
	END

#!/bin/bash

make

echo "Make Complete"
echo " "

./executable

#Move all the txt files to the folder data files
for textFile in *.txt
do
	echo "processing $textFile"
	mv $textFile DataFiles
done

#Plot the data
python2.6 plotPython.py


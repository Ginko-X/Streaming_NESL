#!/usr/bin/env python

# Helper program to plot the data of works and steps into one figure
# Usage: ./plot.py <costFilename>  # without ".cost"

import matplotlib.pyplot as plt
import numpy as np
import sys

def readData(filename):
	fileobj = open(filename)
	sneslW = []
	sneslS = []
	svcodeW = []
	svcodeS = []

	lines = fileobj.readlines()
	if lines[0].strip().split(' ')[0] == "For"	:	
		size = lines[0].strip().split(' ')[3:]
	else :
		return []

	for line in lines:
		lineList = line.strip().split(' ')
		if lineList[0] == "SNESL" : 
		    sneslW.append(int(lineList[2][:-1]))
		    sneslS.append(int(lineList[4][:-1]))
		else: 
			if lineList[0] == "SVCODE" : 
				svcodeW.append(int(lineList[2][:-1]))
				svcodeS.append(int(lineList[4][:-1]))		    
	
	fileobj.close()
	return (size,[sneslW,sneslS,svcodeW,svcodeS])


def subPlot(subFig, xs, sdata, svdata, costLabel):			
	plt.sca(subFig)  
	plt.plot(xs, sdata, 'o-',label="SNESL")
	plt.plot(xs, svdata, 'D-',label="SVCODE")
	plt.xlabel("input size")
	plt.ylabel(costLabel)
	plt.legend(loc='best')


cost_file = sys.argv[1]
(size,res) = readData(cost_file+".cost")
ax1 = plt.subplot(211) 
ax2 = plt.subplot(212) 
subPlot(ax1, size, res[0], res[2], "work")
subPlot(ax2, size, res[1], res[3], "step")
plt.savefig(cost_file +".jpg")


# Program to print out model projects currently listed in
# the database, along with the meta-data attributes.

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory

import sys
import os
import pickle

# Establish class object to hold information on model runs
class modelDatabase:
        def __init__(self):
                self.alias = []
                self.modelInDir = []
                self.topDir = []
                self.forceInDir = []
                self.tag = []
                self.geoFile = []
                self.fullDomFile = []
                self.mskFile = []
                self.link2GageFile = []
                self.strObsFile = []
                self.snotelObsFile = []
                self.amfObsFile = []
                self.metObsFile = []
		self.snodasPath = []
        def printInfo(self):
                filePathDb = "./modelMeta_db.pkl"
                # Read data in from pickle file
                with open(filePathDb,'rb') as input:
                        dbTmp = pickle.load(input)
               	lenTmp = len(dbTmp.tag)
                for i in range(0,lenTmp):
			print "----------------------------------------------------"
			print "Model Project Number:            " + str(i+1)
			print "*Alias:                          " + dbTmp.alias[i]
			print "*Source Directory:               " + dbTmp.modelInDir[i]
			print "*Output Analaysis Directory:     " + dbTmp.topDir[i] + "/" + dbTmp.alias[i]
			print "*Model Tag:                      " + dbTmp.tag[i]
			print "*Model Forcing Source Directory: " + dbTmp.forceInDir[i]
			print "*Input Geo File:                 " + dbTmp.geoFile[i]
			print "*Input FullDom File:             " + dbTmp.fullDomFile[i]
			print "*Input Mask File:                " + dbTmp.mskFile[i]
			print "*Streamflow Observations File:   " + dbTmp.strObsFile[i]
			print "*SNOTEL Observations File:       " + dbTmp.snotelObsFile[i]
			print "*AMF Observations File:          " + dbTmp.amfObsFile[i]
			print "*HydroMet Observations File:     " + dbTmp.metObsFile[i]
			print "*Link2Gage File:                 " + dbTmp.link2GageFile[i]
			print "*SNODAS Directory:               " + dbTmp.snodasPath[i]

fileIn = "./modelMeta_db.pkl"

if not os.path.isfile(fileIn):
	print "ERROR: Input database file not found."
	print "       Please create one to list content."
 	sys.exit(1)	

# Open database file.
db = modelDatabase()

# Print information to screen for user
modelDatabase.printInfo(db)

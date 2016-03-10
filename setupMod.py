# Main program for setting up and managing model projects and runs.

# Logan Karsten
# National Center For Atmospheric Research
# Research Applications Laboratory

import os
from ConfigParser import SafeConfigParser
import pickle
import shutil

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
	def readData(self,parser,readFlag):
		filePathDb = "./modelMeta_db.pkl"
		if readFlag == 1:
			# Read data in from pickle file
			with open(filePathDb,'rb') as input:
				dbTmp = pickle.load(input)
			lenTmp = len(dbTmp.tag)
			for i in range(0,lenTmp):
				self.topDir.append(dbTmp.topDir[i])
				self.modelInDir.append(dbTmp.modelInDir[i])
				self.alias.append(dbTmp.alias[i])
				self.tag.append(dbTmp.tag[i])
				self.forceInDir.append(dbTmp.forceInDir[i])
				self.geoFile.append(dbTmp.geoFile[i])
				self.fullDomFile.append(dbTmp.fullDomFile[i])
				self.mskFile.append(dbTmp.mskFile[i])
				self.link2GageFile.append(dbTmp.link2GageFile[i])
				self.strObsFile.append(dbTmp.strObsFile[i])
				self.snotelObsFile.append(dbTmp.snotelObsFile[i])
				self.amfObsFile.append(dbTmp.amfObsFile[i])
				self.metObsFile.append(dbTmp.metObsFile[i])
				self.snodasPath.append(dbTmp.snodasPath[i])

		# Read in temporary data from config file
		topOutTmp = parser.get('top_level','topOut')
		modelInTmp = parser.get('model_specification','modelDir')
		tagTmp = parser.get('model_specification','tag')
		aliasTmp = parser.get('model_specification','alias')
		forcingInTmp = parser.get('forcing_specification','forcingDir')
		geoFileTmp = parser.get('geo_specification','geoFile')
		geoHydFileTmp = parser.get('geo_specification','geoHydFile')
		mskFileTmp = parser.get('geo_specification','maskFile')
		lnkFileTmp = parser.get('geo_specification','link2gageFile')
		strObsFileTmp = parser.get('observations_specification','strObsFile')
		snotelObsFileTmp = parser.get('observations_specification','snotelObsFile')
		amfObsFileTmp = parser.get('observations_specification','amfObsFile')
		metObsFileTmp = parser.get('observations_specification','metObsFile')
		snodasPathTmp = parser.get('observations_specification','snodasPath')

		# Check to make sure information has not already been entered into databse
		lenTmp = len(self.alias)
		if lenTmp > 0:
			for i in range(0,lenTmp):
				if self.alias[i] == aliasTmp:
					print "ERROR: " + aliasTmp + " has already been entered into database."
					print "Consider removing before trying again."
					return

		# Add parameters to new list
		self.topDir.append(topOutTmp)
		self.modelInDir.append(modelInTmp)
		self.alias.append(aliasTmp)
		self.tag.append(tagTmp)
		self.forceInDir.append(forcingInTmp)
		self.geoFile.append(geoFileTmp)
		self.fullDomFile.append(geoHydFileTmp)
		self.mskFile.append(mskFileTmp)
		self.link2GageFile.append(lnkFileTmp)
		self.strObsFile.append(strObsFileTmp)
		self.snotelObsFile.append(snotelObsFileTmp)
		self.amfObsFile.append(amfObsFileTmp)
		self.metObsFile.append(metObsFileTmp)
		self.snodasPath.append(snodasPathTmp)

	def setupProject(self):
		""" Function to create necessary subdirectories
		    and symbolic links necessary to perform
		    analysis and plotting for specified model run.
		"""

		ind = len(self.alias) - 1

		# First check to see if subdirectory already exists.
		# If it does, this has already been performed. 
		subDir1 = self.topDir[ind] + "/" + self.alias[ind]
		if os.path.exists(subDir1):
			print "ERROR: " + self.alias[ind] + " project subdirectory already exists."
			return
		else:
			os.mkdir(subDir1)

		# Make temporary directory used to hold R datasets
		tmpDir = subDir1 + "/tmp"
		os.mkdir(tmpDir)

		# Make model input directory
		modelInDir1 = subDir1 + "/WRF-Hydro_Output" 
		os.symlink(self.modelInDir[ind],modelInDir1)

		# Create namelist directory to hold all namelists created by evaluation program.
		namelistDir = subDir1 + "/namelists"
		os.mkdir(namelistDir)

		# Make observations directory, symlink observation files in (if they were listed)
		obsDir1 = subDir1 + "/Observations"
		strObsLnk = obsDir1 + "/strObs.Rdata"
		snotelObsLnk = obsDir1 + "/snotelObs.Rdata"
		amfObsLnk = obsDir1 + "/amfObs.Rdata"
		metObsLnk = obsDir1 + "/metObs.Rdata"
		snodasLnk = obsDir1 + "/snodas"
		os.mkdir(obsDir1)
		# Only create links if file was entered into model project
		if len(self.strObsFile[ind]) != 0:
			os.symlink(self.strObsFile[ind],strObsLnk)
		if len(self.snotelObsFile[ind]) != 0:
			os.symlink(self.snotelObsFile[ind],snotelObsLnk)
		if len(self.amfObsFile[ind]) != 0:
			os.symlink(self.amfObsFile[ind],amfObsLnk)
		if len(self.metObsFile[ind]) != 0:
			os.symlink(self.metObsFile[ind],metObsLnk)
		if len(self.snodasPath[ind]) != 0:
			os.symlink(self.snodasPath[ind],snodasLnk)
	
		# Make geospatial directory containing symbolic links to geospatial files.
		geoDir1 = subDir1 + "/geo"
		os.mkdir(geoDir1)
		geoLsmLnk = geoDir1 + "/geo_em.nc"
		geoHydroLnk = geoDir1 + "/geo_Hydro.nc"
		mskFile = geoDir1 + "/masks_geo.Rdata"
		link2GagesFile = geoDir1 + "/links_2_gages.txt"
		os.symlink(self.geoFile[ind],geoLsmLnk)
		os.symlink(self.fullDomFile[ind],geoHydroLnk)
		if len(self.link2GageFile[ind]) != 0:
			os.symlink(self.link2GageFile[ind],link2GagesFile)

		# Link forcing directory
		forcDir1 = subDir1 + "/forcing"
		os.symlink(self.forceInDir[ind],forcDir1)

		# Create output directories to hold analysis/plotting data
		outDir1 = subDir1 + "/analysis_out"
		outDir2 = outDir1 + "/analysis_datasets"
		outDir3 = outDir1 + "/plotting"
		outDir4 = outDir1 + "/read_datasets"
		os.mkdir(outDir1)
		os.mkdir(outDir2)
		os.mkdir(outDir3)
		os.mkdir(outDir4)

	def deleteProject(self,aliasCheck,filePathDb):
		""" Remove project and directory structure
		    associated with particular alias. 
		    USE WITH CAUTION!!!!
		"""

		aliasTmp = []
		modelInDirTmp = []
		topDirTmp = []
		forceInDirTmp = []
		tagTmp = []
		geoFileTmp = []
		fullDomFileTmp = []
		mskFileTmp = []
		link2GageFileTmp = []
		strObsFileTmp = []
		snotelObsFileTmp = []
		amfObsFileTmp = []
		metObsFileTmp = []
		snodasPathTmp = []

		with open(filePathDb,'rb') as input:
	                dbTmp = pickle.load(input)
                        lenTmp = len(dbTmp.tag)
                        for i in range(0,lenTmp):
                                self.topDir.append(dbTmp.topDir[i])
                                self.modelInDir.append(dbTmp.modelInDir[i])
                                self.alias.append(dbTmp.alias[i])
                                self.tag.append(dbTmp.tag[i])
                                self.forceInDir.append(dbTmp.forceInDir[i])
                                self.geoFile.append(dbTmp.geoFile[i])
                                self.fullDomFile.append(dbTmp.fullDomFile[i])
                                self.mskFile.append(dbTmp.mskFile[i])
                                self.link2GageFile.append(dbTmp.link2GageFile[i])
                                self.strObsFile.append(dbTmp.strObsFile[i])
                                self.snotelObsFile.append(dbTmp.snotelObsFile[i])
                                self.amfObsFile.append(dbTmp.amfObsFile[i])
                                self.metObsFile.append(dbTmp.metObsFile[i])	
				self.snodasPath.append(dbTmp.snodasPath[i])

		# Loop through projects in database, once found,
		# remove project, and files. Return updated 
		# database to be saved 
		for i in range(0,len(self.alias)):
			if self.alias[i] != aliasCheck:
				aliasTmp.append(self.alias[i])
				modelInDirTmp.append(self.modelInDir[i])
				topDirTmp.append(self.topDir[i])
				forceInDirTmp.append(self.forceInDir[i])
				tagTmp.append(self.tag[i])
				geoFileTmp.append(self.geoFile[i])
				fullDomFileTmp.append(self.fullDomFile[i])
				mskFileTmp.append(self.mskFile[i])
				link2GageFileTmp.append(self.link2GageFile[i])
				strObsFileTmp.append(self.strObsFile[i])
				snotelObsFileTmp.append(self.snotelObsFile[i])
				amfObsFileTmp.append(self.amfObsFile[i])
				metObsFileTmp.append(self.metObsFile[i])
				snodasPathTmp.append(self.snodasPath[i])
			else: # Found project to delete
				deleteDir = self.topDir[i] + "/" + self.alias[i]
				shutil.rmtree(deleteDir, ignore_errors=True)

		# Walk through temporary directory and update database
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

		for i in range(0,len(aliasTmp)):
			self.alias.append(aliasTmp[i])
			self.modelInDir.append(modelInDirTmp[i])
			self.topDir.append(topDirTmp[i])
			self.forceInDir.append(forceInDirTmp[i])
			self.tag.append(tagTmp[i])
			self.geoFile.append(geoFileTmp[i])
			self.fullDomFile.append(fullDomFileTmp[i])
			self.mskFile.append(mskFileTmp[i])
			self.link2GageFile.append(link2GageFileTmp[i])
			self.strObsFile.append(strObsFileTmp[i])
			self.snotelObsFile.append(snotelObsFileTmp[i])
			self.amfObsFile.append(amfObsFileTmp[i])
			self.metObsFile.append(metObsFileTmp[i])
			self.snodasPath.append(snodasPathTmp[i])


def addModelProject():
	""" Setup new model project, which will be added to the db
	    containing all model projects, whith their associated
	    information
	"""

	configPath = "./parm/setup_parm.parm"
	parser = SafeConfigParser()

	if os.path.isfile(configPath):
		parser.read(configPath)
	else:
		print "ERROR: Config file setup_parm.parm not found."
		return

	outDir = parser.get('top_level','topOut')

	if not os.path.exists(outDir):
		print "ERROR: output directory: " + outDir + " not found."
		return

	# Initiate model database class instance
	db = modelDatabase()

	# Read in model database containing all relevant information
	readFlag = 1
	dbPath = "./modelMeta_db.pkl"
	if not os.path.isfile(dbPath):
		readFlag = 0

	modelDatabase.readData(db,parser,readFlag)
	numModels = len(db.tag)

	# Check for existence of files/directories
	if not os.path.exists(db.modelInDir[numModels-1]):
		print "ERROR: Model input directory: " + db.modelInDir[numModels] + " not found."
	if not os.path.exists(db.forceInDir[numModels-1]):
		print "ERROR: Forcing input directory: " + db.forceInDir[numModels] + " not found."
	if not os.path.isfile(db.geoFile[numModels-1]):
		print "ERROR: Geofile: " + db.geoFile[numModels] + db.geoFile[numModels] + " not found."
	if not os.path.isfile(db.fullDomFile[numModels-1]):
		print "ERROR: High resolution geofile: " + db.fullDomFile[numModels] + " not found."
	if not os.path.isfile(db.mskFile[numModels-1]):
		print "ERROR: Mask file: " + db.mskFile[numModels] + " not found."
	# Won't check for existence of remaining files as they are optional.

	# Save database, or updated database to pickle object to be read in next time.
	with open(dbPath,'wb') as output:
		pickle.dump(db, output, pickle.HIGHEST_PROTOCOL)

	# Use db entry to create a subdirectory to hold symbolic links to all necessary data to run 
	# analysis.
	modelDatabase.setupProject(db)

def removeModelProject(alias):
	""" Remove model project, along with associated links,
	    and data. USE WITH CAUTION!!! DATA IS GONE FOREVER
 	    ONCE THIS IS INVOKED!!!!
	"""

	dbPath = "./modelMeta_db.pkl"
	if not os.path.isfile(dbPath):
		print "ERROR: Cannot remove database entry, DB file not found."
		return
	
	# Initiate model database class instance
	db = modelDatabase()

	modelDatabase.deleteProject(db,alias,dbPath)
	
	# Save updated database with project removed
	with open(dbPath,'wb') as output:
		pickle.dump(db, output, pickle.HIGHEST_PROTOCOL)
	

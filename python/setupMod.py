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
		self.ensList = []
		self.ensTag = []
		self.geoFile = []
		self.geoRes = []
		self.fullDomFile = []
		self.mskFile = []
		self.link2GageFile = []
		self.strObsFile = []
		self.snotelObsFile = []
		self.amfObsFile = []
		self.metObsFile = []
		self.snodasPath = []
		self.nCores = []
	def readData(self,parser,readFlag):
		filePathDb = "./parm/modelMeta_db.pkl"
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
				self.ensList.append(dbTmp,ensList[i])
				self.ensTag.append(dbTmp,ensTag[i])
				self.forceInDir.append(dbTmp.forceInDir[i])
				self.geoFile.append(dbTmp.geoFile[i])
				self.geoRes.append(dbTmp.geoRes[i])
				self.fullDomFile.append(dbTmp.fullDomFile[i])
				self.mskFile.append(dbTmp.mskFile[i])
				self.link2GageFile.append(dbTmp.link2GageFile[i])
				self.strObsFile.append(dbTmp.strObsFile[i])
				self.snotelObsFile.append(dbTmp.snotelObsFile[i])
				self.amfObsFile.append(dbTmp.amfObsFile[i])
				self.metObsFile.append(dbTmp.metObsFile[i])
				self.snodasPath.append(dbTmp.snodasPath[i])
				self.nCores.append(dbTmp.nCores[i])

		# Read in temporary data from config file
		topOutTmp = parser.get('top_level','topOut')
		modelInTmp = parser.get('model_specification','modelDir')
		tagTmp = parser.get('model_specification','tag')
		aliasTmp = parser.get('model_specification','alias')
		ensListTmp = parser.get('ensemble_specification','ensembleList')
		ensTagTmp = parser.get('ensemble_specification','ensembleTags')
		# Convert strings to lists
		ensListTmp = ensListTmp.split()
		ensTagTmp = ensTagTmp.split()
		forcingInTmp = parser.get('forcing_specification','forcingDir')
		geoFileTmp = parser.get('geo_specification','geoFile')
		geoResTmp = parser.get('geo','geoRes')
		geoHydFileTmp = parser.get('geo_specification','geoHydFile')
		mskFileTmp = parser.get('geo_specification','maskFile')
		lnkFileTmp = parser.get('geo_specification','link2gageFile')
		strObsFileTmp = parser.get('observations_specification','strObsFile')
		snotelObsFileTmp = parser.get('observations_specification','snotelObsFile')
		amfObsFileTmp = parser.get('observations_specification','amfObsFile')
		metObsFileTmp = parser.get('observations_specification','metObsFile')
		snodasPathTmp = parser.get('observations_specification','snodasPath')
		nCoresTmp = parser.get('run_options','nCores')

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
		self.ensList.append(ensListTmp)
		self.ensTag.append(ensTagTmp)
		self.forceInDir.append(forcingInTmp)
		self.geoFile.append(geoFileTmp)
		self.geoRes.append(geoResTmp)
		self.fullDomFile.append(geoHydFileTmp)
		self.mskFile.append(mskFileTmp)
		self.link2GageFile.append(lnkFileTmp)
		self.strObsFile.append(strObsFileTmp)
		self.snotelObsFile.append(snotelObsFileTmp)
		self.amfObsFile.append(amfObsFileTmp)
		self.metObsFile.append(metObsFileTmp)
		self.snodasPath.append(snodasPathTmp)
		self.nCores.append(nCoresTmp)

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
		ensListTmp = []
		ensTagTmp = []
		geoFileTmp = []
		geoResTmp = []
		fullDomFileTmp = []
		mskFileTmp = []
		link2GageFileTmp = []
		strObsFileTmp = []
		snotelObsFileTmp = []
		amfObsFileTmp = []
		metObsFileTmp = []
		snodasPathTmp = []
		nCoresTmp = []

		with open(filePathDb,'rb') as input:
	                dbTmp = pickle.load(input)
                        lenTmp = len(dbTmp.tag)
                        for i in range(0,lenTmp):
                                self.topDir.append(dbTmp.topDir[i])
                                self.modelInDir.append(dbTmp.modelInDir[i])
                                self.alias.append(dbTmp.alias[i])
                                self.tag.append(dbTmp.tag[i])
				self.ensList.append(dbTmp.ensList[i])
				self.ensTag.append(dbTmp.ensTag[i])
                                self.forceInDir.append(dbTmp.forceInDir[i])
                                self.geoFile.append(dbTmp.geoFile[i])
				self.geoRes.append(dbTmp.geoRes[i])
                                self.fullDomFile.append(dbTmp.fullDomFile[i])
                                self.mskFile.append(dbTmp.mskFile[i])
                                self.link2GageFile.append(dbTmp.link2GageFile[i])
                                self.strObsFile.append(dbTmp.strObsFile[i])
                                self.snotelObsFile.append(dbTmp.snotelObsFile[i])
                                self.amfObsFile.append(dbTmp.amfObsFile[i])
                                self.metObsFile.append(dbTmp.metObsFile[i])	
				self.snodasPath.append(dbTmp.snodasPath[i])
				self.nCores.append(dbTmp.nCores[i])

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
				ensListTmp.append(self.ensList[i])
				ensTagTmp.append(self.ensTag[i])
				geoFileTmp.append(self.geoFile[i])
				geoResTmp.append(self.geoRes[i])
				fullDomFileTmp.append(self.fullDomFile[i])
				mskFileTmp.append(self.mskFile[i])
				link2GageFileTmp.append(self.link2GageFile[i])
				strObsFileTmp.append(self.strObsFile[i])
				snotelObsFileTmp.append(self.snotelObsFile[i])
				amfObsFileTmp.append(self.amfObsFile[i])
				metObsFileTmp.append(self.metObsFile[i])
				snodasPathTmp.append(self.snodasPath[i])
				nCoresTmp.append(self.nCores[i])
			else: # Found project to delete
				deleteDir = self.topDir[i] + "/" + self.alias[i]
				shutil.rmtree(deleteDir, ignore_errors=True)

		# Walk through temporary directory and update database
		self.alias = []
		self.modelInDir = []
                self.topDir = []
                self.forceInDir = []
                self.tag = []
		self.ensList = []
		self.ensTag = []
                self.geoFile = []
		self.geoRes = []
                self.fullDomFile = []
                self.mskFile = []
                self.link2GageFile = []
                self.strObsFile = []
                self.snotelObsFile = []
                self.amfObsFile = []
                self.metObsFile = []
		self.snodasPath = []
		self.nCores = []

		for i in range(0,len(aliasTmp)):
			self.alias.append(aliasTmp[i])
			self.modelInDir.append(modelInDirTmp[i])
			self.topDir.append(topDirTmp[i])
			self.forceInDir.append(forceInDirTmp[i])
			self.tag.append(tagTmp[i])
			self.ensList.append(ensListTmp[i])
			self.ensTag.append(ensTagTmp[i])
			self.geoFile.append(geoFileTmp[i])
			self.geoRes.append(geoResTmp[i])
			self.fullDomFile.append(fullDomFileTmp[i])
			self.mskFile.append(mskFileTmp[i])
			self.link2GageFile.append(link2GageFileTmp[i])
			self.strObsFile.append(strObsFileTmp[i])
			self.snotelObsFile.append(snotelObsFileTmp[i])
			self.amfObsFile.append(amfObsFileTmp[i])
			self.metObsFile.append(metObsFileTmp[i])
			self.snodasPath.append(snodasPathTmp[i])
			self.nCores.append(nCoresTmp[i])

	def copyModel(self,dbPath,args,aliasIn,aliasNew):
		""" Copy model database entry to
		    new database entry using passed
		    arguments.
		"""
		with open(dbPath,'rb') as input:
                        dbTmp = pickle.load(input)
                        lenTmp = len(dbTmp.tag)
                        for i in range(0,lenTmp):
                                self.topDir.append(dbTmp.topDir[i])
                                self.modelInDir.append(dbTmp.modelInDir[i])
                                self.alias.append(dbTmp.alias[i])
                                self.tag.append(dbTmp.tag[i])
				self.ensList.append(dbTmp.ensList[i])
				self.ensTag.append(dbTmp.ensTag[i])
                                self.forceInDir.append(dbTmp.forceInDir[i])
                                self.geoFile.append(dbTmp.geoFile[i])
				self.geoRes.append(dbTmp.geoRes[i])
                                self.fullDomFile.append(dbTmp.fullDomFile[i])
                                self.mskFile.append(dbTmp.mskFile[i])
                                self.link2GageFile.append(dbTmp.link2GageFile[i])
                                self.strObsFile.append(dbTmp.strObsFile[i])
                                self.snotelObsFile.append(dbTmp.snotelObsFile[i])
                                self.amfObsFile.append(dbTmp.amfObsFile[i])
                                self.metObsFile.append(dbTmp.metObsFile[i])
                                self.snodasPath.append(dbTmp.snodasPath[i])
				self.nCores.append(dbTmp.nCores[i])

		# Loop through projects in database, once the primary
		# input alias has been found, calculate the index, which
		# will be used to pull information from for the new entry.
		aliasInd = -1
                for i in range(0,len(self.alias)):
			if self.alias[i] == aliasIn:
				aliasInd = i
			if self.alias[i] == aliasNew:
				aliasInd = -2

		# If model not found, raise error.
		if aliasInd == -1:
			print "ERROR: Existing model project specified."
			raise 

		# If model has already been entered, raise error.
		if aliasInd == -2:
			print "ERROR: Requested new project already in database."
			raise

		# Begin populating new database entry
		self.alias.append(aliasNew)

		if args.topOut:
			self.topDir.append(args.topOut)
		else:
			self.topDir.append(self.topDir[aliasInd])

		if args.modelIn:
			self.modelInDir.append(args.modelIn)
		else:
			self.modelInDir.append(self.modelInDir[aliasInd])
		
		if args.forcingDir:
			self.forceInDir.append(args.forcingDir)
		else:
			self.forceInDir.append(self.forceInDir[aliasInd])

		if args.tag:
			self.tag.append(args.tag)
		else:
			self.tag.append(self.tag[aliasInd])

		if args.ensList and not args.ensTag:
			print "ERROR: Must supply both ensemble list and tags."
			raise
		if args.ensTag and not args.ensList:
			print "ERROR: Must supply both ensemble list and tags."
			raise

		if len(args.ensTag.split()) != len(args.ensList.split()):
			print "ERROR: Ensemble list and tags must be equal length."
			raise

		if args.ensList:
			self.ensList.append(args.ensList.split())
		else:
			self.ensList.append(self.ensList[aliasInd])

		if args.ensTag:
			self.ensTag.append(args.ensTag.split())
		else:
			self.ensTag.append(self.ensTag[aliasInd])

		if args.mskFile:
			self.mskFile.append(args.mskFile)
		else:
			self.mskFile.append(self.mskFile[aliasInd])

		if args.geoFile:
			self.geoFile.append(args.geoFile)
		else:
			self.geoFile.append(self.geoFile[aliasInd])

		if args.geoRes:
			self.geoRes.append(args.geoRes)
		else:
			self.geoRes.append(args.geoRes[aliasInd])

		if args.hydFile:
			self.fullDomFile.append(args.hydFile)
		else:
			self.fullDomFile.append(self.fullDomFile[aliasInd])

		if args.link2gage:
			self.link2GageFile.append(args.link2gage)
		else:
			self.link2GageFile.append(self.link2GageFile[aliasInd])
	
		if args.snPath:
			self.snodasPath.append(args.snPath)
		else:
			self.snodasPath.append(self.snodasPath[aliasInd])

		if args.strObsFile:
			self.strObsFile.append(args.strObsFile)
		else:
			self.strObsFile.append(self.strObsFile[aliasInd])

		if args.snotelFile:
			self.snotelObsFile.append(args.snotelFile)
		else:
			self.snotelObsFile.append(self.snotelObsFile[aliasInd])

		if args.metFile:
			self.metObsFile.append(args.metFile)
		else:
			self.metObsFile.append(self.metObsFile[aliasInd])

		if args.amfFile:
			self.amfObsFile.append(args.amfFile)
		else:
			self.amfObsFile.append(self.amfObsFile[aliasInd])
	
		if args.nCores:
			self.nCores.append(args.nCores)
		else:
			self.nCores.append(self.nCores[aliasInd])

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
	dbPath = "./parm/modelMeta_db.pkl"
	if not os.path.isfile(dbPath):
		readFlag = 0

	modelDatabase.readData(db,parser,readFlag)
	numModels = len(db.tag)

	# Check for existence of files/directories
	if not os.path.exists(db.modelInDir[numModels-1]):
		print "ERROR: Model input directory: " + db.modelInDir[numModels-1] + " not found."
	if not os.path.exists(db.forceInDir[numModels-1]):
		print "ERROR: Forcing input directory: " + db.forceInDir[numModels-1] + " not found."
	if not os.path.isfile(db.geoFile[numModels-1]):
		print "ERROR: Geofile: " + db.geoFile[numModels] + db.geoFile[numModels-1] + " not found."
	if not os.path.isfile(db.fullDomFile[numModels-1]):
		print "ERROR: High resolution geofile: " + db.fullDomFile[numModels-1] + " not found."
	if not os.path.isfile(db.mskFile[numModels-1]):
		print "ERROR: Mask file: " + db.mskFile[numModels-1] + " not found."
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

	dbPath = "./parm/modelMeta_db.pkl"
	if not os.path.isfile(dbPath):
		print "ERROR: Cannot remove database entry, DB file not found."
		return
	
	# Initiate model database class instance
	db = modelDatabase()

	modelDatabase.deleteProject(db,alias,dbPath)
	
	# Save updated database with project removed
	with open(dbPath,'wb') as output:
		pickle.dump(db, output, pickle.HIGHEST_PROTOCOL)

def copyModelProject(args,aliasIn,aliasNew):
	""" Copy existing model database entry 
	    using entered user options.
	"""

	dbPath = "./parm/modelMeta_db.pkl"
        if not os.path.isfile(dbPath):
                print "ERROR: Cannot remove database entry, DB file not found."
                return

        # Initiate model database class instance
        db = modelDatabase()

	modelDatabase.copyModel(db,dbPath,args,aliasIn,aliasNew)

	# Save updated database with project removed
        with open(dbPath,'wb') as output:
                pickle.dump(db, output, pickle.HIGHEST_PROTOCOL)

	# Establish subdirectory within specified output directory 
	# for model project.
	modelDatabase.setupProject(db)	

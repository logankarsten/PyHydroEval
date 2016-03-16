# Functions to edit the R namelist file for analysis/plotting.
# This will edit the file symbolicly linked in place.

# Logan Karsten
# National Center for Atmopsheric Research
# Research Applications Laboratory

import fileinput
import sys
import os
from pyHydroEvalUtils import editLine as el
from pyHydroEvalUtils import returnDate as rt
import ioMgmntMod

def editNamelist(pathIn,args,dbIn):
	# Go through various options chosen by user and edit namelist file accordingly
	numModIn = len(args.modelProjects)
	numModDb = len(dbIn.alias)
	for i in range(0,numModDb):
                if dbIn.alias[i] == args.modelProjects[0]:
                        indDbOrig = i

	# Calculate model tags associated with alias values chosen.
	tagInds = []
	tags = []
	aliasTags = []
	modPaths = []
	forcPaths = []
	for i in range(0, numModDb):
		for j in range(0, numModIn):
			if dbIn.alias[i] == args.modelProjects[j]:
				tagInds.append(i)
				tags.append(dbIn.tag[i])
				aliasTags.append(dbIn.alias[i])
				modPaths.append(dbIn.modelInDir[i])
				forcPaths.append(dbIn.forceInDir[i])

	# Compose tag string that will be placed into namelist file
	tagStr = "c("
	for i in range(0, numModIn):
		if i == (numModIn - 1):
			tagStr = tagStr + "'" + tags[i] + "')"
		else:
			tagStr = tagStr + "'" + tags[i] + "', " 	

	# Compose model/force path list string
	pathListStr = "c("
	forcPathListStr = "c("
	for i in range(0, numModIn):
		if i == (numModIn - 1):
			pathListStr = pathListStr + "'" + modPaths[i] + "')"
			forcPathListStr = forcPathListStr + "'" + forcPaths[i] + "')"
		else:
			pathListStr = pathListStr + "'" + modPaths[i] + "', "
			forcPathListStr = forcPathListStr + "'" + forcPaths[i] + "', "

	# Edit high resolution routing domain file.
	searchStr = "hydFile <- NULL"
	replaceStr = "hydFile <- " + "'" + dbIn.fullDomFile[indDbOrig] + "'"
	el(pathIn,searchStr,replaceStr)

	# Edit the geo file entry.
	searchStr = "geoFile <- NULL"
	replaceStr = "geoFile <- " + "'" + dbIn.geoFile[indDbOrig] + "'"
	el(pathIn,searchStr,replaceStr)

	# Edit mask file.
	if len(dbIn.mskFile[indDbOrig]) != 0:
		searchStr = "maskFile <- NULL"
		replaceStr = "maskFile <- " + "'" + dbIn.mskFile[indDbOrig] + "'" 
		el(pathIn,searchStr,replaceStr)

	# Edit tmp directory.
	searchStr = "tmpDir <- NULL"
	replaceStr = "tmpDir <- " + "'" + dbIn.topDir[indDbOrig] + "/tmp" + "'"
	el(pathIn,searchStr,replaceStr)

	# Place model directories and tag listings into namelist file
	searchStr = "modPathList <- NULL"
	replaceStr = "modPathList <- " + pathListStr
	el(pathIn,searchStr,replaceStr)
	searchStr = "modTagList <- NULL"
	replaceStr = "modTagList <- " + tagStr
	el(pathIn,searchStr,replaceStr)

	# Edit AMF Observations file.
	if len(dbIn.amfObsFile[indDbOrig]) != 0:
		searchStr = "AMFfile <- NULL"
		replaceStr = "AMFfile <- " + "'" + dbIn.amfObsFile[indDbOrig] + "'"
		el(pathIn,searchStr,replaceStr)

	# Edit SNOTEL Observations file.
	if len(dbIn.snotelObsFile[indDbOrig]) != 0:
		searchStr = "SNOfile <- NULL"
		replaceStr = "SNOfile <- " + "'" + dbIn.snotelObsFile[indDbOrig] + "'"
		el(pathIn,searchStr,replaceStr)

	# Edit HydroMet Observation file.
	if len(dbIn.metObsFile[indDbOrig]) != 0:
		searchStr = "METfile <- NULL" 
		replaceStr = "METfile <- " + "'" + dbIn.metObsFile[indDbOrig] + "'"
		el(pathIn,searchStr,replaceStr)

	# Edit all analysis/read dates in file.
	str1 = "', format='%Y-%m-%d %H:%M', tz='UTC')"
	str2 = "', format='%Y-%m-%d', tz='UTC')"
	if args.begADate is not None:
		begADateObj = rt(args.begADate)
                endADateObj = rt(args.endADate)
		begAStr1 = begADateObj.strftime("%Y-%m-%d")
		begAStr2 = begADateObj.strftime("%Y-%m-%d %H:%M")
	        endAStr1 = endADateObj.strftime("%Y-%m-%d")
		endAStr2 = endADateObj.strftime("%Y-%m-%d %H:%M")	
		
		searchStr = "readModStart <- NULL"
		replaceStr = "readModStart <- as.POSIXct('" + begAStr2 + str1
		el(pathIn,searchStr,replaceStr)

		searchStr = "readModEnd <- NULL"
		replaceStr = "readModEnd <- as.POSIXct('" + endAStr2 + str1
		el(pathIn,searchStr,replaceStr)

		searchStr = "readForcStart <- NULL"
		replaceStr = "readForcStart <- as.POSIXct('" + begAStr2 + str1
                el(pathIn,searchStr,replaceStr)

		searchStr = "readForcEnd <- NULL"
		replaceStr = "readForcEnd <- as.POSIXct('" + endAStr2 + str1
		el(pathIn,searchStr,replaceStr)

		searchStr = "readSnodasStart <- NULL"
		replaceStr = "readSnodasStart <- as.POSIXct('" + begAStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "readSnodasEnd <- NULL"
		replaceStr = "readSnodasEnd <- as.POSIXct('" + endAStr1 + str2 
		el(pathIn,searchStr,replaceStr)

		searchStr = "stdate_stats <- NULL"
		replaceStr = "stdate_stats <- as.POSIXct('" + begAStr2 + str1
		el(pathIn,searchStr,replaceStr)

		searchStr = "enddate_stats <- NULL"
		replaceStr = "enddate_stats <- as.POSIXct('" + endAStr2 + str1
		el(pathIn,searchStr,replaceStr)

		searchStr = "stdate_stats_sub <- NULL"
		replaceStr = "stdate_stats_sub <- as.POSIXct('" + begAStr2 + str1
		el(pathIn,searchStr,replaceStr)

		searchStr = "enddate_stats_sub <- NULL"
		replaceStr = "enddate_stats_sub <- as.POSIXct('" + endAStr2 + str1
		el(pathIn,searchStr,replaceStr)

	# Edit all plotting dates in file.
	if args.begPDate is not None:
		begPDateObj = rt(args.begPDate)
                endPDateObj = rt(args.endPDate)
                begPStr1 = begPDateObj.strftime("%Y-%m-%d")
                begPStr2 = begPDateObj.strftime("%Y-%m-%d %H:%M")
                endPStr1 = endPDateObj.strftime("%Y-%m-%d")
                endPStr2 = endPDateObj.strftime("%Y-%m-%d %H:%M")

		searchStr = "accflowStartDate <- NULL"
		replaceStr = "accflowStartDate <- as.POSIXct('" + begPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "accflowEndDate <- NULL"
		replaceStr = "accflowEndDate <- as.POSIXct('" + endPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "hydroStartDate <- NULL"
		replaceStr = "hydroStartDate <- as.POSIXct('" + begPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "hydroEndDate <- NULL"
		replaceStr = "hydroEndDate <- as.POSIXct('" + endPStr1 + str2

		searchStr = "accprecipStartDate <- NULL"
		replaceStr = "accprecipStartDate <- as.POSIXct('" + begPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "accprecipEndDate <- NULL"
		replaceStr = "accprecipEndDate <- as.POSIXct('" + endPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "flowsweStartDate <- NULL"
		replaceStr = "flowsweStartDate <- as.POSIXct('" + begPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "flowsweEndDate <- NULL"
		replaceStr = "flowsweEndDate <- as.POSIXct('" + endPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "flowlsmStartDate <- NULL"
		replaceStr = "flowlsmStartDate <- as.POSIXct('" + begPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "flowlsmEndDate <- NULL"
		replaceStr = "flowlsmEndDate <- as.POSIXct('" + endPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "sweStartDate <- NULL"
		replaceStr = "sweStartDate <- as.POSIXct('" + begPStr1 + str2
		el(pathIn,searchStr,replaceStr)
	
		searchStr = "sweEndDate <- NULL"
		replaceStr = "sweEndDate <- as.POSIXct('" + endPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "metStartDate <- NULL"
		replaceStr = "metStartDate <- as.POSIXct('" + begPStr1 + str2
		el(pathIn,searchStr,replaceStr)
	
		searchStr = "metEndDate <- NULL"
		replaceStr = "metEndDate <- as.POSIXct('" + endPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "snowBasStartDate <- NULL"
		replaceStr = "snowBasStartDate <- as.POSIXct('" + begPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "snowBasEndDate <- NULL"
		replaceStr = "snowBasEndDate <- as.POSIXct('" + endPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "snowMapBegDate <- NULL"
		replaceStr = "snowMapBegDate <- as.POSIXct('" + begPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "snowMapEndDate <- NULL"
		replaceStr = "snowMapEndDate <- as.POSIXct('" + endPStr1 + str2
		el(pathIn,searchStr,replaceStr)

		searchStr = "snowScatterBegDate <- NULL"
		replaceStr = "snowScatterBegDate <- as.POSIXct('" + begPStr1 + str2
		el(pathIn,searchStr,replaceStr)
	
		searchStr = "snowScatterEndDate <- NULL"
		replaceStr = "snowScatterEndDate <- as.POSIXct('" + endPStr1 + str2
		el(pathIn,searchStr,replaceStr)

	# Edit baseline namelist options corresponding to arguments passed in.
	if args.begADate is not None:
		begADateObj = rt(args.begADate)
        	endADateObj = rt(args.endADate)
        	begAStr1 = begADateObj.strftime("%Y%m%d%H%M")
		endAStr1 = endADateObj.strftime("%Y%m%d%H%M")

	strTmp = ''
	for i in range(0,numModIn):
               	if i == (numModIn - 1):
               		strTmp = strTmp + aliasTags[i]
               	else:
               		strTmp = strTmp + aliasTags[i] + "_"
	if args.lsmRead is not None:
		searchStr = "readMod <- FALSE"
		replaceStr = "readMod <- TRUE"
		el(pathIn,searchStr,replaceStr)

		if int(args.lsmRead) == 1:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMBAS_ALL.Rdata"
			searchStr = "readBasinLdasout <- FALSE"
                        replaceStr = "readBasinLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 2:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMBAS_SUB.Rdata"
			searchStr = "readBasinLdasout <- FALSE"
                        replaceStr = "readBasinLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutSUB <- FALSE"
			replaceStr = "varsLdasoutSUB <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 3:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMBAS_NFIE.Rdata"
			searchStr = "readBasinLdasout <- FALSE"
                        replaceStr = "readBasinLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutNFIE <- FALSE"
			replaceStr = "varsLdasoutNFIE <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 4:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMBAS_IOC.Rdata"
			searchStr = "readBasinLdasout <- FALSE"
                        replaceStr = "readBasinLdasout <- TRUE"
			searchStr = "varsLdasoutIOC0 <- FALSE"
			replaceStr = "varsLdasoutIOC0 <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 5:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMBAS_SNOW.Rdata"
			searchStr = "readBasinLdasout <- FALSE"
                        replaceStr = "readBasinLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutSNOW <- FALSE"
			replaceStr = "varsLdasoutSNOW <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 6:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMSNOTEL_ALL.Rdata"
			searchStr = "readSnoLdasout <- FALSE"
			replaceStr = "readSnoLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)	
		elif int(args.lsmRead) == 7:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMSNOTEL_SUB.Rdata"
                        searchStr = "readSnoLdasout <- FALSE"
                        replaceStr = "readSnoLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutSUB <- FALSE"
                        replaceStr = "varsLdasoutSUB <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 8:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMSNOTEL_NFIE.Rdata"
			searchStr = "readSnoLdasout <- FALSE"
                        replaceStr = "readSnoLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutNFIE <- FALSE"
                        replaceStr = "varsLdasoutNFIE <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 9:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMSNOTEL_IOC.Rdata"
			searchStr = "readSnoLdasout <- FALSE"
                        replaceStr = "readSnoLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutIOC0 <- FALSE"
                        replaceStr = "varsLdasoutIOC0 <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 10:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMSNOTEL_SNOW.Rdata"
			searchStr = "readSnoLdasout <- FALSE"
                        replaceStr = "readSnoLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutSNOW <- FALSE"
                        replaceStr = "varsLdasoutSNOW <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 11:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMAMF_ALL.Rdata"
			searchStr = "readAmfLdasout <- FALSE"
			replaceStr = "readAmfLdasout <- TRUE"
			el(pathIN,searchStr,replaceStr)
		elif int(args.lsmRead) == 12:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMAMF_SUB.Rdata"
			searchStr = "readAmfLdasout <- FALSE"
                        replaceStr = "readAmfLdasout <- TRUE"
                        el(pathIN,searchStr,replaceStr)
			searchStr = "varsLdasoutSUB <- FALSE"
                        replaceStr = "varsLdasoutSUB <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 13:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMAMF_NFIE.Rdata"
			searchStr = "readAmfLdasout <- FALSE"
                        replaceStr = "readAmfLdasout <- TRUE"
                        el(pathIN,searchStr,replaceStr)
			searchStr = "varsLdasoutNFIE <- FALSE"
                        replaceStr = "varsLdasoutNFIE <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 14:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMAMF_IOC.Rdata"
			searchStr = "readAmfLdasout <- FALSE"
                        replaceStr = "readAmfLdasout <- TRUE"
                        el(pathIN,searchStr,replaceStr)
			searchStr = "varsLdasoutIOC0 <- FALSE"
                        replaceStr = "varsLdasoutIOC0 <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 15:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMAMF_SNOW.Rdata"
			searchStr = "readAmfLdasout <- FALSE"
                        replaceStr = "readAmfLdasout <- TRUE"
                        el(pathIN,searchStr,replaceStr)
			searchStr = "varsLdasoutSNOW <- FALSE"
                        replaceStr = "varsLdasoutSNOW <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 16:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMMET_ALL.Rdata"
			searchStr = "readMetLdasout <- FALSE"
			replaceStr = "readMetLdasout <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 17:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMMET_SUB.Rdata"
			searchStr = "readMetLdasout <- FALSE"
                        replaceStr = "readMetLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutSUB <- FALSE"
                        replaceStr = "varsLdasoutSUB <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 18:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMMET_NFIE.Rdata"
			searchStr = "readMetLdasout <- FALSE"
                        replaceStr = "readMetLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutNFIE <- FALSE"
                        replaceStr = "varsLdasoutNFIE <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 19:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMMET_IOC.Rdata"
			searchStr = "readMetLdasout <- FALSE"
                        replaceStr = "readMetLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutIOC0 <- FALSE"
                        replaceStr = "varsLdasoutIOC0 <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.lsmRead) == 20:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_LSMMET_SNOW.Rdata"
			searchStr = "readMetLdasout <- FALSE"
                        replaceStr = "readMetLdasout <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "varsLdasoutSNOW <- FALSE"
                        replaceStr = "varsLdasoutSNOW <- TRUE"
                        el(pathIn,searchStr,replaceStr)

		modPathOut = "'" + dbIn.topDir[indDbOrig] + "/" + dbIn.alias[indDbOrig] + \
                             "/analysis_out/read_datasets/" + modFileOut + "'"
		searchStr = "modReadFileOut <- NULL"
		replaceStr = "modReadFileOut <- " + modPathOut
		el(pathIn,searchStr,replaceStr)
	
	if args.rtRead is not None:
		searchStr = "readMod <- FALSE"
                replaceStr = "readMod <- TRUE"
                el(pathIn,searchStr,replaceStr)	
		if int(args.rtRead) == 1:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_RTBAS.Rdata"
			searchStr = "readBasinRtout <- FALSE"
			replaceStr = "readBasinRtout <- TRUE"
			el(pathIn.searchStr,replaceStr)
		modPathOut = "'" + dbIn.topDir[indDbOrig] + "/" + dbIn.alias[indDbOrig] + \
                             "/analysis_out/read_datasets/" + modFileOut + "'"
		searchStr = "modReadFileOut <- NULL"
                replaceStr = "modReadFileOut <- " + modPathOut
                el(pathIn,searchStr,replaceStr)

	if args.gwRead is not None:
		searchStr = "readMod <- FALSE"
                replaceStr = "readMod <- TRUE"
                el(pathIn,searchStr,replaceStr)
		if int(args.gwRead) == 1:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_GWOUT.Rdata"
			searchStr = "readGwout <- FALSE"
			replaceStr = "reaGwout <- TRUE"
			el(pathIn.searchStr,replaceStr)
               	modPathOut = "'" + dbIn.topDir[indDbOrig] + "/" + dbIn.alias[indDbOrig] + \
                             "/analysis_out/read_datasets/" + modFileOut + "'"
		searchStr = "modReadFileOut <- NULL"
                replaceStr = "modReadFileOut <- " + modPathOut
                el(pathIn,searchStr,replaceStr)

	if args.fxRead is not None:
		searchStr = "readMod <- FALSE"
                replaceStr = "readMod <- TRUE"
                el(pathIn,searchStr,replaceStr)
		if int(args.fxRead) == 1:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_FRXST.Rdata"
			searchStr = "readFrxstout <- FALSE"
			replaceStr = "readFrxstout <- TRUE"
			el(pathIn.searchStr,replaceStr)
		modPathOut = "'" + dbIn.topDir[indDbOrig] + "/" + dbIn.alias[indDbOrig] + \
                             "/analysis_out/read_datasets/" + modFileOut + "'"
		searchStr = "modReadFileOut <- NULL"
                replaceStr = "modReadFileOut <- " + modPathOut
                el(pathIn,searchStr,replaceStr)

	if args.chRead is not None:
		searchStr = "readMod <- FALSE"
                replaceStr = "readMod <- TRUE"
                el(pathIn,searchStr,replaceStr)
		if int(args.chRead) == 1:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_CHRTOUT_ALL.Rdata"
			searchStr = "readChrtout <- FALSE"
			replaceStr = "readChrtout <- TRUE"
			el(pathIn.searchStr,replaceStr)
		elif int(args.chRead) == 2:
			modFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_CHRTOUT_GAGES.Rdata"
			searchStr = "readChrtout <- FALSE"
                        replaceStr = "readChrtout <- TRUE"
                        el(pathIn.searchStr,replaceStr)
			if len(dbIn.link2GageFile[indDbOrig]) == 0:
				print "ERROR: A CHRTOUT read option was selected without the"
				print "       necessary link2gages table."
				raise
			searchStr = "readLink2gage <- NULL"
			replaceStr = "readLink2gage <- read.table('" + dbIn.link2GageFile[indDbOrig] + \
				     "', sep='\t', header=TRUE)"
		modPathOut = "'" + dbIn.topDir[indDbOrig] + "/" + dbIn.alias[indDbOrig] + \
			     "/analysis_out/read_datasets/" + modFileOut + "'"
		searchStr = "modReadFileOut <- NULL"
                replaceStr = "modReadFileOut <- " + modPathOut
                el(pathIn,searchStr,replaceStr)

	if args.forRead is not None:
		searchStr = "readForc <- FALSE"
		replaceStr = "readForc <- TRUE"
		el(pathIn,searchStr,replaceStr)

		searchStr = "forcPathList <- NULL"
		replaceStr = "forcPathList <- " + forcPathListStr
		el(pathIn,searchStr,replaceStr)

		searchStr = "forcTagList <- NULL"
		replaceStr = "forcTagList <- " + tagStr
		el(pathIn,searchStr,replaceStr)
 
		if int(args.forRead) == 1:
			forcFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_FORC_BASREAD.Rdata"
			searchStr = "readBasinLdasin <- FALSE"
			replaceStr = "readBasinLdasin <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.forRead) == 2:
			forcFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_FORC_SNOTELREAD.Rdata"
			searchStr = "readSnoLdasin <- FALSE"
			replaceStr = "readSnoLdasin <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.forRead) == 3:
			forcFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_FORC_AMFREAD.Rdata"
			searchStr = "readAmfLdasin <- FALSE"
			replaceStr = "readAmfLdasin <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.forRead) == 4:
			forcFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_FORC_METREAD.Rdata"
			searchStr = "readMetLdasin <- FALSE"
			replaceStr = "readMetLdasin <- TRUE"
			el(pathIn,searchStr,replaceStr)

		forcPathOut = "'" + dbIn.topDir[indDbOrig] + "/" + dbIn.alias[indDbOrig] + \
			      "/analysis_out/read_datasets/" + forcFileOut + "'" 
		searchStr = "forcReadFileOut <- NULL"
		replaceStr = "forcReadFileOut <- " + forcPathOut
		el(pathIn,searchStr,replaceStr)
		
	if args.snRead is not None:
		searchStr = "readSnodas <- FALSE"
		replaceStr = "readSnodas <- TRUE"
		el(pathIn,searchStr,replaceStr)

		strTmpSn = "c('" + dbIn.snodasPath[indDbOrig] + "')"
		searchStr = "snodasPathList <- NULL"
		replaceStr = "snodasPathList <- " + strTmpSn
		el(pathIn,searchStr,replaceStr)

		searchStr = "snodasTagList <- NULL"
		replaceStr = "snodasTagList <- c('SNEQV')"
		el(pathIn,searchStr,replaceStr)

		if int(args.snRead) == 1:
			snFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_SNODAS_BASIN.Rdata"
			searchStr = "readBasinSnodas <- FALSE"
			replaceStr = "readBasinSnodas <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.snRead) == 2:
			snFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_SNODAS_SNOTEL.Rdata"
			searchStr = "readSnoSnodas <- FALSE"
			replaceStr = "readSnoSnodas <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.snRead) == 3:
			snFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_SNODAS_AMF.Rdata"
			searchStr = "readAmfSnodas <- FALSE"
			replaceStr = "readAmfSnodas <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.snRead) == 4:
			snFileOut = begAStr1 + "_" + endAStr1 + "_" + strTmp + "_SNODAS_MET.Rdata"
			searchStr = "readMetSnodas <- FALSE"
			replaceStr = "readMetSnodas <- TRUE"
			el(pathIn,searchStr,replaceStr)

		snPathOut = "'" + dbIn.topDir[indDbOrig] + "/" + dbIn.alias[indDbOrig] + \
                            "/analysis_out/read_datasets/" + snFileOut + "'"
		searchStr = "snodasReadFileOut <- NULL"
		replaceStr = "snodasReadFileOut <- " + snPathOut
		el(pathIn,searchStr,replaceStr)

	if args.stat is not None:
		searchStr = "calcStats <- FALSE"
		replaceStr = "calcStats <- TRUE"
		el(pathIn,searchStr,replaceStr)
		
		if int(args.stat) == 1:
			searchStr = "basSnoProc <- FALSE"
			replaceStr = "basSnoProc <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.stat) == 2:
			searchStr = "snoProc <- FALSE"
			replaceStr = "snoProc <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.stat) == 3:
			searchStr = "amfProc <- FALSE"
			replaceStr = "amfProc <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.stat) == 4:
			searchStr = "metProc <- FALSE"
			replaceStr = "metProc <- TRUE"
			el(pathIn,searchStr,replaceStr)

	if args.plot is not None:
		searchStr = "createPlots <- FALSE"
		replaceStr = "createPlots <- TRUE"
		el(pathIn,searchStr,replaceStr)

		searchStr = "writePlotDir <- NULL"
		replaceStr = "writePlotDir <- '" + dbIn.topDir[indDbOrig] + "/" + \
			     dbIn.alias[indDbOrig] + "/analysis_out/plotting'"
		el(pathIn,searchStr,replaceStr)

		if int(args.plot) == 1:
			searchStr = "accflowTags <- NULL"
			replaceStr = "accflowTags <- " + tagStr
			el(pathIn,searchStr,replaceStr)
			searchStr = "accflowPlot <- FALSE"
			replaceStr = "accflowPlot <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 2:
			searchStr = "hydroTags <- NULL"
			replaceStr = "hydroTags <- " + tagStr
			el(pathIn,searchStr,replaceStr) 
			searchStr = "hydroPlot <- FALSE"
                        replaceStr = "hydroPlot <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 3:
			searchStr = "hydroTags2 <- NULL" 
			replaceStr = "hydroTags2 <- " + tagStr
			el(pathIn,searchStr,replaceStr)
			searchStr = "hydroEnsPlot <- FALSE"
                        replaceStr = "hydroEnsPlot <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 4:
			searchStr = "accprecipTags <- NULL"
			replaceStr = "accprecipTags <- " + tagStr
			el(pathIn,searchStr,replaceStr)
			searchStr = "accprecipPlot <- FALSE"
                        replaceStr = "accprecipPlot <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 5:
			searchStr = "flowsweTags <- NULL" 
			replaceStr = "flowsweTags <- " + tagStr
			el(pathIn,searchStr,replaceStr)
			searchStr = "flowswePlot <- FALSE"
                        replaceStr = "flowswePlot <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 6:
			searchStr = "flowlsmTags <- NULL"
		 	replaceStr = "flowlsmTags <- " + tagStr
			el(pathIn,searchStr,replaceStr)	
			searchStr = "flowlsmPlot <- FALSE"
                        replaceStr = "flowlsmPlot <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 7:
			searchStr = "sweTags <- NULL"
			replaceStr = "sweTags <- " + tagStr
			el(pathIn,searchStr,replaceStr)
			searchStr = "swePlot <- FALSE"
                        replaceStr = "swePlot <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 8:
			searchStr = "metTags <- NULL"
			replaceStr = "metTags <- " + tagStr
			el(pathIn,searchStr,replaceStr)
			searchStr = "metPlot <- FALSE"
                        replaceStr = "metPlot <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 9:
			ioMgmntMod.snodasBasCheck(indDbOrig,begPDateObj,endPDateObj,pathIn,args,dbIn,(strTmp + "_SNODAS_BASIN.Rdata"))
			searchStr = "snowBasinPlot <- FALSE"
                        replaceStr = "snowBasinPlot <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 10:
			searchStr = "strBiasTags <- NULL"
			replaceStr = "strBiasTags <- " + tagStr
			el(pathIn,searchStr,replaceStr)
			searchStr = "strBiasMap <- FALSE"
                        replaceStr = "strBiasMap <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 11:
			searchStr = "strCorrTags <- NULL"
			replaceStr = "strCorrTags <- " + tagStr
			el(pathIn,searchStr,replaceStr)
			searchStr = "strCorrMap <- FALSE"
                        replaceStr = "strCorrMap <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 12:
			searchStr = "snosweErrTags <- NULL"
			replaceStr = "snowsweErrTags <- " + tagStr
			el(pathIn,searchStr,replaceStr)
			searchStr = "snosweErrMap <- FALSE"
                        replaceStr = "snosweErrMap <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 13:
			searchStr = "snoprecipErrTags <- NULL"
			replaceStr = "snoprecipErrTags <- " + tagStr
			el(pathIn,searchStr,replaceStr)
			searchStr = "snoprecipErrMap <- FALSE"
                        replaceStr = "snoprecipErrMap <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 14:
			searchStr = "snodasErrorMap <- FALSE"
                        replaceStr = "snodasErrorMap <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 15:
			searchStr = " peakSweMap<- FALSE"
                        replaceStr = "peakSweMap <- TRUE"
                        el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 16:
			searchStr = "snowPointScatter <- FALSE"
                        replaceStr = "snowPointScatter <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "snotelScatter <- FALSE"
			replaceStr = "snotelScatter <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 17:
			searchStr = "snowPointScatter <- FALSE"
                        replaceStr = "snowPointScatter <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "metScatter <- FALSE"
			replaceStr = "metScatter <- TRUE"
			el(pathIn,searchStr,replaceStr)
		elif int(args.plot) == 18: 
			searchStr = "snowPointScatter <- FALSE"
                        replaceStr = "snowPointScatter <- TRUE"
                        el(pathIn,searchStr,replaceStr)
			searchStr = "basinScatter <- FALSE"
			replaceStr = "basinScatter <- TRUE"
			el(pathIn,searchStr,replaceStr)

###################################################
##  Main Control Script to process model output  ##
###################################################

###########################################################################################
## RUN (do not change anything below this line)

library(rwrfhydro)
library(data.table)
library(scales)

if (!is.null(maskFile)){
	load(maskFile)
}
source("util_FUNC.R")

# If reach routing was specified but no rtlinks object exists,
# read it in from the routeLinksFile.
if (reachRting == TRUE){
	if (is.null(routeLinkFile)){
		if (is.null(rtLinks)){
			stop(paste('ERROR: No route link file specified and rtLinks object does not exist'))
	  	}
	} else {
		rtLinks <- ReadRouteLink(routeLinkFile)
	}
}	

# If subsetting of basins has been enabled, subset basins/frxst points
# immediately before anything else is done.
if (exists("basinSub") & exists("frxstPts") & !is.null(basinSub)){
  listSubBasin <- subsetBasins(basinSub,
			       mskgeo.nameList, 
 		               frxstPts, 
			       basin2gageList, 
			       gage2basinList,
                               mskgeo.areaList, 
			       mskgeo.countInds, 
			       mskgeo.List, 
			       mskgeo.maxInds,
                               mskgeo.minInds, 
			       mskhyd.areaList, 
			       mskhyd.countInds,
                               mskhyd.List, 
			       mskhyd.maxInds, 
			       mskhyd.minInds,
                               mskhyd.nameList,
			       stid2gageList)
  mskgeo.nameList <- listSubBasin[[1]]
  frxstPts <- listSubBasin[[2]]
  basin2gageList <- listSubBasin[[3]]
  gage2basinList <- listSubBasin[[4]]
  mskgeo.areaList <- listSubBasin[[5]]
  mskgeo.countInds <- listSubBasin[[6]]
  mskgeo.List <- listSubBasin[[7]]
  mskgeo.maxInds <- listSubBasin[[8]]
  mskgeo.minInds <- listSubBasin[[9]]
  mskhyd.areaList <- listSubBasin[[10]]
  mskhyd.countInds <- listSubBasin[[11]]
  mskhyd.List <- listSubBasin[[12]]
  mskhyd.maxInds <- listSubBasin[[13]]
  mskhyd.minInds <- listSubBasin[[14]]
  mskhyd.nameList <- listSubBasin[[15]]
  stid2gageList <- listSubBasin[[16]]
}

if (exists("basinSub") & !exists("frxstPts") & !exists("mskhyd.nameList") & exists("mskgeo.nameList") & !is.null(basinSub)){
  listSubBasin <- subsetRegions(basinSub,
                                mskgeo.nameList,
                                basin2gageList,
                                mskgeo.areaList,
                                mskgeo.countInds,
                                mskgeo.List,
                                mskgeo.maxInds,
                                mskgeo.minInds)
  mskgeo.nameList <- listSubBasin[[1]]
  basin2gageList <- listSubBasin[[2]]
  mskgeo.areaList <- listSubBasin[[3]]
  mskgeo.countInds <- listSubBasin[[4]]
  mskgeo.List <- listSubBasin[[5]]
  mskgeo.maxInds <- listSubBasin[[6]]
  mskgeo.minInds <- listSubBasin[[7]]
}
# Model Reads 
if (readMod | readForc) {
        source("read_MODELOUT.R")
}

# Obs
if (calcStats | createPlots) {
	if (!is.null(AMFfile) & amfProc & exists("ptgeo.amf")) {
		if (file.exists(AMFfile)) {
			load(AMFfile)
			obsAmfData <- subset(obsAmfData, obsAmfData$site_id %in% ptgeo.amf$id)
		} else {
			stop(paste("Ameriflux obs file specified but does not exist:", AMFfile))
		}
	}
	if (!is.null(SNOfile) & (snoProc | indSwePlot) & exists("ptgeo.sno")) {
        	if (file.exists(SNOfile)) {
                	load(SNOfile)
			obsSnoData <- subset(obsSnoData, obsSnoData$site_id %in% ptgeo.sno$id)
		} else {
                	stop(paste("SNOTEL obs file specified but does not exist:", SNOfile))
       		}
	}
	if (!is.null(METfile) & metProc & exists("ptgeo.met")) {
        	if (file.exists(METfile)) {
                	load(METfile)
			obsMetData <- subset(obsMetData, obsMetData$site_id %in% ptgeo.met$id)
		} else {
                	stop(paste("MET obs file specified but does not exist:", METfile))
        	}
	}
	if (!is.null(STRfile) & (strProc | accflowPlot | hydroPlot | hydroEnsPlot| flowswePlot | flowlsmPlot) ) {
		obsStrData_FINAL <- data.frame()
		#obsStrMeta_FINAL <- data.frame()
		# Gridded routing case w/ subset
		if ( !reachRting & exists("stid2gageList") ) {
			if (is.list(stid2gageList)) {
				gageList <- data.frame(st_id=names(stid2gageList), site_no=unlist(stid2gageList), stringsAsFactors=FALSE)
			} else {
				gageList <- stid2gageList
			}
		# Reach routing case w/ subset
		} else if (reachRting) {
			if ( exists("statsLink2gage") & !is.null(statsLink2gage) ) {
				gageList <- statsLink2gage[,c("link","site_no")]
			} else {
				gageList <- subset(rtLinks[,c("link","site_no")], !(rtLinks$site_no == ''))
			}
                } else if (readLink2gage) {
                        gageList <- readLink2gage
		# No subset
		} else {
			gageList <- NULL
		}
		for (i in STRfile) {
			if (file.exists(i)) {
				load(i)
				if (exists("obsStrData.map")) obsStrData <- remapData(obsStrData, obsStrData.map)
				if (exists("obsStrMeta.map")) obsStrMeta <- remapData(obsStrMeta, obsStrMeta.map)
				if ( !is.null(gageList) ) { 
					obsStrData_TMP <- subset(obsStrData, obsStrData$site_no %in% unique(gageList$site_no))
					#obsStrMeta_TMP <- subset(obsStrMeta, obsStrMeta$site_no %in% unique(gageList$site_no))
				} else {
					obsStrData_TMP <- obsStrData
					#obsStrMeta_TMP <- obsStrMeta
				}
				obsStrData_FINAL <- plyr::rbind.fill(obsStrData_FINAL, obsStrData_TMP)
                        	#obsStrMeta_FINAL <- plyr::rbind.fill(obsStrMeta_FINAL, obsStrMeta_TMP)
			} else {
				stop(paste("Streamflow obs file specified but does not exist:", STRfile))
			}
		}
		obsStrData <- obsStrData_FINAL
		#obsStrMeta <- obsStrMeta_FINAL
		if ( reachRting & !is.null(gageList) ) {
			obsStrData <- plyr::join(obsStrData, gageList, by="site_no")
		}
		rm(obsStrData_FINAL, obsStrMeta_FINAL, obsStrData_TMP, obsStrMeta_TMP)
	}
}

# Read in basin Snow/SNODAS
if (readSnodas & readBasinSnodas) {
        # Load necessary mask data to perform analysis
        source("read_BASIN_SNOW.R")
}

# Read in point SNODAS
if (readSnodas & (readSnoSnodas | readAmfSnodas | readMetSnodas)) {
	# Load necessary mask data to perform reads
	source("read_SNODAS.R")
}

# Stats Calculations
if (calcStats & (strProc | snoProc | amfProc | metProc)) {
	message("Calculating stats")
	if (is.null(modReadFileOut)) {
		if (file.exists(modReadFileIn)) {
			load(modReadFileIn)
		}
	} else {
		if (is.null(modReadFileIn)) {
			if (file.exists(modReadFileOut)) {
				load(modReadFileOut)
			}
		} else {
			if (file.exists(modReadFileIn)) {
				load(modReadFileIn)
			}
		}
	}
	if (metProc) {
        	if (is.null(forcReadFileOut)) {
                	if (file.exists(forcReadFileIn)) {
                        	load(forcReadFileIn)
                	}
        	} else {
                	if (is.null(forcReadFileIn)) {
                        	if (file.exists(forcReadFileOut)) {
                                	load(forcReadFileOut)
                        	}
                	} else {
                        	if (file.exists(forcReadFileIn)) {
                                	load(forcReadFileIn)
                        	}
                	}
        	}
	}
	source("calc_PERFSTATS.R")
}

# Basin snow analysis 
if (calcStats & basSnoProc) {
	# Load basin snow file 
	load(snodasReadFileOut)
	# Load model output (necessary for total accumulated streamflow).
	source("calc_BASIN_SNOW.R")
}

# Plots
if (createPlots) {
	if (accflowPlot | hydroPlot | hydroEnsPlot | accprecipPlot | 
			flowswePlot | flowlsmPlot | indSwePlot | 
			strBiasMap | strCorrMap | 
			snosweErrMap | snoprecipErrMap) {
        	message("Generating plots")
		if (strBiasMap | strCorrMap | snosweErrMap | snoprecipErrMap) {
			load(statsFileOut)
		}
        	if (is.null(modReadFileOut)) {
                	if (file.exists(modReadFileIn)) {
                        	load(modReadFileIn)
                	}
        	} else {
                	if (is.null(modReadFileIn)) {
                        	if (file.exists(modReadFileOut)) {
                                	load(modReadFileOut)
                        	}
                	} else {
                        	if (file.exists(modReadFileIn)) {
                                	load(modReadFileIn)
                        	}
                	}
        	}
	}
	if (basSnoEnsPlot) {
		if (file.exists(modReadFileIn)) {
			load(modReadFileIn)
		}
	}
	if (metPlot) {
                if (is.null(forcReadFileOut)) {
                        if (file.exists(forcReadFileIn)) {
                                load(forcReadFileIn)
                        }
                } else {
                        if (is.null(forcReadFileIn)) {
                                if (file.exists(forcReadFileOut)) {
                                        load(forcReadFileOut)
                                }
                        } else {
                                if (file.exists(forcReadFileIn)) {
                                        load(forcReadFileIn)
                                }
                        }
                }
        }
	if (snowBasinPlot) {
		if (file.exists(snowBasDataFile)) {
			load(snowBasDataFile)
		}
	}
	if (snowPointScatter) {
		# Load model data in for points
		if (file.exists(modReadFileIn)){
			load(modReadFileIn)
		}

		# Load necessary files for SNOTEL analysis
		if (!is.null(SNOfile) & snotelScatter) {
			if (file.exists(SNOfile)){
				load(SNOfile)
			}
			if (file.exists(snodasSNOfile)){
				load(snodasSNOfile)
			}
		}

		# Load necessary files for Hydro-met analysis
		if (metScatter) {
			if (file.exists(METfile)) {
				load(METfile)
			}
			if (file.exists(snodasMETfile)){
				load(snodasMETfile)
			}
		}

		# Load necessary files for basin-agreggation analysis
		if (!is.null(SNOfile) & basinScatter){
			if (file.exists(SNOfile)){
				load(SNOfile)
			}
			if (file.exists(snodasSNOfile)){
				load(snodasSNOfile)
			}
		}
	}
	if (snotelAccPcpPlot) {
		if (file.exists(modReadFileIn)) {
			load(modReadFileIn)
		}
		if (file.exists(SNOfile)) {
			load(SNOfile)
		}
	}

        source("calc_PLOTS.R")
}

# EXIT
#quit("no")


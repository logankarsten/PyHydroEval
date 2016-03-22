# Parent program file to run various analysis/plotting options
# on WRF-Hydro output using R and rwrfhydro toolsets. Prior
# use of the other Python functions to setup and manage 
# a menu of model options is required before running this 
# program.

# Logan Karsten
# National Center for Amtmospheric Research
# Research Applications Laboratory

import sys
sys.path.insert(0, './python')

import subprocess
import argparse
import os
import pyHydroEvalUtils
import datetime
import compileNamelist

def main(argv):
	# Parse arguments passed in.
	parser = argparse.ArgumentParser(description='Main program to run analysis on WRF-Hydro')
	parser.add_argument('modelProjects', metavar='alias', type=str, nargs='+',
			    help='A list of model projects to run analysis on')
	parser.add_argument('--begADate',nargs='?', help='Beginning Date for Analysis/Read in YYYYMMDDHH Format')
	parser.add_argument('--endADate',nargs='?', help='Ending Date for Analysis/Read in YYYYMMDDHH Format')
	parser.add_argument('--lsmRead',nargs='?', help='LDASOUT Read Flag (1-20)')
	parser.add_argument('--rtRead',nargs='?', help='RTOUT Read Flag (1-1)')
	parser.add_argument('--gwRead',nargs='?', help='GWOUT Read Flag (1-1)')
	parser.add_argument('--fxRead',nargs='?', help='frxst Read Flag (1-1)')
	parser.add_argument('--chRead',nargs='?', help='CHRTOUT Read Flag (1-1)')
	parser.add_argument('--forRead',nargs='?', help='Forcing Read Flag (1-4')
	parser.add_argument('--snRead',nargs='?', help='SNODAS Read Flag (1-4)')
	parser.add_argument('--stat',nargs='?', help='Statistics Performance Flag (1-5)')
	parser.add_argument('--plot',nargs='?', help='Plot Flag (1-18)')
	parser.add_argument('--begPDate',nargs='?', help='Beginning Date for Plotting in YYYYMMDDHH Format')
	parser.add_argument('--endPDate',nargs='?', help='Ending Date for Plotting in YYYYMMDDHH Format')

	args = parser.parse_args()

 	# Check to make sure arguments make sense
	try:	
		pyHydroEvalUtils.checkArgs(args)
	except:
		print "ERROR: Improper arguments passed."
		sys.exit(1)

	# Convert date arguments to datetime objects. Also check to ensure dates make sense.
	if args.begADate or args.endADate:
		begADateObj = pyHydroEvalUtils.returnDate(args.begADate)
		endADateObj = pyHydroEvalUtils.returnDate(args.endADate)
		if begADateObj >= endADateObj:
			print "ERROR: Beginning analysis date must be less than ending date."
			sys.exit(1)
	if args.begPDate or args.endPDate: 
		begPDateObj = pyHydroEvalUtils.returnDate(args.begPDate)
                endPDateObj = pyHydroEvalUtils.returnDate(args.endPDate)
		if begPDateObj >= endPDateObj:
			print "ERROR: Beginning Plotting date must be less than ending date."
			sys.exit(1)

	# Check to ensure proper files exist to run analysis
	dbPath = "./parm/modelMeta_db.pkl"
	namelistTemplate = "./parm/namelist_template.R"

	if not os.path.isfile(dbPath):
		print "ERROR: Database: " + dbPath + " not found."
		sys.exit(1)
	if not os.path.isfile(namelistTemplate):
		print "ERROR: Template R namelist file: " + namelistTemplate + " not found."
		sys.exit(1)

	# Read in model project database
	try:
		db = pyHydroEvalUtils.readDb(dbPath)
	except:
		print "ERROR: Model project database failed to read in."
		sys.exit(1)

	# Check that alias names passed in are present in model project database
	try:
		pyHydroEvalUtils.checkDb(args,db)
	except:
		print "ERROR: checkDb failed."
		sys.exit(1)

	# Copy template namelist file to namelist directory in model project directory.
	# If multiple model projects have been chosen for cross-model validation, 
	# original namelist will be placed in first model project listed, with symbolic
	# links in remaining model projects.
	try:
		namePath, nameLink = pyHydroEvalUtils.initNamelist(args,db)
	except:
		print "ERROR: Failure to initialize R namelist file."
		sys.exit(1)

	# Begin editing R namelist file
	try:
 		compileNamelist.editNamelist(namePath,args,db)
	except: 
		print "ERROR: Failure to compile R namelist file."
	        os.unlink(nameLink)	
		sys.exit(1)	


	# Run Rscript command to perform analysis. Pipe stdout and stderr
	# to text files for users to inspect after analysis job complete.
	stdOutPath = "./stdout_" + str(os.getpid()) + ".txt"
	stdErrPath = "./stdout_" + str(os.getpid()) + ".txt"

        cmd = "Rscript " + nameLink
	subprocess.call(cmd,shell=True)	
	#try:
	#	fOut = open(stdOutPath,"w")
	#	fErr = open(stdErrPath,"w")
	#	subprocess.call(cmd,stdout=fOut,stderr=fErr)
	#except:
	#	print "ERROR: Failure to execute/run analysis."
	#	print "ERROR: Please see output diagnostic files."
	#	subprocess.call("rm *.R",shell=True)
	#	os.unlink(nameLink)
	#	sys.exit(1)

	# Remove namelist link specific to processor ID
	try:
		os.unlink(nameLink)
	except:
		print "ERROR: Failure to remove link: " + nameLink
		sys.exit(1)

	# Remove temporary links to R scripts
	#cmd4 = "rm *.R"
	
	#try:
	#	subprocess.call(cmd4,shell=True)
	#except:
	#	print "ERROR: Failure to remove symbolic links to R scripts."
	#	sys.exit(1)	
if __name__ == "__main__":
	main(sys.argv[1:])

# Program to remove model projects from database
# and associated output directory. PLEASE USE
# WITH CAUTION!!!!


# Logan Karsten
# National Center for Atmopsheric Research
# Research Applications Laboratory

import setupMod
import os
import sys

argIn = sys.argv

dbPath = "./modelMeta_db.pkl"
if not os.path.isfile(dbPath):
	print "ERROR: Database file: " + dbPath + " not found."
	sys.exit(1)

if len(argIn) != 2:
	print "ERROR: Incorrect number of arguments passed."
	sys.exit(1)

setupMod.removeModelProject(argIn[1])

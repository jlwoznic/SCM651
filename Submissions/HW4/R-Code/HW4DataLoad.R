# 
# Course: SCM 651
# Name: Team 73
# Homework #4
# Project Code: Load and Manipulate Data
# Due Date: 06/02/2019
# Date Submitted:
#
# read in a dataset so that it can be useful.
# use this package to read in a XLS file

# for PC
#ubData <- read.csv("C:/Users/Joyce/Desktop/Syracuse/SCM651/Submissions/HW4/scm651_homework_4_universal_bank.csv")
# for Mac
ubData <- read.csv("/Users/joycewoznica/Desktop/Personal/Syracuse/scm651_homework_4_universal_bank.csv")

# this is excellent! from Hsmic
bankDF <- as.data.frame(ubData)
# view as a table
View(bankDF)


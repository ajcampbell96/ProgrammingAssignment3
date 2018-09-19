# ProgrammingAssignment3
Programming Assignment 3 for R Programming in the Data Science Specialization on Coursera.

## Given Files
The following files were supplied as part of the programming assignment:
* Hospital_Revised_Flatfiles.pdf
* ProgrammingAssignment3.pdf
* outcome-of-care-measures.csv

## best.R
best.R returns the name of the hospital with the lowest mortality rate for a given state and outcome.
The only possible outcomes are "heart attack", "heart failure", and "pneumonia". The state given must be abbreviated and capitalized, for example: Pennsylvania is "PA".

## rankhospital.R
rankhospital.R returns the name of the hospital with a certain rank for an outcome. It takes an outcome, state, and number, where the number indicates which rank you want to see. Number can take "best", "worst", or an integer such as 2, 3, or 4.
The only possible outcomes are "heart attack", "heart failure", and "pneumonia". The state given must be abbreviated and capitalized, for example: Pennsylvania is "PA".

## rankall.R
rankall.R returns the hospitals with a particular rank in all states for a particular outcome. It utilizes rankhospital.R to has the same parameter structure as rankhospital.R.

## findhospitalrank.R
findhospitalrank.R returns the ranks of each outcome for a given hospital. For example: "Hopkins" will return all hospitals with "Hopkins" in their name, along with where they rank for heart attacks, heart failures, and pneumonia.

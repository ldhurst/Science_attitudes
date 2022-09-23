# Science_attitudes
 Scripts and dataset for science attitude analysis
 
 The folder scripts is set into two partitions, recoding_scripts and analysis_scripts.  
 
 The recoding scripts parse the original survey spread sheet (in spss format).  One excludes questions that are not biological( survey_recoding_bioknowdig.R) one incldes all questions (survey_recoding_sciknowdig.R).  The produce corresponding recoded files, ending biodig and dig respectively.  These are in puts for the analysis.
 
 The analysis scripts are split into two folders, one for the analysis of the data with only biological questions (bio_know)), one for all questions (sci_know)).  Each folder contains the necessary input files and an R script to generate all figures and tables in the manuscript and its supplements.
 
 The script will import packages needed if your computer doesn't already have them.  Some systems may require appropriate permissions to do this.

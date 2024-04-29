# Introduction
This repository contains data and code for the research paper:

>Exploratory scoping of place-based opportunities for convergence research. Casey Helgesona, Lisa Auermuller, DeeDee Bennett Gayle, S&#246;onke Dangendorf, Elisabeth A. Gilmore, Klaus Keller, Robert E. Kopp, Jorge Lorenzo-Trueba, Michael Oppenheimer, Kathleen Parrish, Victoria Ramenzoni, Nancy Tuana, and Thomas Wahl.

The point of sharing this repository is to make the analysis and visualization in the paper reproducible. The repository contains data files and R files. The R code reads the data and makes figures. The data sets are very small (<30 KB total). The analysis is minimal, and the code is primarily for visualization. 

# Contents
+ **data1.csv** 
+ **plots1.R**
+ **data2.csv** 
+ **plots2.R** 

The .csv files contain scores from an expert elicitation. Experts entered scores into a Google Form, and those scores were downloaded as .csv files. The R files contain code for analysis and visualization of data in the .csv files. "plots1.R" uses "data1.csv" and "plots2.R" uses "data2.csv". Running the R files generates the figures in the paper and puts them in the "outputs" subdirectory. The files can be run in either order and need not be run in the same R session.


# 
Created by Casey Helgeson (ORCID iD 0000-0001-5333-9954).
This work is supported by the National Science Foundation as part of the Megalopolitan Coastal Transformation Hub (MACH) under NSF award ICER-2103754. Additional funding from Dartmouth College and the Penn State Center for Climate Risk Management (CLIMA).

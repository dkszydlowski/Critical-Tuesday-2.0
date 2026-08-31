# Critical-Tuesday-2.0
Updated code for calculating the resilience of Tuesday Lake across gradients of nutrient enrichment experiments and staining.

The code for this manuscript contains four main folders: data, scripts, figures, and results.

**Data:** Within the data folder there are folders for “unformatted data” and “formatted data.” The unformatted data are original data files that will be archived on the Environmental Data Initiative. The formatted data are data for analysis that were formatted using the scripts in this repository, or that were already ready for analysis.

**Scripts:** The scripts folder has subfolders for the different steps of analysis and are numbered in the order they should be executed: 0.) data cleaning and formatting 1.) standardize with DLM 2.) ADF tests 3.) Fit DDJ models 4.) calculate passage times 5.) bootstrap results 6.) DLM for critical transitions. The “FIGURES” folder has scripts for generating each of the figures in the main text and the supplemental. The “extras” folder has a script for checking correlations between variables. All of the subfolders have scripts numbered in the order they should be run. If scripts share a number, either could be run first. Descriptions of what each individual script does can be found in a comment at the top of the script.

**Results:** The results folder houses outputs for each step of the scripts. They are generally large files and are meant to be stored locally, though are reproducible from the provided data and code.

**Figures:** Contains all of the manuscript figures with a subfolder for supplemental figures. There is also a PowerPoint file used for additional formatting of Figure 1.

# NFL Fourth Down Inverse Optimization Analysis
Fundamental files and information for master's project titled "A Novel Inverse Optimization Approach to the Fourth Down Decision in Football".

## FILES:

### Scripts:

01_data.R

This script manipulates and creates the data frame used for the inverse optimization analysis. Most of the functions in the corresponding section in the code were written by Dr Sandholtz but the remainder of the script itself was written by Jacob. The reason the code to pull the data from the `nflfastr` package is commented out is because it is time consuming the run. For that reason, a .rda file is saved in the directory for easy access.

02_eda.R

This file contains code to recreate the vizualizations in the paper up until the analysis is done (through the Methods section of the paper).

03_io-analysis.R

This file contains the code to run the IO analysis one time. The actual analysis as done in the paper was done many times on the server. The script may take a while to run, depending on how efficiently the algorithm searches on that iteration. The `change_df` at the beginning of the script manually chooses which bins need to be inverted for the analysis. Unfortunately while I was working on the script, I couldn't figure out a more efficient or better programmatic way to write this so it was done manually.

04_results.R

This contains the code for the visualizations in the Results section of the paper. It utilizes the data generated and stored in the io-data folder, but applies to any data sets generated from the 03_io-analysis.R script.

### Data files:

clean_pbp_df.rds

This is the file that I saved so that running the code is much quicker. It is easy to see where this file comes from within the 01_data.R file, but between loading the data from the `nflfastr` package and running the clean_data function on the data, it was much more feasible just to save the data as a .rds file.

io-data/*.csv

These are data sets that are examples of what comes out of the 03_io-analysis.R script. You are welcome to generate more of your own using the script, but these are here for ease as well in recreating the visualizations and commentary in the Results section of the paper.

### Other files:

A_Novel_Inverse_Optimization_Approach_to_the_Fourth_Down_Decision_in_Football.pdf

A copy of the final draft of Jacob's master's project.

Presentation.pptx

Slide deck of the presentation used for Jacob's final master's project presentation.

## NOTES

Feel free to contact Jacob Miller (43jmills@gmail.com) with questions regarding the contents/motivations/details. I will answer the best that I can.

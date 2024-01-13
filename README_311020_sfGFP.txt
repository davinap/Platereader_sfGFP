This folder contains the plate reader data used to plot sfGFP intensity of strains expressing sfGFP under the control of YTK promoters. 

Every '.xlsx' file contiains the raw data, with the timepoint of each reading indicated in the file name. 
The layout of the 96-well plate can be found in the '...layout.csv' file. Within this file, the promoter for each strain is indicated below:
896 = pTDH3
897 = pCCW12
898 = pPGK1
899 = pHHF2
900 = pTEF1
901 = pTEF2
902 = pHHF1
903 = pHTB2
904 = pRPL18B
905 = pALD6
906 = pPAB1
907 = pRET2
908 = pRNR1
909 = pSAC6
910 = pRNR2
911 = pPOP6
912 = pRAD27
913 = pPSP2
914 = pREV1

In the R script, the 'plater' package is used to match sample names in the layout.csv file and OD600 readings in the .xlsx files. Then a series of functions are used to plot the blank-corrected values over a period of 56h. There are also a series of functions to plot blank-corrected, OD-normalised sfGFP intensities. To calculate the OD-normalised blank-corrected values, extract the files from the '...od_for_norm' folder so they can be read from the same working directory as the script.   

'tidy_new.csv' contains the non blank-corrected OD600 readings for all of the samples at different time points. It can be generated from the platecurver script.

In the '...plots' folder:
The plots produced by the R script can be found here. The word document contains the sfGFP intensities over time for all promoters in all media conditions. 
The blank-corrected sfGFP intensities for promoters in specific media conditions have also been saved in the corresponding .csv files in this folder. 

ypdx = YPD 10% glucose media
ypd = YPD media
sm = YNB media
yepg/yepgminus = YEPG media



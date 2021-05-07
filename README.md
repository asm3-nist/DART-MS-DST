Installation and operation notes for
NIST DART-MS Database Search Tool (DST)
2021/01/18
updated: 2021/05/04
==================================================

Dependencies:

Application requires 
R version 3.6 or higher (https://www.r-project.org/) 
and corresponding version of 
Rtools (https://cran.r-project.org/bin/windows/Rtools/) 
to install certain external package dependencies, and a browser to 
view the application (e.g. internet explorer, firefox, safari). 
Browser is only used as a viewer and app does not require an 
internet connection during run time.


Instructions:

Download the DST folder to your local machine.

Open R and *change the working directory* to the DST folder 
(e.g. C:/Downloads/DART-MS-DST-master/v0).

Enter the following command:

> source('RunShinyApp.R')

All necessary external R packages will be automatically installed on the 
first run of the application. For details about which external packages 
have been used, see the source file “asm_externalPackages.R” in 
the "shiny/source/" directory.


For help or more information, contact: DARTdata@nist.gov
    

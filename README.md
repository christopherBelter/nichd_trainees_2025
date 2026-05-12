# nichd_trainees_2025
This repo contains R code for matching NICHD trainees to their Scopus author profiles and then analyzing and visualizing the resulting data.  A more complete description of the rationale, methods, analysis, and results of this code will be presented in a forthcoming academic publication. 

## Overview
The repo contains three files. The first, `trainee_followup_2025.r`, contains the R code needed to gather, clean, analyze, and visualize the trainee data. This file is annotated throughout with general descriptions of what the code is doing in each code block and how it might be adapted or modified for other purposes. The second file, `qvr_processing.r`, contains helper functions called in the followup script to collapse multi-row data into a format of one trainee per row and to clean trainee organization names to enable better matching between the NIH's internal systems and the Scopus author profiles. Finally, the `nichd_palette.txt` contains a list of NICHD-branded colors used in the visualizations. 

## Setting the Scopus API key
The `authorSearch()` and `authorRetrieve()` functions from the `scopus_author_dev.r` file expects your Scopus API key to be saved as an environment variable called `SCOPUS_API_KEY`. You can get an API key from https://dev.elsevier.com/ and save it in your .Renviron file as 

```
SCOPUS_API_KEY = "foo"
```
and the function will find it. Otherwise you can set it manually in your R session by running 

```r
SCOPUS_API_KEY <- "foo"
```
but this isn't recommended in case you share your code, because you could then accidentally share your API key along with the code.

## Folder structure
The followup script assumes that the two helper files are saved in your current working directory. It also assumes the working directory contains two additional folders: "scopus data", where you want to save the resulting XML files returned by the Scopus API, and "csv data", where you'll want to save the summary tables used in the visualizations. You can, of course, change the file paths throughout the followup script to accommodate your local file structure if you want.

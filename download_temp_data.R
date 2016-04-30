#!/usr/bin/Rscript

url <- "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.4.0.0.annual_ns_avg.txt"
download.file(url, destfile="HadCRUT_data.txt", method="curl")

# Author: Sterling Hoeree
#
# Description:
#   Provides functions for downloading and plotting
#   some Household Power Consumption data.
#
# Tutorial:
#   To use these functions, a sample run would be:
#     > plot1("plot1.png")
#     > cleanup()
#   This would create the plot1.png file in the current
#   directory and then remove the intermediate files.
#
# Date: 2014/08/10
#

library(RCurl)
library(data.table)

zipfile = "household_power_consumption.zip"
txtfile = "household_power_consumption.txt"

# Fetch the data from the download URL, if required,
# and extract it to the current working directory.
getData <- function() {
  # download the binary data into a zip file if we
  # don't already have it
  
  if (!file.exists(zipfile)) {
    f = CFILE(zipfile, mode="wb")
    srcUrl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    curlPerform(url=srcUrl, 
                writedata=f@ref, 
                ssl.verifypeer=FALSE,
                noprogress=FALSE)
    close(f)
  }
  
  if (!file.exists(txtfile)) {
    # decompress the file (unzip it)
    unzip(zipfile)
  }
  
  data = fread(txtfile,
               sep=";",
               colClasses=c(rep("character",9)),
               na.strings=c("NA","\\N","?"),
               header=TRUE,
               showProgress=TRUE
               )
  setkey(data, Date)
  
  # only get the data we want, from 2007/02/01,02
  data[Date == "1/2/2007" | Date == "2/2/2007"]
}

# Clean up the downloaded/unzipped files.
# Params:
#   all (default=False) If TRUE, also delete the downloaded
#       zip archive file.
cleanup <- function(all=FALSE) {
  if (file.exists(txtfile)) {
    file.remove(txtfile)
  }
  if (all && file.exists(zipfile)) {
    file.remove(zipfile)
  }
}


# This method actually plots the data, either to the screen 
# or to a PNG file.
# Params:
#   png_file (default=NA) If given, will plot a png file with
#            the given name. If not specified, will plot
#            to the screen instead.
plot1 <- function(png_filename=NA) {
  # get the data
  data = getData()
  histdata = as.numeric(data$Global_active_power)
  
  if (!is.na(png_filename)) {
    png(filename=png_filename)
  }
  
  # make the plot
  hist(histdata, col="red",
       main="Global Active Power",
       xlab="Global Active Power (kilowatts)"
       )
  
  if (!is.na(png_filename)) {
    dev.off()
  }
  
  # clean up the extracted data
  cleanup()
}

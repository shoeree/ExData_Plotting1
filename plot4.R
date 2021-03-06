# Author: Sterling Hoeree
#
# Description:
#   Provides functions for downloading and plotting
#   some Household Power Consumption data.
#
# Tutorial:
#   To use these functions, a sample run would be:
#     > plot2("plot1.png")
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
plot4 <- function(png_filename=NA) {
  # get the data
  data = getData()
  xs = 1:length(data$Time)
  
  if (!is.na(png_filename)) {
    png(filename=png_filename)
  }
  
  # set up the plot environment so I can have multiple
  # plots on the same device
  par(mfcol=c(2,2))
  
  # make the plots
  # NOTE: normally, if this wasn't a self-contained
  # module for a course, I would simply source() and
  # include the other plotN() functions. Instead, I'm
  # repasting their main logic here for verbosity.
  
  # plot2
  gapdata = as.numeric(data$Global_active_power)
  plot.default(xs,
               gapdata,
               type="l", # line graph,
               xlab="",
               ylab="Global Active Power"
  )
  
  # plot3
  subdata_1 = as.numeric(data$Sub_metering_1)
  subdata_2 = as.numeric(data$Sub_metering_2)
  subdata_3 = as.numeric(data$Sub_metering_3)
  plot.default(xs,
               subdata_1,
               type="l", # line graph,
               xlab="",
               ylab="Energy sub metering"
  )
  lines(xs,
        subdata_2,
        col="red")
  lines(xs,
        subdata_3,
        col="blue")
  legend("topright",
         legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         col=c("black","red","blue"),
         lwd=c(1,1,1))
  
  # new plot: Voltage
  voltdata = data$Voltage
  plot.default(xs,
               voltdata,
               type="l", # line graph,
               xlab="datetime",
               ylab="Voltage"
  )
  
  # new plot: global reactive power
  grpdata = data$Global_reactive_power
  plot.default(xs,
               grpdata,
               type="l", # line graph,
               xlab="datetime"
  )
  
  # close the device
  if (!is.na(png_filename)) {
    dev.off()
  }
  
  # clean up the extracted data
  cleanup()
  
  # clean up the extracted data
  cleanup()
}

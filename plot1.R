

downloadAndNormalizeData <- function() {
  
  
  downloadFile <- function(fileURL, localFileName) {
    if(!file.exists(localFileName)) {
      download.file(fileURL, destfile=localFileName) 
    }
    localFileName
  }
  
  csvFile <- "subset_data.csv"
  if(file.exists(csvFile)) {
    tbl <- read.csv(csvFile)
    tbl$DateTime <- strptime(tbl$DateTime, "%Y-%m-%d %H:%M:%S")
  }
  else {
    zipFile <- downloadFile("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "household_power_consumption.zip")
    txtFile <- unz(zipFile, "household_power_consumption.txt")
    tbl <- read.table(txtFile, header=T, sep=';', na.strings="?", colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
    tbl <- tbl[(tbl$Date == "1/2/2007") | (tbl$Date == "2/2/2007"),]
    tbl$DateTime <- strptime(paste(tbl$Date, tbl$Time), "%d/%m/%Y %H:%M:%S")
    write.csv(tbl, csvFile)
  }
  tbl
}

table <- downloadAndNormalizeData()
png(filename = "plot1.png", width = 480, height = 480, units = "px")
hist(table$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)", col="red")
dev.off()


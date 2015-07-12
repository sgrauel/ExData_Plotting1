# TASK: process the Individual household electric power consumption Data Set
# INPUT : 'household_power_consumption.txt in pwd'
# OUTPUT : a tidy dataset in data table form with proper attribute names and types for use in plotting
# NOTE: output of this function serves as input to makePlot1

processHPC <- function() {

  library(data.table)
  
  # read in the data
  powerConsump <- fread("./household_power_consumption.txt", sep = ";", header = TRUE, na.strings = c("?"))
  
  
  # save the collumn names
  
  attrNames <- names(powerConsump)
  
  # give the collumns appropriate types
  # time zone of Clamart, FR : CET i.e. Central European Time Zone NOTE: date component is irrelevan
  powerConsump <- powerConsump[,list(as.Date(Date, "%d/%m/%Y"),
                     as.POSIXct(Time,tz = "CET", format = "%H:%M:%S"),
                     as.double(Global_active_power),
                     as.double(Global_reactive_power),
                     as.double(Voltage),
                     as.double(Global_intensity),
                     as.integer(Sub_metering_1),
                     as.integer(Sub_metering_2),
                     as.integer(Sub_metering_3))]
  
  # give back the names
  setnames(powerConsump,seq(1,length(powerConsump)),attrNames)
  
  
  library(dplyr)
  
  # give collumns appropriate names
  
  powerConsump <- powerConsump %>% rename(GlobalActivePower = Global_active_power, 
                          GlobalReactivePower = Global_reactive_power,
                          GlobalIntensity = Global_intensity,
                          Submetering1 = Sub_metering_1, Submetering2 = Sub_metering_2, 
                          Submetering3 = Sub_metering_3)
  
  # filter for observations taken from dates 2007-02-01 or 2007-02-02
  
  powerConsump <- powerConsump %>% filter( (Date == (as.Date("2007-02-01"))) |
                                            (Date == (as.Date("2007-02-02"))))
  powerConsump
}


# plot 1

# TASK : emulate the first barplot Frequency against Global Active Power (kilowatts)
# INPUT : a tidy dataset in data table form output by 'processHPC'
# OUTPUT: a png file with the first plot

makePlot1 <- function(dt) {
  
  png("./plot1.png", width=480, height=480)
  
  hist(dt$GlobalActivePower, col = "red", main = "Global Active Power", 
       xlab="Global Active Power (kilowatts)", xlim = c(0,6), ylim = c(0,1200))
  
  dev.off()
  
}









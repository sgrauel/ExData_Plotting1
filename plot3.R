processHPC3 <- function() {
  
  library(data.table)
  
  # read in the data
  powerConsump <- fread("./household_power_consumption.txt", sep = ";", header = TRUE, na.strings = c("?"))
  
  
  # save the collumn names
  
  attrNames <- names(powerConsump)
  
  # give the collumns appropriate types
  # time zone of Clamart, FR : CET i.e. Central European Time Zone NOTE: date component is irrelevan
  powerConsump <- powerConsump[,list(Date,
                                     Time,
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
  
  powerConsump <- powerConsump %>% filter((Date == "1/2/2007") |
                                            (Date == "2/2/2007"))
  
  # combine the date and time collumns into one datetime of type POSIXct, a type with date and time components
  
  # zip vectors together
  powerConsump[,DateTime:=paste(Date,Time,sep = " ")]
  
  # mutate datetime to POSIXct
  powerConsump[,DateTime:=as.POSIXct(DateTime, tz = "CET", format = "%d/%m/%Y %H:%M:%S")]
  
  # sort observations by DateTime
  powerConsump <- powerConsump %>% arrange(DateTime)
  
  # select out Date and Time collumns
  powerConsump <- powerConsump %>% select(3:length(names(powerConsump)))
  
  powerConsump
}


makePlot3 <- function(dt) {
  
  png("./plot3.png", width=480, height=480)
  
  plot(dt$DateTime, dt$Submetering1, type="l", ylab="Energy Submetering", xlab="")
  lines(dt$DateTime, dt$Submetering2, type="l", col="red")
  lines(dt$DateTime, dt$Submetering3, type="l", col="blue")
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=2.5, col=c("black", "red", "blue"))
  
  dev.off()
  
}


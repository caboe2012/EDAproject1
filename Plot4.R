###########################################
## ExplDataAnalysis - Project 1 - Plot 4 ##
###########################################
load_and_process_data <- function(){
    # Load the data into an R object, energyRaw, 
    # and convert variables as necessary
    if (!exists('energyRaw')) {
        cat('loading and processing the raw data...this will take a minute\n')
        raw <- read.csv('household_power_consumption.txt', sep = ';', header = T, stringsAsFactors = F)
        raw$Date <- as.Date(raw$Date, format = "%d/%m/%Y")
        raw$Global_active_power <- as.numeric(raw$Global_active_power)
        dates <- raw$Date
        dates <- as.character(dates)
        times <- raw$Time
        times <- as.character(times)
        dates.times <- paste(dates,times)
        dates.times <- strptime(dates.times, "%Y-%m-%d %H:%M:%S")
        raw$DatesTimes <- dates.times
        raw$Sub_metering_1 <- as.numeric(raw$Sub_metering_1)
        raw$Sub_metering_2 <- as.numeric(raw$Sub_metering_2)
        raw$Global_reactive_power <- as.numeric(raw$Global_reactive_power)
        raw$Voltage <- as.numeric(raw$Voltage)
        energyRaw <<- raw
        cat('load complete. \n')
    }
    else {
        cat('raw data already loaded into R...\n')
    }
}

grab_relevant_data <- function(){
    # Find only those rows that contain data we need
    # and load into a new R object, energyData
    if (!exists('energyData')) {
        cat ('Grabbing relevant data. This may take a minute...\n')
        feb1 <- grep('2007-02-01',energyRaw$Date)
        feb2 <- grep('2007-02-02',energyRaw$Date)
        relevant_rows <- c(feb1,feb2)
        dat <- energyRaw[relevant_rows,]
        energyData <<- dat
        row.names(energyData) <- c(1:2880)
    }
    else {
        cat('relevant data already available \n')
    }
}

GAP_plot <- function(){
    # Global Active Power Graph
    plot(energyData$DatesTimes, energyData$Global_active_power, type = 'l', 
         xlab = '', ylab = 'Global Active Power')
}

voltage_plot <- function() {
    # Voltage Graph
    plot(energyData$DatesTimes, energyData$Voltage, type = 'l', xlab = 'datetime', ylab = 'Voltage')
}

GRP_plot <- function(){
    # Global reactive power graph
    plot(energyData$DatesTimes, energyData$Global_reactive_power, type = 'l', 
         xlab = 'datetime', ylab = 'Global_reactive_power')
}
submeter_plot <- function(){
    # Create the layered submeter graph, designating parameters as required
    cat('graphing \n')
    plot(energyData$DatesTimes, energyData$Sub_metering_1, type = 'l', col = 'black',  xlab = '', ylab = 'Energy sub metering')
    points(energyData$DatesTimes, energyData$Sub_metering_2, type = 'l', col = 'red')
    points(energyData$DatesTimes, energyData$Sub_metering_3, type = 'l', col = 'blue')
    legend('topright', bty = 'n', xjust = '1', yjust = '0.5', lty = 'solid' , col = c('black', 'red', 'blue'), legend = c('Sub_metering_1','Sub_metering_2', 'Sub_metering_3'), merge = T)
}

# Grouped subplots
group_plot <- function() {
    png("Plot4_Final.png", width = 480, height = 480, units = "px")
    par(mfcol = c(2, 2), mar = c(4, 4, 2, 1), oma = c(1, 1, 3, 0))
    with(energyData, {
        GAP_plot()
        submeter_plot()
        voltage_plot()
        GRP_plot()
        dev.off()
    })
    
}

preview_png <- function(){
    # View the PNG output file in R
    library(png)
    img <- readPNG('Plot4_Final.png', native = T)
    grid::grid.raster(img)
}

create_plots4 <- function() {
    # program to run the functions
    load_and_process_data()
    grab_relevant_data()
    group_plot()
    preview_png()
}

# Call the create function
create_plots4()


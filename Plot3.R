###########################################
## ExplDataAnalysis - Project 1 - Plot 3 ##
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

submeter_plot_PNG <- function(){
    # Create the layered graph, designating parameters as required
    # Save to a PNG called 'Plot3.png'
    library(png)
    png("Plot3.png", width = 480, height = 480, units = "px")
    par(mfrow = c(1,1), oma = c(1,2,0,0))
    cat('graphing and saving to file \n')
    # Save image to PNG file using the png command to avoid altering legend text
    plot(energyData$DatesTimes, energyData$Sub_metering_1, type = 'l', col = 'black',  xlab = '', ylab = 'Energy sub metering')
    lines(energyData$DatesTimes, energyData$Sub_metering_2, type = 'l', col = 'red')
    lines(energyData$DatesTimes, energyData$Sub_metering_3, type = 'l', col = 'blue')
    legend('topright', lty = 'solid' , xjust = '1', yjust = '0.5', col = c('black', 'red', 'blue'), legend = c('Sub_metering_1','Sub_metering_2', 'Sub_metering_3'))
    dev.off()
}

preview_png <- function(){
    # View the PNG output file in R
    library(png)
    img <- readPNG('Plot3.png', native = T)
    grid::grid.raster(img)
}

create_plots3 <- function() {
    # program to run the functions
    load_and_process_data()
    grab_relevant_data()
    submeter_plot_PNG()
    preview_png()
}

# Call the create function
create_plots3()


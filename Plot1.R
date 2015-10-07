###########################################
## ExplDataAnalysis - Project 1 - Plot 1 ##
###########################################
load_and_process_data <- function(){
    # Load the data into an R object, energyRaw, 
    # and convert variables as necessary
    if (!exists('energyRaw')) {
        cat('loading and processing the data...this will take a minute\n')
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
        cat('data already loaded into R...')
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
        cat('relevant data already loaded')
    }
}

GAP_hist_PNG <- function() {
    # Create the histogram, designating parameters as required
    # Save to a PNG file called 'Plot1.png'
    par(oma = c(1,2,0,0))
    cat('plotting histogram...\n')
    hist(energyData$Global_active_power, col = 'red', main = 'Global Active Power', xlab = 'Global Active Power (kilowatts)')
    cat('saving to file.\n')
    dev.copy(png, file = 'Plot1.png')
    dev.off()
}

create_plots <- function() {
    # program to run the functions
    load_and_process_data()
    grab_relevant_data()
    GAP_hist_PNG()
}

preview_png <- function(){
    # View the PNG output file in R
    library(png)
    img <- readPNG('Plot1.png', native = T)
    grid::grid.raster(img)
}

# Call the create function
#create_plots()


library(ggplot2)
## source("ExData_Init.R")
##
## Question 6
##
## Compare emissions from motor vehicle sources in Baltimore City with 
## emissions from motor vehicle sources in Los Angeles County, California 
## (fips == "06037"). Which city has seen greater changes over time in motor 
## vehicle emissions?
##
plot6 <- function() {
    # Subset NEI data by city's fip and add city name.
    NEI <- getNEI(c("24510","06037"))
    NEI$city[NEI$fips == "24510"] <- "Baltimore City"
    NEI$city[NEI$fips == "06037"] <- "Los Angeles County"
    NEI$city <- as.factor(NEI$city)
    
    # Load the SCC data frames.
    SCC <- getSCC()
    vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)

    # Gather the subset of the NEI data which corresponds to vehicles
    vehiclesNEI <- NEI[NEI$SCC %in% SCC[vehicles,]$SCC,]
    vehiclesNEI$year <- as.factor(vehiclesNEI$year)
    
    
    png("plot6.png",width=480,height=480,units="px",bg="transparent")
    
    ggp <- ggplot(vehiclesNEI, aes(x=year, y=Emissions, fill=city)) +
        geom_bar(aes(fill=year), stat="identity") +
        facet_grid(scales="free", space="free", .~city) +
        guides(fill=FALSE) + theme_bw() +
        labs(title="PM2.5 Emissions From Baltimore & Los Angeles in 1999-2008", 
             subtitle="Motor Vehicle Sources") +
        labs(x="Year", y="Total PM2.5 Emission (Tons)")
    
    print(ggp)
    
    dev.off()
    
}

ExData_Init <- function() {
    
    ## download/unzip the dataset for the project
    ## project files are stored on local "data" folder
    datadir <- "data"
    destfile<- paste(datadir, "NEI_data.zip", sep="/")
    #    datafile<- paste(datadir, "household_power_consumption.txt", sep="/")
    
    if (!file.exists(datadir)) {
        dir.create(datadir)
    }
    
    ## download the file if it doesn't exist
    if (!file.exists(destfile)) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(fileUrl, destfile=destfile, mode="wb")
        ## unzip the project files
        unzip(destfile, exdir=datadir)
    }
    ## read data
    #    pwrdata <- read.table(datafile, stringsAsFactors = FALSE, header = TRUE, sep =";"  )
    #    return(pwrdata)
}

getNEI <- function(fips = NULL) {
    datadir <- "data"
    datafile<- paste(datadir, "summarySCC_PM25.rds", sep="/")
    ExData_Init()
    NEI <- readRDS(datafile)
    if (!is.null(fips)) {
        NEI <- NEI[(NEI$fips %in% fips),]
    }
    return(NEI)
}

getSCC <- function() {
    datadir <- "data"
    datafile<- paste(datadir, "Source_Classification_Code.rds", sep="/")
    ExData_Init()
    SCC <- readRDS(datafile)
    return(SCC)
}


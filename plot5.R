## source("ExData_Init.R")
## Question 5
##
## How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
##

plot5 <- function() {
    # Subset NEI data to Baltimore's fip
    NEI <- getNEI(c("24510"))
    
    # Load the SCC data frames.
    SCC <- getSCC()
    vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
    
    # Gather the subset of the NEI data which corresponds to vehicles
    vehiclesNEI <- NEI[NEI$SCC %in% SCC[vehicles,]$SCC,]
    
    # Subset the vehicles NEI data to Baltimore's fip
    aggYears <- aggregate(Emissions ~ year, vehiclesNEI, sum)
    aggYears$year <- as.factor(aggYears$year)
    
    
    png("plot5.png",width=480,height=480,units="px",bg="transparent")
    
    barplot(
        (aggYears$Emissions)/100, names.arg=aggYears$year,
#        main="PM2.5 Emissions From Baltimore City in 1999-2008",
#        sub="Motor Vehicle Source",
        xlab="Year",
        ylab="PM2.5 Emissions (Hundred Tons)",
        col=aggYears$year
    )
    
    mtext(side=3,"PM2.5 Emissions From Baltimore City in 1999-2008",line=2, cex=1.5)
    mtext(side=3,"Motor Vehicle Sources",line=1, cex=1)
    
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


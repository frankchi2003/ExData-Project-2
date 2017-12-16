## source("ExData_Init.R")
## Question 1
## Have total emissions from PM2.5 decreased in the United States 
## from 1999 to 2008 ? 
##
## Using the base plotting system, make a plot showing the 
## total PM2.5 emission from all sources for each of the years 
## 1999, 2002, 2005, and 2008.
##
plot1 <- function() {
    # Load the NEI data frames.
    NEI <- getNEI()

    # Aggregate by sum the total emissions by year
    aggYears <- aggregate(Emissions ~ year, NEI, sum)
    aggYears$year <- as.factor(aggYears$year)
    png("plot1.png",width=480,height=480,units="px",bg="transparent")
    
    barplot(
        (aggYears$Emissions)/10^6, names.arg=aggYears$year,
#        main="PM2.5 Emissions Across US in 1999-2008",
#        sub="All Sources",
        xlab="Year",
        ylab="PM2.5 Emissions (million Tons)",
        col=aggYears$year
    )
    mtext(side=3,"PM2.5 Emissions Across US in 1999-2008",line=2, cex=1.5)
    mtext(side=3,"All Sources",line=1, cex=1)
    
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


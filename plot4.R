## source("ExData_Init.R")
## Question 4
##
## Across the United States, how have emissions from coal combustion-related sources changed 
## from 1999-2008?
##
plot4 <- function() {
    # Load the NEI & SCC data frames.
    NEI <- getNEI()
    SCC <- getSCC()
    
    # Subset coal combustion related NEI data
    coalCombustion <- (grepl("combustion", SCC$SCC.Level.One, ignore.case=TRUE) & 
                       grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE))
    combustionNEI <- NEI[NEI$SCC %in% SCC[coalCombustion,]$SCC,]
    aggYears <- aggregate(Emissions ~ year, combustionNEI, sum)
    aggYears$year <- as.factor(aggYears$year)

    png("plot4.png",width=480,height=480,units="px",bg="transparent")
    
    barplot(
        (aggYears$Emissions)/1000, names.arg=aggYears$year,
#        main="PM2.5 Emissions Across US in 1999-2008",
#        sub="Coal Combustion Source",
        xlab="Year",
        ylab="PM2.5 Emissions (Thousand Tons)",
        col=aggYears$year
    )
    mtext(side=3,"PM2.5 Emissions Across US in 1999-2008",line=2, cex=1.5)
    mtext(side=3,"Coal Combustion Sources",line=1, cex=1)
    
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


library(ggplot2)
## source("ExData_Init.R")
## Question 3
##
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City ? 
## Which have seen increases in emissions from 1999-2008 ? 
##
## Use the ggplot2 plotting system to make a plot answer this question.
##
plot3 <- function() {
    # Subset NEI data by Baltimore's fip.
    NEI <- getNEI(c("24510"))
    
    NEI$year <- as.factor(NEI$year)
    NEI$type <- as.factor(NEI$type)
    
    png("plot3.png",width=480,height=480,units="px",bg="transparent")

    ggp <- ggplot(NEI, aes(year, Emissions, fill=type)) +
        geom_bar(stat="identity") +
        theme_bw() + 
        guides(fill=FALSE)+
        facet_grid(.~type, scales = "free", space="free") + 
        labs(title="PM2.5 Emissions From Baltimore City in 1999-2008", subtitle="by Source Type") +
        labs(x="year", y=expression("PM2.5 Emission (Tons)")) 
    
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


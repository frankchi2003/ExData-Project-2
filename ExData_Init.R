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



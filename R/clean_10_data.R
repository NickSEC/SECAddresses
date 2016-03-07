        #' Read and Clean 10K and 10Q address data
        #' @description Read 10K or 10Q address file into a data frame.
        #' The data is taken from the parsed EDGAR list that the SEC provides.
        #'
        #' @param x a text file in SEC's format
        #' @return A data frame of the original columns, plus 3 extra columns:
        #'      Lat            Latitude of the business zip code
        #'      Long           Longitude of the business zip code
        #'      companies      Count of companies in the same zip code
        #' @details 3 extra columns are added to the original data frame.
        #' The latitude and longitude information is obtained from a zipcode
        #' data file that is loaded internally.  Any line that doesn't have a
        #' valid business zip code in the 'zipba' field will get NA for
        #' its latitude and longitude.
        #' The last column, 'companies', is a count of all of the rows in the
        #' data frame that contain the same business zip code.  This gives an
        #' idea of how popular an area is, and can be used as an argument to
        #' a plot command to indicate the size of a plot point.
        #' Companies are removed if they list a foreign address.
        #' Blank addresses are filled in.  The main advantage to this, besides address
        #' data continuity, is in having the business address fields populated.  Some
        #' analytical routines pull their data from the business address, so it's a good
        #' idea to have it fully populated.
        #'The business address fields checked are:  'bas1','bas2','cityba',.stprba','zipba'
        #'The mainling address fields checked are:  'mas1','mas2','cityma',.stprma','zipma'        #' Blank addres fields are filled in based on
        #' @import zipcode
        #' @examples
        #' data_for_2016 <- clean_10_data("sub16.txt")
        #' @author Nick Lukianoff
        #' @export
clean_10_data <- function(x) {
        #Read in the data
        x <- read.delim(x, stringsAsFactors = FALSE)

        #Clean up the data
        x$sic <- formatC(x$sic, width = 4, format = "d", flag = "0") #format sic codes
        x$zipba <- substr(x$zipba,1,5) #create only 5-digit zip codes
        x$zipma <- substr(x$zipma,1,5) #create only 5-digit zip codes
        suppressWarnings(x$zipba <- as.numeric(x$zipba))
        x <- subset(x, zipba >= 100 & zipba <= 99999) #remove bad zips

        #Remove any non-US registrations
        x <- subset(x, x$countryinc == "US" |
                            x$countryba == "US" | x$countryma == "US")
        x <- subset(x, x$countryba == "" | x$countryba == "US")
        x <- subset(x, x$countryma == "" | x$countryma == "US")
        x$countryinc <- NULL
        x$countryba <- ifelse(x$countryba == "", 'US', as.character(x$countryba))
        x$countryma <- ifelse(x$countryma == "", 'US', as.character(x$countryma))

        #Fix state names
        x$stprinc <- ifelse(x$stprinc == "", as.character(x$stprba), as.character(x$stprinc)) #replace with business state
        x$stprinc <- ifelse(x$stprinc == "", as.character(x$stprma), as.character(x$stprinc)) #replace with personal state
        a1 <- which(x$stprinc %in% state.abb) #verify that codes are actual US states
        x <- x[a1,] #select only US states
        rm(a1)

        #Backfill addresses
        #use personal address is no busines address on file
        for (i in 1:nrow(x)) {
                if (x$bas1[i] == "") {
                        x$bas1[i] <- x$mas1[i]
                        x$bas2[i] <- x$mas2[i]
                        x$cityba[i] <- x$cityma[i]
                        x$stprba[i] <- x$stprma[i]
                        x$zipba[i] <- x$zipma[i]
                }
        }

        #duplicate business address if no personal address on file
        for (i in 1:nrow(x)) {
                if (x$mas1[i] == "") {
                        x$mas1[i] <- x$bas1[i]
                        x$mas2[i] <- x$bas2[i]
                        x$cityma[i] <- x$cityba[i]
                        x$stprma[i] <- x$stprba[i]
                        x$zipma[i] <- x$zipba[i]
                }
        }

        #Remove duplicate companies
        x <- subset(x,!duplicated(x$cik))

        #select useful zip fields, then pad out zip codes
        library(zipcode, quietly = TRUE, warn.conflicts = FALSE)
        data("zipcode")
        pzips <- subset(zipcode, select = c("zip","latitude","longitude"))
        colnames(pzips) <- c("Zip", "Lat", "Long")
        pzips$Zip <- formatC(pzips$Zip, width = 5, format = "d", flag = "0")

        #Add geodata & select only mappable rows
        x$zipba <- formatC(x$zipba, width = 5, format = "d", flag = "0") #pad zip codes
        x_temp <- merge(x, pzips, by.x = "zipba", by.y = "Zip", sort = FALSE)

        #select only address data
        x <- subset(x_temp, select = c("adsh", "cik", "sic", "name", "stprinc",
                                  "bas1", "bas2", "cityba", "stprba", "zipba",
                                  "mas1", "mas2", "cityma", "stprma", "zipma",
                                  "Lat", "Long"))

#        x <- x_temp[,c(2:(zip_col-1),1,(zip_col):ncol(x)+2)] # put the zip code field back into its place
        rm(x_temp)
        last_col <- ncol(x)+1
        for (i in 1:nrow(x))
                x[i,last_col] <- sum(x$zipba == x$zipba[i]) #get zipcode counts for mapping
        colnames(x)[last_col] <- "companies"
        rm(last_col)

        return(x)
}

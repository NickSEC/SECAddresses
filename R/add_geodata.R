        #' Add latitude and longitude information to a data frame
        #' @description Accepts a data frame of 10K or 10Q address data.
        #' Geographical information is added based on the business zip code
        #' field 'zipba'.
        #' The data is taken from the parsed EDGAR list that the SEC provides.
        #' @param x A data frame in SEC's format
        #' @return A data frame of the original columns, plus 3 extra columns:
        #'      Lat            Latitude of the business zip code
        #'      Long           Longitude of the business zip code
        #'      companies      Count of companies in the same zip code
        #' @details 3 extra columns are added to the original data frame.
        #' The latitude and longitude information is obtained from a zipcode
        #' data file that is loaded internally.  Any line that doesn't have a
        #' valid business zip code will get NA for its latitude and longitude.
        #'
        #' The last column, 'companies', is a count of all of the rows in the
        #' data frame that contain the same business zip code.  This gives an
        #' idea of how popular an area is, and can be used as an argument to
        #' a plot command to indicate the size of a plot point.
        #' @note You may want to run with routine with warning suppressed,
        #' since bad zip codes will generate NA values, and print an error
        #' message to the screen.
        #' @import zipcode
        #' @examples
        #' x <- suppressWarnings(add_geodata(df))
        #' @author Nick Lukianoff
        #' @export
add_geodata <- function(x) {
        #Add geodata & select only mappable rows
        #requires the zipcode file to be loaded
        #creates new columns "companies","Lat","Long"
        #removes any rows with bad zip codes

        #select useful zip fields, then pad out zip codes
        library(zipcode, quietly = TRUE, warn.conflicts = FALSE)
        data("zipcode")
        pzips <- subset(zipcode, select = c("zip","latitude","longitude"))
        colnames(pzips) <- c("Zip", "Lat", "Long")
        pzips$Zip <- formatC(pzips$Zip, width = 5, format = "d", flag = "0")

        #Add geodata & select only mappable rows
        x$zipba <- as.numeric(x$zipba)
        x$zipba <- formatC(x$zipba, width = 5, format = "d", flag = "0") #pad zip codes
        zip_col <- which(colnames(x) == "zipba") #get the zip code's column position
        x_temp <- merge(x, pzips, by.x = "zipba", by.y = "Zip", sort = FALSE, all.x = TRUE)

        #select only address data and put the zip code field back into its place
        x <- x_temp[,c(2:zip_col,1,(zip_col-1):ncol(x)+2)]
        rm(x_temp)
        last_col <- ncol(x)+1
        for (i in 1:nrow(x))
                x[i,last_col] <- sum(x$zipba == x$zipba[i]) #get zipcode counts for mapping
        colnames(x)[last_col] <- "companies"
        rm(zip_col)
        rm(last_col)

        return(x)
        }

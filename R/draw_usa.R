        #' Map registered companies in the continental USA
        #' @description Displays a map of a the continental US, showing registered
        #' companies clustered by the business zipcode, sized by the amount per zip
        #' code, and color-coded by the company's primary industry.
        #' @param x A data frame of addresses in the SEC's format
        #' @param savemap file name of the saved map in .png format.  If this is
        #' not provided, the function will draw the map on the screen,
        #' but not save it to a file.
        #' @return A registered company map of the continental US
        #' @details This function will use the data frame's geographic data
        #' (latitude and longitude based on the business zip code) to map the
        #' locations of registered companies in that state.  The map data is shown
        #' by size of data point (based on the amount of companies in a given area),
        #' and color-coded by the industry that it is in.
        #' @note  This function makes use of the ggmap package.  It also uses the
        #' 'Lat' and 'Long' fields to perform its mapping.
        #' The base SEC address file doesn't contain any geographical data.  This means
        #' that before running this routine, create "Lat" and "Long" columns, and create
        #' a 'companies' column that contains a count of companies per zip code.  The
        #' routine 'add_geodata' can perform this function for you.
        #' @import manipulate
        #' @importFrom ggmap qmplot
        #' @examples
        #' draw_usa(df)
        #' draw_usa(df,"Registered Companies")
        #' @author Nick Lukianoff
        #' @export
draw_usa <- function(x, savemap = FALSE) {
        #Draw a map of the continental USA showing companies for a given year
        industries <- c("Land Related", "Mining & Construction", "Small Manufacturing",
                        "Large Manufacturing", "Utilities", "Trade", "Financial",
                        "Business Services", " Social Services", "Public Admin")

        #Create a data group of continental locations
        x <- subset(x, stprba != "AK" & stprba != "HI") #remove Alaska and Hawaii
        #Draw a map of the continental sites
        industry <- industries[as.numeric(substr(x$sic,1,1)) + 1]
        data_year <- paste("Company Locations in 20",substr(x[1,]$adsh,12,13),sep = "")
        suppressMessages(print(
                qmplot(
                        Long,Lat, data = x,
                        color = industry, size = companies, zoom = 4,
                        messaging = FALSE
                ) + ggtitle(data_year)
        ))
        if (!savemap == FALSE) {
                dev.copy(png,paste(savemap,".png",sep=""), width = 1600, height = 1105, res = 150) #save the plot
                dev.off()
        }
}

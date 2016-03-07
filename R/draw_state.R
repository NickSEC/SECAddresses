        #' Map a state's registered companies
        #' @description Displays a map of a state, showing registered companies
        #' clustered by the business zipcode, sized by the amount per zip code,
        #' and color-coded by the company's primary industry.
        #' @param x A data frame of addresses in the SEC's format
        #' @param state_input Uppercase 2-character state code.  If this is not
        #' provided, the function will prompt for a state.
        #' @param savemap file name of the saved map in .png format.  If this is
        #' not provided, the function will draw the map on the screen,
        #' but not save it to a file.
        #' @return A registered company map of the selected state
        #' @details This function will prompt for a 2-letter uppercase state name (if
        #' there isn't one passed in), then use the data frame's geographic data
        #' (latitude and longitude based on the business zip code) to map the
        #' locations of registered companies in that state.  The map data is shown
        #' by size of data point (based on the amount of companies in a given area),
        #' and color-coded by the industry that it is in.
        #' If no state is entered, Virginia (VA) is assumed.
        #' If no records are found for a given state, an error message is printed.
        #' @note  This function makes use of the ggmap package.
        #' The base SEC address file doesn't contain any geographical data.  This means
        #' that before running this routine, create "Lat" and "Long" columns, and create
        #' a 'companies' column that contains a count of companies per zip code.  The
        #' routine 'add_geodata' can perform this function for you.
        #' @importFrom ggmap qmplot
        #' @examples
        #' draw_state(df)
        #' draw_state(df,"DE")
        #' draw_state(df,"DE","Delaware Companies")
        #' @author Nick Lukianoff
        #' @export
draw_state <- function(x, state_input = '', savemap = FALSE) {
        #Pick a US state and map it
        industries <- c("Land Related", "Mining & Construction", "Small Manufacturing",
                        "Large Manufacturing", "Utilities", "Trade", "Financial",
                        "Business Services", " Social Services", "Public Admin")

        #Select a state for testing
        while (!(state_input %in% state.abb)) {
                state_input <- readline("Enter a state for mapping (default is VA): ")
                if (nchar(state_input) == 0) state_input = "VA"
        }
        x1 <- subset(x, x$stprba == state_input) #select only the one state
        if (nrow(x1) == 0) {
                print(paste("No records selected from", state_input))
                return(NA)
        }

        #Draw a map of an individual state sites
        industry <- industries[as.numeric(substr(x1$sic,1,1)) + 1]
        data_year <- paste("20",substr(x1[1,]$adsh,12,13),sep = "")
        suppressMessages(print(
                qmplot(
                        Long,Lat,
                        data = x1, color = industry, size = companies, legend = "right",
                        messaging = FALSE
                ) + ggtitle(paste(state_input,data_year))
        ))
        if (!savemap == FALSE) {
                dev.copy(png,paste(savemap,".png",sep=""),
                         width = 1200, height = 900, res = 150)
                dev.off()
        }
}

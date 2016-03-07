        #' Interactive map of registered companies in the continental USA
        #' @description This function takes a list of address data froames in
        #' the SEC's format, and maps incorporated companies per year.
        #' There is an option to select any year from the list of passed data frames.
        #' @param x A list of data frames from 2010 onwards
        #' @return An interactive bar graph, selectable by state and year
        #' @details This function will use the data frame's geographic data
        #' (latitude and longitude based on the business zip code) to map the
        #' locations of registered companies in that state.  The map data is shown
        #' by size of data point (based on the amount of companies in a given area),
        #' and color-coded by the industry that it is in.
        #' @note The routine assumes that the first item in the list is the data
        #' for 2010, and each subsequent list item is an additional sequential year.
        #' This routine removes Alaska and Hawaii companies in order to only display
        #' companies that have a business address in the continental United States.
        #' This routine requires the 'ggmap' library.
        #' This routine requires the 'manipulate' library.
        #' @import manipulate
        #' @importFrom ggmap qmplot
        #' @examples
        #' plot_usa(list(df1,df2,df3))
        #' @author Nick Lukianoff
        #' @export
plot_usa <- function(x) {
        #Draw a selecatble map of the continental sites
        #input a list of data frames from 2010 onwards

        year_names <- (2009 + 1:length(x))
        year_names <- as.list(year_names)

        for (i in 1:length(x)) {
                x[[i]] <- subset(x[[i]], stprba != "AK" & stprba != "HI")
        }
        manipulate(
                qmplot( Long, Lat, data = x[[as.numeric(xyear)-2009]], size = companies,
                        legend = "bottom", messaging = FALSE)
                + scale_fill_discrete(labels = "Density")
                + ggtitle(paste(
                        "Registered Companies in 20",
                        substr(x[[as.numeric(xyear)-2009]][1,]$adsh,12,13),sep = "")),
                xyear = picker(year_names, label = "Year"))
}

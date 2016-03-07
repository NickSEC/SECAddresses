        #' Interactive bar graph of incorporated companies per state per year
        #' @description This function takes a list of address dataframes in the
        #' SEC's format, and graphs each state's incorporated companies per year.
        #' There are options to select any year from the list, or to obtain a
        #' graph of the sum of all of the years.  A dropdown list allows the
        #' subtraction of the states with the most incorporated companies.  This
        #' allows the states with smaller amounts to be more clearly seen.
        #' @param x A list of data frames from 2010 onwards
        #' @return An interactive bar graph, selectable by state and year
        #' @details This routine is a graphing routine that generates a bar
        #' graph, where x is the number of rows per state in each data frame
        #' that is passed in, y is the year, starting with 2010.
        #' A dropdown list is presented listing all of the years represented in
        #' the data frames that are passed in, as well as an option to show all
        #' of the years together.
        #' A second dropdown list is presented with a listing of the largest
        #' state values.  These states can be removed in order to better show
        #' the values of the remaining states.
        #' The states that can be removed are:  DE, NV, MD, CA, NY, FL
        #' @note The routine assumes that the first item in the list is the data
        #' for 2010, and each subsequent list item is an additional sequential year.
        #' This routine requires the 'manipulate' library.
        #' @import manipulate
        #' @examples
        #' plot_states(list(df1,df2,df3))
        #' @author Nick Lukianoff
        #' @export
plot_states <- function(x) {
        #Map state statistics on a map by selecting a year
        #input a list of data frames from 2010 onwards

        #Combine state data into its own data frame for analysis
        state_list <- as.data.frame(state.abb)
        colnames(state_list) <- "Var1"
        for (i in 1:length(x)) {
                stl_tmp <- as.data.frame(table(x[[i]]$stprinc))
                suppressWarnings(state_list <- merge(state_list, stl_tmp, by = "Var1", all.x = TRUE))
        }
        rm(stl_tmp)
        state_list[is.na(state_list)] <- 0 #remove NAs to make math easier
        colnames(state_list) <- (2008 + 1:ncol(state_list))
        colnames(state_list)[1] <- "State"
        state_list$All <- apply(state_list[2:ncol(state_list)],1,sum)
        #set up a variable to hold the data range
        year_names <- colnames(state_list)[2:(ncol(state_list))]
        year_names <- as.list(year_names)

        #Graph state data
        manipulate(
                barplot(
                        state_list[group, x2],
                        xlab = "State", ylab = "Companies",
                        names.arg = state_list$State[group], cex.names = .5, las = 2,
                        main = paste(x2,"10K Filings by State")
                ),
                x2 = picker(year_names, initial = "All", label = "Year"),
                group = picker(
                        "None" = 1:50,
                        "DE" = c(1:7,9:50),
                        "DE, NV" = c(1:7,9:32,34:50),
                        "DE, NV, MD" = c(1:7,9:19,21:32,34:50),
                        "DE, NV, MD, CA" = c(1:4,6:7,9:19,21:32,34:50),
                        "DE, NV, MD, CA, NY" = c(1:4,6:7,9:19,21:32,35:50),
                        "DE, NV, MD, CA, NY, FL" = c(1:4,6:7,10:19,21:32,35:50),
                        label = "States Excluded"
                )
        )
}

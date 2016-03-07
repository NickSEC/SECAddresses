        #' XBRL Reporting Compliance Graph by State
        #' @description Accepts a list of annual company listings, and graphs
        #' the number of companies in that state filing per year.  It does this
        #' by counting the number of rows in each list member that correspond to
        #' a given state, and plotting it by year.
        #' @param x A list of data frames from 2010 onwards
        #' @param state_input Uppercase 2-character state code.  If this is not
        #' provided, the function will prompt for a state.
        #' @return A line graph of annual company filings is generated
        #' @details This routine is a simple plotting routine that generates an
        #' x-y line graph, where x is the year, starting with 2010, and y is
        #' the number of rows per state in each data frame that is passed in.
        #' If no state is entered, Virginia (VA) is assumed.
        #' If no records are found for a given state, an error message is printed.
        #' @note The routine assumes that the first item in the list is the data
        #' for 2010, and each subsequent list item is an additional sequential year.
        #' @examples
        #' state_compliance(list(df1,df2,df3))
        #' state_compliance(list(df1,df2,df3), "MD")
        #' @author Nick Lukianoff
        #' @export
state_compliance <- function(x, state_input = '') {
        #Show a graph of a state's registered companies over the years
        #input a list of data frames from 2010 onwards

        years <- (2010:(2009+length(x)))
        years <- as.data.frame(years)
        compliance <- data.frame()

        while (!(state_input %in% state.abb)) {
                state_input <- readline("Enter a state for mapping (default is VA): ")
                if (nchar(state_input) == 0) state_input = "VA"
        }

        for (i in 1:length(x)) {
                compliance <- rbind(compliance,sum(x[[i]]$stprba == state_input))
        }

        if (nrow(compliance) == 0) {
                print(paste("No records selected from", state_input))
                return(NA)
        }

        compliance <- cbind(compliance,years)
        colnames(compliance)[1] <- "comp"
        plot(
                compliance$years, compliance$comp, type = "l",
                main = paste("XBRL Compliance in",state_input),
                ylab = "Companies", xlab = "Year"
        )
}

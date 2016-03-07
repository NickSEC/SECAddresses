        #' 2 tables of company statistics by state
        #' @description This function lists the number of registered companies
        #' in each of the United States.  The first table lists every state
        #' alphabetically, with a count of incorporated companies.  The second
        #' table lists every state, along with a percentage of companies
        #' incorporated in that state.  The second table is sorted by percentage.
        #' @param x A data frame in the SEC's format
        #' @return 2 tables of company statistics by state
        #' @details  The first thing that this routine does is to clear the screen
        #' by sending a Ctrl-L code to the main device.  It then displays a table,
        #' sorted by state, of companies per state.  A second table follows it,
        #' sorted by percentage of companies in each state.
        #' @note  Keep in mind the Ctrl-L code that precedes the function results.
        #' While this isn't an issue for screen output, it must be dealt with if
        #' you will be storing these 2 tables in a variable.
        #' These tables do not include US Territories, or foreign countries.
        #' @examples
        #' state_stat(df)
        #' x<- state_stat(df)
        #' @author Nick Lukianoff
        #' @export
state_stat <- function(x) {
        #Check state & company statistics
        #pass in the data frame with state information

        cat("\014") #send a Ctrl-L to clear the screen
        cat("States Where Registered:")
        print(table(x$stprinc)) #how many companies per state ?

        #what percentage in each state ?
        cat("\n")
        cat("Percent per State:")
        print(sort(round(prop.table(table(x$stprinc)) * 100,2),decreasing = TRUE))
        cat("\n")
}

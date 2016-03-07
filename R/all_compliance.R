        #' Show how many companies send in XML reports yearly
        #' @description Accepts a list of annual company listings, and graphs
        #' the number of companies filing per year.  It does this by counting
        #' the number of rows in each list member, and plotting it against the
        #' year.
        #' @param x A list of data frames from 2010 onwards
        #' @return A line graph of annual company filings is generated
        #' @details This routine is a simple plotting routine that generates an
        #' x-y line graph, where x is the year, starting with 2010, and y is
        #' the number of rows in each data frame that is passed in.
        #' @note The routine assumes that the first item in the list is the data
        #' for 2010, and each subsequent list item is an additional sequential year.
        #' @examples
        #' all_compliance(list(df1,df2,df3))
        #' @author Nick Lukianoff
        #' @export
all_compliance <- function(x) {
        #Show how many companies send in XML reports yearly
        #input a list of data frames from 2010 onwards

        compliance <- data.frame()
        for (i in 1:length(x)) {
                compliance <- rbind(compliance, nrow(x[[i]]))
        }
        years <- (2010:(2009+length(x)))
        compliance <- cbind(compliance,years)
        colnames(compliance)[1] <- "comp"

        plot(compliance$years, compliance$comp, type = "l",
             main = paste("XBRL Compliance 2010-",2009+length(x),sep=""),
             ylab = "Companies", xlab = "Year")
}

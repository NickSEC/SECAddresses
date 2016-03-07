        #' Remove foreign addresses from address file
        #' @description This function strips any data frame row whose
        #' business or mailing addresses don't correspond to a valid US state.
        #' @param x A data frame in the SEC's format
        #' @return A data frame containing only companies registered in a US state
        #' @details This function will strip away any company that contains any
        #' address that is outside of the United States.  In addition, if the
        #' incorporated state field is empty, it get populated from the business
        #' address, or if the business address is blank, from the mailing address.
        #' @note This function removes companies from US territories, as well as
        #' foreign companies.
        #' @examples
        #' x <- remove_nonUSA(df)
        #' @author Nick Lukianoff
        #' @export
remove_nonUSA <- function(x) {
        #Remove any non-US registrations
        x <- subset(x, x$countryinc == "US" |
                            x$countryba == "US" | x$countryma == "US")
        x <- subset(x, x$countryba == "" | x$countryba == "US")
        x <- subset(x, x$countryma == "" | x$countryma == "US")
        x$countryba <-
                ifelse(x$countryba == "", 'US', as.character(x$countryba))
        x$countryma <-
                ifelse(x$countryma == "", 'US', as.character(x$countryma))

        #Fix state names
        x$stprinc <-
                ifelse(x$stprinc == "",
                       as.character(x$stprba),
                       as.character(x$stprinc)) #replace with business state
        x$stprinc <-
                ifelse(x$stprinc == "",
                       as.character(x$stprma),
                       as.character(x$stprinc)) #replace with personal state
        a1 <-
                which(x$stprinc %in% state.abb) #verify that codes are actual US states
        x <- x[a1,] #select only US states
        rm(a1)
        return(x)
}

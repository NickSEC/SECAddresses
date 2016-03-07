        #' Backfill addresses
        #' @description If a business address field is blank, this routine copies
        #' the mailing address into it.  Likewise, if a mailing address field is
        #' blank, it copies the business address into it.
        #' The data is taken from the parsed EDGAR list that the SEC provides.
        #' @param x A data frame in the SEC's format
        #' @return A data frame with non-blank address fields
        #' @details This routine is provided for continuity of the address data in
        #' 10K and 10Q filings.  There are times when only a business address, or
        #' only a mailing address, are provided.  This routine will copy the non-blank
        #' address into the blank address.  The main advantage to this, besides address
        #' data continuity, is in having the business address fields populated.  Some
        #' analytical routines pull their data from the business address, so it's a good
        #' idea to have it fully populated.
        #'The business address fields checked are:  'bas1','bas2','cityba',.stprba','zipba'
        #'The mainling address fields checked are:  'mas1','mas2','cityma',.stprma','zipma'
        #' @note Since it is highly unlikely that both the maling and business
        #' address fields will be blank at the same time, no check is made to see
        #' if this is the case.
        #' @examples
        #' x <- backfill_addresses(df)
        #' @author Nick Lukianoff
        #' @export
backfill_addresses <- function(x) {
        #Backfill addresses
        #use personal address if no busines address on file
for (i in 1:nrow(x)) {
        if (x$bas1[i] == "") {
                x$bas1[i] <- x$mas1[i]
                x$bas2[i] <- x$mas2[i]
                x$cityba[i] <- x$cityma[i]
                x$stprba[i] <- x$stprma[i]
                x$zipba[i] <- x$zipma[i]
        }
}

#use business address if no personal address on file
for (i in 1:nrow(x)) {
        if (x$mas1[i] == "") {
                x$mas1[i] <- x$bas1[i]
                x$mas2[i] <- x$bas2[i]
                x$cityma[i] <- x$cityba[i]
                x$stprma[i] <- x$stprba[i]
                x$zipma[i] <- x$zipba[i]
        }
}
        return(x)
}

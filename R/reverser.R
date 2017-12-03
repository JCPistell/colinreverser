#' Reverses a string or number
#' @param toReverse A string or number
#' @param convert A boolean to indicate if numbers should be converted back to numbers or left as strings.
#' @return the reverse of the provided string or number
#' @examples
#' colin_reverser("foo")
#' @importFrom magrittr "%>%"
#' @export 
colin_reverser <- function(toReverse, convert=TRUE) {
    split <- autoSplit(toReverse)
    r <- rev(split) %>% paste(collapse = "")
    if(convert) {
        tryCatch(as.numeric(r),
                 warning = function(c) return(r)
                 )
    } else {
        r
    }
}

# Helper function to make splitting easier
autoSplit <- function(toSplit) {
    strsplit(as.character(toSplit), "")[[1]]
}

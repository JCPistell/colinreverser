#' Reverses a string or number
#' @param toReverse A string or number
#' @return the reverse of the provided string or number
#' @examples
#' colin_reverser("foo")
#' @importFrom magrittr "%>%"
#' @export 
colin_reverser <- function(toReverse) {
    split <- autoSplit(toReverse)
    rev(split) %>% paste(collapse = "")
}

# Helper function to make splitting easier
autoSplit <- function(toSplit) {
    strsplit(as.character(toSplit), "")[[1]]
}

#' Format string components with padding and optional uppercase
#'
#' This internal utility formats a vector of values by padding them with
#' leading zeros to a specified width. Optionally, the values can be converted
#' to uppercase.
#'
#' @param x A vector of values to format.
#' @param width An integer specifying the target width for each value.
#' @param upper Logical; if TRUE, convert the values to uppercase.
#'
#' @return A character vector with each element formatted to the specified width
#' and optionally in uppercase.
#'
#' @details
#' - Leading spaces are replaced with zeros.
#' - Useful for constructing IDU codes or components consistently.
#'
#' @examples
#' \dontrun{
#' pad0(c(1, 23, 456), width = 5)
#' # Returns: "00001" "00023" "00456"
#'
#' pad0(c("ab", "cd"), width = 4, upper = TRUE)
#' # Returns: "00AB" "00CD"
#' }
#'
#' @keywords internal
#'
pad0 <- function(x, width, upper = FALSE) {
  x <- sprintf(paste0("%", width, "s"), as.character(x))
  x <- gsub(" ", "0", x)
  if (upper) x <- toupper(x)
  x
}

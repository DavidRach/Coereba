#' Deploy a Shiny app to modify the estimated gate cutoffs.
#'
#' @importFrom shiny runApp
#'
#' @export


Coereba_App <- function() {
    runApp(
    system.file("shiny", package="Coereba"),
    display.mode = "normal")
}

#' Deploy a Shiny app to modify Gate Cutoffs
#'
#' @importFrom shiny runApp
#'
#' @export
#'
#'

CoerebaApp <- function() {
    runApp(
    system.file("shinyApp", package="Coereba"),
    display.mode = "normal")
}

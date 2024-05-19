#' Generate the plots into a single pdf
#'
#' @param x A list of plots
#' @param filename Name to save the .pdf as
#' @param outfolder Location to save the .pdf
#' @param thecolumns The number of columns per page
#' @param therows The number of rows per page
#'
#' @importFrom patchwork wrap_plots
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#'
#' @return Some additional value to edit
#' @export
#'
#' @examples NULL
#'

Utility_Patchwork <- function(x, filename, outfolder, thecolumns, therows){
  theList <- x
  theList <- Filter(Negate(is.null), theList)
  theListLength <- length(theList)

  theoreticalitems <- therows*thecolumns
  #theoreticalpages <- theListLength/theoreticalitems
  #theoreticalpages <- round(theoreticalpages, 0)

  split_list <- function(input_list, chunk_size) {
    split(input_list, ceiling(seq_along(input_list) / chunk_size))
  }

  sublists <- split_list(theList, theoreticalitems)
  length(sublists)

  MergedName <- paste(outfolder, filename, sep = "/")

  pdf(file = paste(MergedName, ".pdf", sep = "", collapse = NULL), width = 9,
      height = 7) #Optional Adjustments for Second

  for(i in sublists){p <- wrap_plots(i, ncol = thecolumns, nrow = therows,
                                     widths = 0.8, heights = 0.8)
  print(p)
  }

  dev.off()

}

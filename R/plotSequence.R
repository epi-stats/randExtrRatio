#' Function to plot the imbalance of allocation sequences
#'
#' Function to plot sequences generate with function randomRatio
#' The function assumes that the different trial arms are coded as integer
#' with numbers starting at 1 and has no missing data
#'
#' @param x Vector with the random allocation sequence
#'
#' @param lty Line type as in the generic plot function
#'
#' @return invisible(NULL)
#'
#' @examples
#' test <- randomRatio(5, c(21,21,2,2,8), 500, loops = 1000, returnSeq = 1)
#' head(test)
#' plotSequence(test)
#' test <- randomRatio(5, c(21,21,2,2,8), 500,
#'                     allowedImbalance = c(0.06,0.06, 0.04,0.04, 0.05),
#'                     seed = 1234,
#'                     returnSeq = 5)
#' head(test)
#' plotSequence(test[,1])
#'
#' @export


plotSequence <- function(x, lty = 1) {
  arms <- max(x)

  opar <- par()
  par(mar = c(4, 4, 0, 0) + 0.1)

  n <- table(x)

  plot(
    seq(0, 1, length.out = sum(n)),
    seq(0, 1, length.out = sum(n)),
    type = "l",
    col = "gray",
    xlab = "Allocation over time",
    ylab = "Observed proportions"
  )
  abline(
    h = seq(0.1, 0.9, 0.1),
    v = seq(0.1, 0.9, 0.1),
    col = "gray",
    lty = 3
  )
  abline(
    a = 0,
    b = 1,
    col = "gray",
    lty = 1,
    lwd = 2
  )

  for (i in 1:length(n)) {
    lines(seq(0, 1, length.out = sum(n)),
          cumsum(x == i) / n[i],
          col = i + 1,
          lty = lty)
    mtext(paste("arm", i),
          at = 0.1,
          line = -1 * i,
          col = i + 1)
  }

  par(opar)
  invisible(NULL)
}

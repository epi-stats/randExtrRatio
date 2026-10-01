#' Function to plot the imbalance of allocation sequences
#'
#' Function to plot sequences generate with function \link[randExtrRatio]{randomRatio}
#' The function assumes that the different trial arms are coded as integer
#' with numbers starting at 1 and has no missing data
#'
#' @param x Vector with the random allocation sequence
#'
#' @param colors vector of colors of same length as number of trial arms (default 1:Narms)
#'
#' @param ... further arguments passed to \link[graphics]{lines}, e.g. lty or lwd
#'
#' @return invisible(NULL)
#'
#' @examples
#' test <- randomRatio(5, c(21,21,2,2,8), 500, loops = 1000, returnSeq = 1)
#' head(test)
#' plotSequence(test)
#' test <- randomRatio(5, c(21,21,2,2,8), 500,
#'                     allowedImbalance = c(0.04,0.04, 0.07,0.07, 0.05),
#'                     seed = 1234,
#'                     returnSeq = 5)
#' head(test)
#' plotSequence(test[,1])
#'
#' @export


plotSequence <- function(x, colors = NA, ...) {
  N <- length(x)
  ns <- table(x)
  armid <- names(ns)
  if(is.na(colors)[1]) colors <- (1:length(ns)) + 1
  if(length(colors) != length(ns)) stop("N colors must = N trial arms")

  sequence <- seq(1, N, length.out = N)
  probs <- sapply(armid, function(x)
    cumsum(allo == x) / ns[x])
  pdiff <- probs - sequence / N
  plot(
    sequence,
    rep(0, N),
    type = "l",
    ylim = c(min(pdiff), max(pdiff))*100,
    lwd = 2,
    xlab="N enrolled",
    ylab="% diff. from expected freq."
  )
  for (i in 1:ncol(pdiff)) {
    lines(sequence, pdiff[, i]*100, col = colors[i], ...)
    mtext(
      armid[i],
      at = N * 0.1,
      side = 3,
      line = -i,
      col = colors[i]
    )
  }

  invisible(NULL)
}

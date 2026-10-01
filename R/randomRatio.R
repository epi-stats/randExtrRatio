#' Generate randomisation sequences for large allocation ratios
#'
#' Generates allocation sequences for clinical trials when the allocation
#' ratio is large. In this situation, simple block randomisation may cause
#' problems because a large block size could result in groups being
#' over- or underrepresented.
#'
#' To overcome this limitation, the function combines features of block
#' randomisation, biased coin design, and covariate-constrained randomisation
#' to ensure that the allocation remains close to the anticipated ratio.
#'
#' @param arms Number of trial arms.
#' @param ratios Allocation ratio for each arm, e.g. for 3 arms c(10,5,1).
#' @param totalN Anticipated total number of participants.
#' @param allowedImbalance Maximum tolerated imbalance. Default is 7\%.
#' @param loops Number of allocation sequences to be evaluated. Default is 10000.
#' @param seed Random seed.
#' @param returnSeq Number of valid sequences to be returned. Default is 1;
#'   \code{NA} returns all valid sequences.
#'
#' @return A vector or matrix containing one or more valid allocation sequences.
#'
#' @examples
#' test <- randomRatio(5, c(21, 21, 2, 2, 8), 500, loops = 1000)
#' test
#'
#' test <- randomRatio(5, c(21, 21, 2, 2, 8), 500,
#'                     loops = 1000, returnSeq = 5)
#' head(test)
#'
#' test <- randomRatio(
#'   5,
#'   c(21, 21, 2, 2, 8),
#'   500,
#'   loops = 1000,
#'   allowedImbalance = c(0.03, 0.03, 0.06, 0.06, 0.04),
#'   seed = 1234,
#'   returnSeq = 5
#' )
#' head(test)
#'
#' @export



randomRatio <- function(arms,
                        ratios,
                        totalN,
                        allowedImbalance = NULL,
                        loops = 10000,
                        seed = 1103,
                        returnSeq = 1)   # set to NA for all

{
  # set the allowed max imbalance to 7% if nothing is specified
  if (is.null(allowedImbalance))
    allowedImbalance <- rep(0.07, arms)
  if (length(allowedImbalance) != arms)
    stop("Length of allowedImbalance must be = arms")

  calcDif <- function(x, y, n, obs)
  {
    seq(0, 1, length.out = sum(n)) - cumsum(y[, obs] == x) / n[x]
  }

  set.seed(seed)
  block <- unlist(mapply(rep, 1:arms, each = ratios))
  print(paste("Block size:", length(block)))

  # Generate random sequences. length = N / block size
  allSeq <- replicate(loops, as.integer(replicate(
    ceiling(totalN / length(block)), sample(block)
  )))
  ns <- ceiling(totalN / length(block)) * ratios
  print(paste("N per arm:", paste(ns, collapse = ",")))

  # Check which sequences are never above the maximum allowed imbalance
  valid <- rep(NA, loops)
  for (i in 1:loops) {
    if (i %% 100 == 0)
      cat(". ")
    if (i %% 1000 == 0)
      cat(i)
    dif <- mapply(calcDif,
                  x = 1:arms,
                  MoreArgs = list(y = allSeq, n = ns, obs = i))
    maxAbsDif <- apply(abs(dif), 2, max)
    valid[i] <- all(maxAbsDif < allowedImbalance)
  }
  cat("\n")
  valid <- which(valid)

  # Return NULL if no serquence is valid
  if (length(valid) == 0) {
    print("O valid sequences - relax allowedImbalance or increase loops")
    return(NULL)
  }

  # Return some or all valid sequences
  else
    print(paste(length(valid), "valid sequences"))
  if (is.na(returnSeq))
    return(allSeq[, valid])
  else
    return(allSeq[, sample(valid, returnSeq)])
}




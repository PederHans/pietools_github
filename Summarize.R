
Summarize <- function(df,
                       sumvar,
                       weightvar = "counter",
                       segvar = "segment",
                       meanvars = c("T_LOSS_RATIO_REL_CAPPED_250", "T_LOSS_RATIO_REL_UC")) {
  # data(mtcars)
  # mtcars$mpg2 <- jitter(mtcars$mpg, amount = 5)
  # cutoffs <- GetCutoffs(mtcars, var2seg = "mpg", method = "wtd.quantile", "cyl", nsegs = 5)
  # mtcars$segment <- Segment(df = mtcars, var2seg = "mpg2", bounds = cutoffs)
  # table(mtcars$segment)
  #
  # df = mtcars
  # sumvar = "counter"
  # segvar = "segment"
  # meanvars = c("mpg2", "mpg")

  # get numeric columns
  num.cols <- unlist(lapply(df, is.numeric))
  df <- df[, num.cols]

  # define weight
  df <- data.frame(df)
  df$counter <- 1
  if (is.null(weightvar)) {
    df$weight <- df$counter
  } else {
    df$weight <- df[, weightvar]
  }

  # means
  means <- aggregate(. ~ segment, df[, c(segvar, weightvar, sumvar, meanvars)], mean) # changed
  means$weight <- NULL
  means$counter.1 <- NULL

  # sums
  sums <- aggregate(. ~ segment, df[, c(segvar, "weight")], sum) # changed

  # combine
  means_and_sums <- merge(means, sums, by = segvar) # changed

  return(means_and_sums)

}




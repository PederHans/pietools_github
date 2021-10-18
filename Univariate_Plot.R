Univariate_Plot <- function(  df,
                              xvar,
                              target1,
                              target2,
                              cutoff.method = "equal.interval",
                              nsegs = 5,
                              weightvar = "counter",
                              sumvar = "counter",
                              meanvars = c(target1, target2)) {

  cutoffs <-
    GetCutoffs(
      mtcars,
      var2seg = xvar,
      method = cutoff.method,
      weightvar = weightvar,
      nsegs = nsegs
    )
  print(cutoffs)

  mtcars$segment <-
    Segment(df = mtcars,
            var2seg = xvar,
            bounds = cutoffs)

  mtcars.summarized <-
    Summarize(
      df = mtcars,
      sumvar = weightvar,
      segvar = "segment",
      meanvars = meanvars
    )
  print(mtcars.summarized)

  BarLinePlot2(
    df = mtcars.summarized,
    segvar = "segment",
    linevar1 = target2,
    linevar2 = target1,
    barvar = "weight",
    ylab = 'Mpg2_pred (blue) and Mpg (red)',
    xlab = "Segment",
    y2lab = "weight",
    title = "Univariate Plot",
    x.axis.labels = cutoffs
  )


}

# # # vignette
# data(mtcars)
# mtcars$counter <- 1
# mtcars$mpg_pred <- round(jitter(mtcars$mpg, amount = 5), 2)
# pietools::Univariate_Plot(df = mtcars,
#                xvar = "mpg",
#                target1 = "mpg",
#                target2 = "mpg_pred",
#                cutoff.method = "equal.interval",
#                nsegs = 5,
#                weightvar = "counter",
#                sumvar = "counter",
#                meanvars = c(target1, target2)
# )

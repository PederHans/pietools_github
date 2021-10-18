Lift_Plot <- function(df,
                      target,
                      pred,
                      cutoff.method = "quantile",
                      nsegs = 5,
                      weightvar = "counter",
                      sumvar = "counter",
                      meanvars = c(target1, target2)) {



  cutoffs <-
    GetCutoffs(
      mtcars,
      var2seg = pred,
      method = cutoff.method,
      weightvar = weightvar,
      nsegs = nsegs
    )
  print(pred)
  print(cutoff.method)
  print(cutoffs)

  mtcars$segment <-
    Segment(df = mtcars,
            var2seg = pred,
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
    linevar1 = target,
    linevar2 = pred,
    barvar = "weight",
    ylab = 'Mpg2_pred (blue) and Mpg (red)',
    xlab = "Segment",
    y2lab = "weight",
    title = "Lift Plot",
    x.axis.labels = 1:nsegs
  )


}

# # vignette
# data(mtcars)
# mtcars$counter <- 1
# set.seed(54)
# mtcars$mpg_pred <- round(jitter(mtcars$mpg, amount = 5), 2)
# pietools::Lift_Plot(df = mtcars,
#                target = "mpg",
#                pred = "mpg_pred",
#                cutoff.method = "quantile",
#                nsegs = 5,
#                weightvar = "counter",
#                sumvar = "counter",
#                meanvars = c(target1, target2)
# )

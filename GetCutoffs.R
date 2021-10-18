GetCutoffs <- function(df,
                       var2seg,
                       method = "quantile",
                       weightvar = NA,
                       segname = "segment.no",
                       nsegs = 10) {

  require(Hmisc)

  print(paste("var2seg:", var2seg))
  print(paste("weightvar:", weightvar))

  options(warn = -1)

  nsegs.minus.one <- nsegs -1
  themaxprob <- nsegs.minus.one / nsegs
  theinterval <- 1/nsegs
  theseq <- seq(theinterval, 1, theinterval)
  print("quantile sequence of probailities:")
  print(theseq)

  if (method == "quantile") {

    breaks <- unname(quantile(df[, var2seg],
                              probs = theseq,
                              na.rm = T))

  } else

    if (method == "wtd.quantile") {

      breaks <- unname(Hmisc::wtd.quantile(
        df[, var2seg],
        probs = theseq,
        weight = df[, weightvar],
        na.rm = T
      ))

    } else

      if (method == "equal.interval"){

        nvalues <- length(unique(df[, var2seg]))
        minval <- min(df[, var2seg], na.rm = T)
        maxval <- max(df[, var2seg], na.rm = T)
        range <- maxval - minval
        width <- (range)/(nsegs-1)
        breaks <- seq(minval, maxval, by = width)

      }
  return(breaks)
}



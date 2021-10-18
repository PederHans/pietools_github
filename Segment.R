Segment3 <- function(df,
                     var2seg,
                     bounds = NULL
                     ) {

  # define breaks
  bounds <- unique(bounds)
  breaks <- c(-Inf, cutoffs[-length(bounds)], Inf)

  # segment
  df$segment.no <- as.numeric(cut(
    df[, var2seg],
    breaks = breaks,
    labels = F
  ))
  print("segment counts:")
  print(table(df$segment.no))

  return(df$segment.no)
}

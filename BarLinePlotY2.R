# https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
BarLinePlotY2 <- function(df,
                     segvar,
                     linevar1,
                     linevar2,
                     barvar,
                     title = paste0(linevar1, " by " , linevar2, " with ", barvar, " by ", segvar),
                     xlab = segvar,
                     ylab = paste0(linevar1, " (blue)"),
                     y2lab = paste0(linevar2, " (red)"),
                     ftext = "") {

  df <- mtcars.summarized
  segvar <- "segment"
  linevar1 <- "mpg"
  linevar2 <- "disp"
  barvar <- "weight"
  title <- paste0(linevar1, " by " , linevar2, " with ", barvar, " by ", segvar)
  xlab <- segvar
  ylab <- paste0(linevar1, " (blue)")
  y2lab <- paste0(linevar2, " (red)")
  ftext <- ""

  require(ggplot2)

  # get plotting data
  segment <- rep(df[, segvar], 3)
  metric <- c(rep(linevar1, nrow(df)), rep(linevar2, nrow(df)), rep(barvar, nrow(df)))
  origvalue <- c(df[, linevar1],
                 df[, linevar2],
                 df[, barvar])
  # stack data
  sdf <- data.frame(
    segment = segment
    , metric = metric
    , origvalue = origvalue
  )
  # multiplier <- max(df[metric == linevar2, origvalue]) / max(df[metric == linevar1, origvalue])
  # print(multiplier)
  print("stacked data:")
  print(sdf)
  sdf$dispvalue <- sdf$origvalue
  sdf[metric == linevar2,] <- sdf[metric == linevar2, "origvalue"] * multiplier
  # plot
  mysegs <- as.numeric(row.names(sdf)[sdf$metric == linevar1])
  nsegs <- length(mysegs)
  liftplot <- ggplot(data = sdf, aes(x = segment, y = dispvalue)) +
    geom_bar(data = sdf[sdf$metric == barvar, ],
             stat = "identity",
             show.legend = F)
    geom_line(
      data = sdf[sdf$metric == linevar1,],
      color = "blue",
      linetype = 2,
      aes(col = metric, group = metric),
      size = 2,
      show.legend = F
    )
  #   geom_line(
  #     data = sdf[sdf$metric == linevar2, ],
  #     aes(col = metric, group = metric),
  #     color = "red",
  #     size = 2,
  #     show.legend = F
  #   ) +
  #   scale_x_continuous(breaks = mysegs[c(T, rep(F, nsegs / 10 - 1))],
  #                      name = xlab) +
  #   scale_y_continuous(
  #     sec.axis = sec_axis(~ . * multiplier,
  #                         name = ylab),
  #     name = y2lab,
  #     position = "right"
  #   ) +
    # theme(legend.position = "top") +
    # theme(plot.title = element_text(hjust = 0.5)) +
    # ggtitle(title) +
    # labs(caption = ftext) +
    # theme(plot.caption = element_text(hjust = 0))
  print(liftplot)
}



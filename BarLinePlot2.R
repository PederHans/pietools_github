BarLinePlot2 <- function(df,
                         segvar,
                         linevar1,
                         linevar2,
                         barvar,
                         title = "Lift Plot",
                         xlab = segvar,
                         ylab  = paste(linevar1, "/", linevar2),
                         y2lab = barvar,
                         ftext = "",
                         x.axis.labels
                         ) {

  require(ggplot2)
  require(reldist)

  # # test params
  # df <- mtcars.summarized
  # segvar <- "segment"
  # linevar1 <- "mpg2"
  # linevar2 <- "mpg"
  # barvar <- "cyl"
  # title <- "Lift Plot"
  # xlab <- segvar
  # ylab <- paste(linevar1, "/", linevar2)
  # y2lab <- barvar
  # ftext <- ""
  # x.axis.labels <- df[, 3]


  df <- data.frame(df)
  barvarmax <- max(df[, barvar])
  linevars <- c(linevar1, linevar2)
  linevarmin <- min(df[, linevars])
  linevarmax <- max(df[, linevars])
  multiplier <- barvarmax/(linevarmax - linevarmin)
  offset <- -(multiplier * linevarmin)
  segment <- rep(df[, segvar], 3)
  metric <- c(rep(linevar1, nrow(df)), rep(linevar2, nrow(df)), rep(barvar, nrow(df)))
  origvalue <- c(df[, linevar1],
                 df[, linevar2],
                 df[, barvar])
  dispvalue <- c(df[, linevar1] * multiplier + offset, df[, linevar2] *
                   multiplier + offset, df[, barvar])
  sdf <- data.frame(
    segment = segment
    , metric = metric
    , dispvalue = dispvalue
    , origvalue = origvalue
  )
  print(sdf)
  print(x.axis.labels)
  mysegs <- as.numeric(row.names(sdf)[sdf$metric == linevar1])
  nsegs <- length(mysegs)
  liftplot <- ggplot(data=sdf, aes(x=segment, y=dispvalue)) +
    geom_bar(data=sdf[sdf$metric==barvar, ],
             stat="identity",
             show.legend=F,
             name="Weight")  +
    geom_line(data=sdf[sdf$metric==linevar2, ],
              color="red",
              aes(col=metric, group=metric),
              size=2,
              show.legend=F) +
    geom_line(data=sdf[sdf$metric==linevar1, ],
              aes(col=metric, group=metric),
              color="blue",
              # linetype=2,
              size=2,
              show.legend=F)    +
    xlim(factor(x.axis.labels)) +
    xlab(xlab)  +
    scale_y_continuous(
      sec.axis = sec_axis( ~ . / multiplier - offset / multiplier,
                           name = ylab),
      position = "right",
      name = paste("Sum of", toTitleCase(y2lab))
    )  +
    theme(legend.position="top")  +
    theme(plot.title = element_text(hjust = 0.5))  +
    ggtitle(title) +
    labs(barvar, caption = ftext) +
    theme(plot.caption = element_text(hjust = 0))
  print(liftplot)

}

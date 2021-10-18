Ordered_Gini <- function(df,
                           score,
                           weight = "counter",
                           target,
                           titlepre = paste("Gini Ordered by"),
                           xlab = paste("Cumulative Weight", "(Premium)"),
                           ylab = paste0("Cumulative Target", " (", target, ")"),
                           plot = T,
                           ftext = "") {

  require(data.table)
  require(ggplot2)
  require(scales)
  require(reldist)

  # df <- df
  # # score <- univariate.cols
  # score <- "YEARS_WITH_CARRIER"   # works with 1 column
  # # score <-  c(var2seg, "YEARS_WITH_CARRIER")  # fails with two
  # target <- targetvar1
  # weight <- weightvar

  df <- data.frame(df)

  # sort by score
  df <- df[sort(df[, score]), ]
  df$counter <- 1

  i <- 1

  og.df <- data.table()
  for(i in 1:1) {
    # for(i in 1:length(univariate.cols)) {

    keep.cols <- c(score, target, weight)
    one.df <- df[, keep.cols]
    one.df <- one.df[!is.na(one.df[, score]), ]
    one.df <- one.df[!is.na(one.df[, target]), ]
    one.df <- one.df[!is.na(one.df[, weight]), ]

    # one.df <- one.df[score > 0, ]

    sorted.df <- one.df[order(one.df[, keep.cols[1]]),  ]

    # print(keep.cols[1])

    # print(summary(sorted.df))

    if (is.na(weight)) {
      x <- rep(1, nrow(sorted.df))
    } else {
      x <- as.numeric(sorted.df[, keep.cols[3]])
    }

    y <- as.numeric(sorted.df[, keep.cols[1]])

    px <- x/sum(x, na.rm = 1)
    py <- y/sum(y, na.rm = 1)
    cdfx <- cumsum(px)
    cdfy <-cumsum(py)

    a <- abs(round(sum((cdfx-cdfy)*px), 3))
    og <- 2 * a

    ks <- round(max(abs(cdfy-cdfx)), 3)
    my.values <- df[!is.na(df[, score]), score]
    my.weights <- df[!is.na(df[, score]), weight]

    print("summary of og.check data below:")
    print(length(my.values))
    print(length(my.weights))
    print(summary(my.weights))

    og.check <- round(gini(my.values, my.weights), 3)

    # og is invalid if min is < 0
    min_score <- min(df[, score], na.rm = T)
    if (min_score < 0) {
      print(paste(uni.col, "gini is invalid, min score < 0"))
    }

    # gini values are wrong for negative values
    if (is.na(min_score) | min_score < 0) {
      og <- NA
      og.check <- NA
    }

    ogrow <- cbind(score, og, ks, og.check)
    og.df <- rbind(ogrow, og.df)

    if (isTRUE(plot)) {
      ogplot <- ggplot(sorted.df, aes(x = cdfx,
                                      y = cdfy)) +
        geom_line(color = "blue",
                  size = 1.25) +
        geom_abline(intercept = 0,
                    slope = 1,
                    color = "black",
                    size = 1.25) +
        scale_x_continuous(name=xlab,
                           limits=c(0,1),
                           labels = scales::percent,
                           expand=c(0,0)) +
        scale_y_continuous(name=ylab,
                           limits=c(0,1),
                           labels = scales::percent,
                           expand=c(0,0)) +
        coord_fixed(ratio = .75) +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle(paste0(titlepre, " ", score, "\n(Gini = ", round(og.check*100, 5), "%)")) +
        labs(caption = ftext) +
        theme(plot.caption = element_text(hjust = 0))
      print(ogplot)
      rm(one.df)
    }

  }

  # og.df <- og.df[order(-og)]
  return(og.df)
}




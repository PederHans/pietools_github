Plot_Three_Axes <-
  function(df,
           xvar,
           yvar,
           y2var,
           yvar_label = yvar,
           yvar_color = "red",
           xvar_label = xvar,
           y2var_label = y2var,
           y2var_color = "blue",
           title = paste(toTitleCase(xvar), "by", toTitleCase(yvar), "and", toTitleCase(y2var))) {


  # df <- data
  # xvar <- "day"
  # xvar_label <- "Date Test" # default should be xvar
  # yvar <- "temperature"
  # yvar_color <- "red"
  # yvar_label <- "Temperature (Celsius °)"  # default should be yvar
  # y2var <- "price" # default should be y2var
  # y2var_color <- "blue"
  # y2var_label <- "Price ($)"
  # # title <- paste(xvar, "by", yvar, "and", y2var)
  # title <- "Satan"

  require(pietools)
  require(ggplot2)
  require(tools)

  print(paste("xvar_label", xvar_label))
  print(paste("yvar_label", yvar_label))
  print(paste("y2var_label", y2var_label))
  print(paste("title", title))

  multiplier <- max(df[, y2var]) / max(df[, yvar])
  print(multiplier)

  plot3axes <- ggplot(df, aes(x=df[, xvar])) +
    geom_line( aes(y=df[, yvar]), size=2, color=yvar_color) +
    geom_line( aes(y=df[, y2var] / multiplier), size=2, color=y2var_color) +
    scale_y_continuous(
      name = yvar_label,
      sec.axis = sec_axis(~. * multiplier, name=y2var_label
    )) +
    xlab(xvar_label) +
    theme(
      axis.title.y = element_text(color = yvar_color, size=13),
      axis.title.y.right = element_text(color = y2var_color, size=13)
    ) + ggtitle(title)
    print(plot3axes)

}

# # vignette
# data <- data.frame(
#   day = as.Date("2019-01-01") + 0:99,
#   temperature = runif(100) + seq(1,100)^2.5 / 10000,
#   price = runif(100) + seq(100,1)^1.5 / 10
# )
# summary(data)
# pietools::Plot_Three_Axes(
#   df = data,
#   xvar = "day",
#   yvar = "temperature",
#   y2var = "price",
#   yvar_label = "Temperature (Celsius Degrees)",
#   xvar_label = "Day",
#   y2var_label = "Price ($)",
# )

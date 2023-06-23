#' Plot diurnal changes, removing the effect of meteorology
#'
#' This function calculates the diurnal profile of a pollutant with the effect
#' of meteorology removed. Its primary use is to compare two periods to
#' determine whether there has been a shift in diurnal profile e.g. due to some
#' intervention.
#'
#' @param input_data A data frame to analyse.
#' @param vars The explanatory variables used in the model.
#' @param pollutant Name of the pollutant to apply meteorological normalisation
#'   to.
#' @param dates A vector of dates, length three. These dates are used to
#'   partition the data into two categories (before/after). The date format is
#'   UK e.g. `date = c("19/2/2005", "19/2/2007", "19/2/2010")`.
#' @param ylab Label for y-axis.
#' @export
#' @importFrom rlang .data
#' @return Some data
#' @author David Carslaw
diurnalGbm <-
  function(input_data,
           vars = c("ws", "wd", "hour", "weekday"),
           pollutant = "nox",
           dates = c(
             "01/01/2012", "31/12/2012",
             "31/12/2013"
           ),
           ylab = "value") {
    dates <- lubridate::dmy(dates, tz = attr(input_data$date, "tzone"))

    theData <- openair::selectByDate(input_data, start = dates[1], end = dates[2])

    mod1 <- buildMod(
      theData,
      vars = vars,
      pollutant = pollutant,
      B = 1,
      sam.size = nrow(theData)
    )

    res1 <- plot2Way(mod1, variable = c("weekday", "hour"))

    name1 <-
      paste(format(dates[1], "%d %b %Y"), "to", format(dates[2], "%d %b %Y"))
    names(res1$data)[which(names(res1$data) == "y")] <- name1

    results <- res1


    if (length(dates) == 4) {
      start1 <- dates[3]
      end1 <- dates[4]
    } else {
      start1 <- dates[2] + 24 * 3600 ## start of next day
      end1 <- dates[3]
    }

    theData <- openair::selectByDate(input_data, start = start1, end = end1)

    mod2 <- buildMod(
      theData,
      vars = vars,
      pollutant = pollutant,
      B = 1,
      sam.size = nrow(theData)
    )

    res2 <- plot2Way(mod2, variable = c("weekday", "hour"))

    name2 <-
      paste(format(start1, "%d %b %Y"), "to", format(end1, "%d %b %Y"))
    names(res2$data)[which(names(res2$data) == "y")] <- name2

    results <- merge(res1$data, res2$data, by = c("Hour", "Weekday"))
    results <-
      dplyr::arrange(results, .data$Hour) ## order Hours/weekdays

    ## only need weekday/sat/sun
    ids <- which(results$Weekday %in% c("Sat", "Sun"))
    results$Weekday <- as.character(results$Weekday)
    results$Weekday[-ids] <- "Weekday"

    results <-
      dplyr::group_by(results, .data$Weekday, .data$Hour) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

    results$Weekday <- ordered(
      results$Weekday,
      levels = c("Weekday", "Sat", "Sun"),
      labels = c("Weekday", "Saturday", "Sunday")
    )

    ## difference
    results$difference <- results[[4]] - results[[3]]

    results <- tidyr::pivot_longer(
      results,
      cols = -c(.data$Weekday, .data$Hour, .data$difference),
      names_to = "variable"
    )

    ylim <- range(c(results$difference, results$value)) * 1.03

    # data sets that provide postive and negative differences
    id <- which(results$difference > 0)
    data_neg <- results
    data_neg$difference[id] <- 0

    id <- which(results$difference < 0)
    data_pos <- results
    data_pos$difference[id] <- 0

    plt <-
      ggplot2::ggplot(
        results,
        ggplot2::aes(
          x = .data$Hour,
          y = .data$value,
          colour = .data$variable
        )
      ) +
      ggplot2::geom_line(size = 1) +
      ggplot2::facet_wrap(dplyr::vars(.data$Weekday)) +
      ggplot2::theme(legend.position = "top") +
      ggplot2::geom_ribbon(
        data = data_neg,
        ggplot2::aes(ymin = 0, ymax = .data$difference),
        fill = "dodgerblue",
        colour = "dodgerblue"
      ) +
      ggplot2::geom_ribbon(
        data = data_pos,
        ggplot2::aes(ymin = 0, ymax = .data$difference),
        fill = "firebrick1",
        colour = "firebrick1"
      ) +
      ggplot2::scale_colour_manual(
        values = c("turquoise4", "deeppink"),
        name = "period"
      ) +
      ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18)) +
      ggplot2::ylab(openair::quickText(ylab))

    print(plt)
  }

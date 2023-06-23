#' Quantify most important 2-way interactions
#'
#' @param dw_model Model object from running [buildMod()].
#' @export
#' @return Interaction information.
#' @author David Carslaw
gbmInteractions <- function(dw_model) {
  ## Edited version from dismo package to rank 2-way interactions

  if (!inherits(dw_model, "deweather")) stop("Need to supply a deweather object from buildMod.")

  gbm.object <- dw_model$model

  gbm.call <- gbm.object$call
  n.trees <- gbm.object$n.trees
  depth <- gbm.object$interaction.depth

  alldat <- dw_model$data

  ## don't need date
  if ("date" %in% names(alldat)) {
    alldat <- subset(alldat, select = -date)
  }

  id <- which(names(alldat) == gbm.object$response.name)

  alldat <- alldat[, -id]

  gbm.x <- names(alldat)
  pred.names <- gbm.x

  n.preds <- length(gbm.x)

  cross.tab <- matrix(0, ncol = n.preds, nrow = n.preds)
  dimnames(cross.tab) <- list(pred.names, pred.names)

  cat("Cross tabulating interactions for gbm model with ",
    n.preds, " predictors", "\n",
    sep = ""
  )
  data <- alldat

  for (i in 1:(n.preds - 1)) {
    if (is.vector(data[, i])) {
      x.var <- seq(min(data[, i], na.rm = T),
        max(data[, i], na.rm = T),
        length = 20
      )
    } else {
      x.var <- factor(names(table(data[, i])),
        levels = levels(data[, i])
      )
    }

    x.length <- length(x.var)
    cat(i, " ")
    for (j in (i + 1):n.preds) {
      if (is.vector(data[, j])) {
        y.var <- seq(min(data[, j], na.rm = T),
          max(data[, j], na.rm = T),
          length = 20
        )
      } else {
        y.var <- factor(names(table(data[, j])),
          levels = levels(data[, j])
        )
      }
      y.length <- length(y.var)
      pred.frame <- expand.grid(list(x.var, y.var))
      names(pred.frame) <- c(pred.names[i], pred.names[j])
      n <- 3
      for (k in 1:n.preds) {
        if (k != i & k != j) {
          if (is.vector(data[, k])) {
            pred.frame[, n] <- mean(data[, k], na.rm = T)
          } else {
            temp.table <- sort(table(data[, k]), decreasing = TRUE)
            pred.frame[, n] <- rep(
              names(temp.table)[1],
              x.length * y.length
            )
            pred.frame[, n] <- as.factor(pred.frame[
              ,
              n
            ])
          }
          names(pred.frame)[n] <- pred.names[k]
          n <- n + 1
        }
      }
      prediction <- gbm::predict.gbm(gbm.object, pred.frame,
        n.trees = n.trees, type = "link"
      )

      interaction.test.model <-
        stats::lm(prediction ~ as.factor(pred.frame[, 1]) + as.factor(pred.frame[, 2]))

      interaction.flag <- round(mean(stats::resid(interaction.test.model)^2) *
        1000, 2)
      cross.tab[i, j] <- interaction.flag
    }
  }

  search.index <- ((n.preds^2) + 1) - rank(cross.tab, ties.method = "first")
  n.important <- max(2, round(0.1 * ((n.preds^2) / 2), 0))
  var1.names <- rep(" ", n.important)
  var1.index <- rep(0, n.important)
  var2.names <- rep(" ", n.important)
  var2.index <- rep(0, n.important)
  int.size <- rep(0, n.important)

  for (i in 1:n.important) {
    index.match <- match(i, search.index)
    j <- trunc(index.match / n.preds) + 1
    var1.index[i] <- j
    var1.names[i] <- pred.names[j]
    k <- index.match %% n.preds
    if (k > 0) {
      var2.index[i] <- k
      var2.names[i] <- pred.names[k]
      int.size[i] <- cross.tab[k, j]
    }
  }
  rank.list <- data.frame(
    var1.index, var1.names, var2.index,
    var2.names, int.size
  )
  cat("\n")
  return(list(
    rank.list = rank.list, interactions = cross.tab,
    gbm.call = gbm.object$call
  ))
}

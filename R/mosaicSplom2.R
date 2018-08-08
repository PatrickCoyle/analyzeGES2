#' Create a grid of 2x2 survey-weighted mosaic plots
#'
#' @param factors a vector of factor-type elements
#' @param mar_par a vector of length 2 controlling the margins of the plots
#' @return A grid of 2x2 mosaics, one for each pair of the input factors
#' @examples
#' factors <- c("DROWSY", "HEAVY_TRUCK",  "INT_HWY",  "SEX_IM", "SPEEDREL")
#' mosaicSplom2(factors)
#' @export
mosaicSplom2 <- function (factors = c(),
                          mar_par = rep(0.5, 4))
{
  options(survey.lonely.psu="adjust")

  GES2013.drivers$DUMMY <- 1
  factors <- c("WEIGHT", factors, "DUMMY")
  GES2013.drivers.sub <- subset(GES2013.drivers, select = factors)
  counter <- 1
  ind <- (length(factors)-2)
  mosaic.list <- vector("list", length = ind^2)
  for (i in 1:ind)
  {
    for (j in ind:1)
    {
      grid.newpage()
      if (i == j)
      {
        mosaic.list[[counter]] <- grid.text(paste0(factors[i+1]))
      } else if (i > j) {
        mosaic.list[[counter]] <- grid.text("")
      } else
      {
        NAS <- tapply(GES2013.drivers[,"WEIGHT"],
                      GES2013.drivers[, c(factors[j+1], factors[i+1], "DUMMY")], sum, na.rm=TRUE)
        fmla <- formula(paste0(factors[j+1], "~", factors[i+1]))
        model <- survey::svyglm(fmla,
                        family = quasibinomial(link = logit),
                        design = GES2013.drivers.design,
                        na.action = na.omit)
        if (coef(summary(model))[2, 4] <= .05)
        {
          if (coef(summary(model))[2, 1] >= 0) {
            mosaic.list[[counter]] <- grid.grabExpr(mosaic(fmla,
                                                           data    = NAS,
                                                           legend  = F,
                                                           labels  = F,
                                                           margins = mar_par,
                                                           gp      = gpar(fill = matrix(c("red", "blue", "blue", "red"), 2, 2))
                                                           )
                                                    )
          } else {
            mosaic.list[[counter]] <- grid.grabExpr(mosaic(fmla,
                                                           data    = NAS,
                                                           legend  = F,
                                                           labels  = F,
                                                           margins = mar_par,
                                                           gp      = gpar(fill = matrix(c("blue", "red", "red", "blue"), 2, 2))
                                                           )
                                                    )
          }
        }
        if (coef(summary(model))[2, 4] > .05)
        {
          mosaic.list[[counter]] <- grid.grabExpr(mosaic(fmla,
                                                         data    = NAS,
                                                         legend  = F,
                                                         labels  = F,
                                                         margins = mar_par,
                                                         newpage = F)
                                                  )
        }
      }
      counter <- counter + 1
    }
  }
  mosaic.list <- mosaic.list[!sapply(mosaic.list, is.null)]
  myfun <- get("grid.arrange", asNamespace("gridExtra"))
  do.call(myfun, args = c(mosaic.list,
                          ncol = ind))
}

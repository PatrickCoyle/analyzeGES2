#' The length of a string (in characters).
#'
#' @param fmla_list a list of formulas from which to build svyglm objects
#' @param svydesignObj a svydesign object from which to creat a svyglm object
#' @param test.set a data frame whose variables match the svydesignObj, which is used to build ROC curves based on prediction 
#' @param x.lab x-axis label for the ggplot
#' @param y.lab y-axis label for the ggplot
#' @param main title for the ggplot
#' @return A data frame of ROC curve values for each input formula, and a ggplot of these values
#' @seealso \code{\link{predROC}} which this function wraps
#' @examples
#' @export
model_and_convert <- function(fmla_list, svydesignObj, test.set,
                              x.lab, y.lab, main)
{
  options(survey.lonely.psu="adjust")
  
  newData <- subset(test.set, select=c(predictors, response, "WEIGHT"))
  
  for (i in 1:length(fmla_list))
  {
    model <- survey::svyglm(fmla_list[[i]],
                         family = quasibinomial(link = logit), 
                         design = svydesignObj)
    roc <- predROC(model, newData)
    auc <-  round(sum((sort(roc[, 1])[-1] - sort(roc[, 1])[-nrow(roc)]) * (sort(roc[, 2])[-1] + sort(roc[, 2])[-nrow(roc)])/2), digits=2)
    pred[[i]] <- cbind(roc, AUC=rep(paste("AUC = ", auc), nrow(roc)))
  }
  
  roc <- data.frame(matrix(unlist(lapply(pred, t)), ncol= 3, byrow=T), stringsAsFactors=FALSE)
  ggplot2::ggplot(roc, aes(x = FPR, y = TPR)) + geom_line(aes(color=AUC), size=1.6) + 
    theme_bw() +
    scale_color_manual(values = wes_palette("Moonrise2")) + 
    geom_abline() + 
    guides(colour = guide_legend(override.aes = list(size = 10))) + 
    theme(legend.title=element_blank(), aspect.ratio=1) +
    labs(x = x.lab, y = y.lab, title = main) 
}
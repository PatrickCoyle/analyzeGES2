#' Helper function for WeightedROC::WeightedROC, to produce a data frame of ROC curve values based on a svyglm object and a test set
#'
#' @param glm.obj a svyglm object
#' @param newData a data frame whose variables are compatible with the svyglm object, for use in prediction
#' @param pot a boolean indicating whether or not to generate a plot
#' @return A data frame of ROC curve values based on a svyglm object and a test set
#' @seealso
#' @examples
#' data(list = c("GES2013.drivers.design", "GES2013.drivers"))
#' model <- svyglm(DROWSY ~ HEAVY_TRUCK + INT_HWY + SEX_IM + SPEEDREL,
#'                             family = quasibinomial(link=logit),
#'                             design = GES2013.drivers.design)
#' predROC(model,GES2013.drivers)
#' @export
predROC <- function (glm.obj, newData, plot = TRUE)
{
  pred <- rep(NA, nrow(newData)); names(pred) <- rownames(newData)
  model_terms <- attributes(glm.obj$terms)$variables
  predictors <- as.character(model_terms[3:length(model_terms)])
  response <-  as.character(model_terms[2])
  newData <- newData[,c("WEIGHT",response,predictors)]
  xnn <- na.omit(newData)
  pred[-attr(xnn, "na.action")] <- predict(glm.obj, xnn)
  guess <- 1/(1+exp(-pred))
  dframe <- data.frame(response=ifelse(newData[, response]==0, -1, 1),
                       guess=guess,
                       WEIGHT=newData$WEIGHT)
  dframe <- na.omit(dframe)
  subset(WeightedROC::WeightedROC(dframe$guess, dframe$response, weight=dframe$WEIGHT), select=c(FPR, TPR))
}

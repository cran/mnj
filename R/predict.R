#' Predict function
#'
#' A custom predict function for FlexBoost
#'
#' This is a predict function of FlexBoost. FlexBoost consists of two predict functions.
#' One is built-in function in R and the other is this custom predict function for FlexBoost.
#' This custom predict function is needed for the calculation of the final strong classifier.
#' It returns the expected input data's labels.
#' @param object   Tree information
#' @param X        Variable of train data
#' @param type     Class or probability
#' @param n_tree   Number of trees
#' @examples
#' data <- read.csv(url("http://bit.ly/flex_iris"), TRUE)
#' model <- flex(data[,1:2], data[,6], 10, 0.1, 3, 2)
#' mnj.pred(model, data[,1:2], "response", NULL)

mnj.pred <- function(object, X, type = c("response", "prob"), n_tree = NULL){

  # handle args
  type <- match.arg(type)

  if(is.null(n_tree)) { tree_seq <- seq_along(object$alphas) }

  else                { tree_seq <- seq(1, n_tree) }

  # evaluate score function on sample
  f <- 0

  for(i in tree_seq){
    tree       <- object$trees[[i]]
    tree$terms <- object$terms
    pred       <- as.integer(as.character(stats::predict(tree, data.frame(X), type = "class")))
    f          <- f + object$alphas[i] * pred
  }

  # handle response type
  if(type == "response")  { sign(f) }

  else if(type == "prob") { 1/(1 + exp(-2 * f)) }
}

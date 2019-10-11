#' FlexBoost
#'
#' A Flexible Boosting Algorithm With Adaptive Loss Functions
#'
#' This is a main algorithm of FlexBoost: like other Boosting packages, it returns compatible information.
#' In order to prevent unexpected errors, missing data should not be allowed in input data.
#' Return value is composed of four major parts (e.g. terms, trees, alphas, acc).
#' terms : Input variable information
#' trees : Decision tree information
#' alphas : Weight of weak classifier
#' acc : Train accuracy of each iteration
#' @param X        Variable of train data
#' @param y        Label of train data
#' @param n_rounds How many trees gonna make
#' @param interval Parameter to change Exp Loss-Function
#' @param width    Searching area (more than 1)
#' @param type     Tie evaluation option  (1 or 2, recommed 2)
#' @param control  fix cp = -1, maxdepth = 1 based on AdaBoost
#' @return Returns decision tree informations (e.g. Split criteria, Weight of weak classifier, Train accuracy)
#' @keywords FlexBoost
#' @import rpart
#' @export flex
#' @export mnj.pred
#' @examples
#' data <- read.csv(url("http://bit.ly/flex_iris"), TRUE)
#' flex(data[,1:2], data[,6], 10, 0.1, 3, 2)


flex <- function(X, y,
                      n_rounds,
                      interval,
                      width,
                      type,
                      control = rpart.control(cp = -1, maxdepth = 1)){

  k.list <- c(rbind(1 / (1 + interval * seq(width)), 1 + interval * seq(width)))

  # count the number of rows
  n           <- nrow(X)


  # save parameter k path, globalize path to see after function
  k.path      <- list()


  # initialize (weight, tree, alpha) on each data
  w           <- list(rep(1/n, n))
  trees       <- list()
  alphas      <- list()


  # save (weight, tree, alpha) of 3 ways
  temp.w      <- list()
  temp.trees  <- list()
  temp.alphas <- list()


  # save train accuracy of 3 ways to compare
  temp.result <- list()

  # save train accuracy
  acc.result  <- list()

  # build weak classifiers
  for(i in seq(n_rounds)){


    tree <- rpart::rpart(y ~ .,
                         data = data.frame(X), weights = w[[i]],
                         method = "class", control = control,
                         x = FALSE, y = FALSE, model = TRUE)


    pred      <- as.integer(as.character(stats::predict(tree, data.frame(X), type = "class")))


    # calculate the error of each classifiers
    e         <- sum(w[[i]] * (pred != y))


    # if error >= 0.5, flip the result
    if(e >= 0.5){e <- 1 - e}

    if(e < 1e-5){
      # if first base classifier predicts data perfectly, boosting process ends
      if(i == 1){
        # first base classifier's weight should be 1
        alphas[[i]]     <- 1
        trees[[i]]      <- tree
        terms           <- tree$terms
        acc.result[[i]] <- 1
        break
      }
      break
    }

    # count number of searching area
    n_count  <- 0

    for(i1 in k.list){

      # count number of 3 ways
      n_count <- n_count + 1

      # update weak classifier weight
      alpha   <- 1/(2*i1) * log((1-e)/e)


      # update and normalize weight of each data
      temp.w[[n_count]]     <- w[[i]] * exp(-alpha*pred*y*i1)
      temp.w[[n_count]]     <- temp.w[[n_count]] / sum(temp.w[[n_count]])


      # Remove formulas since they waste memory
      if(i == 1)  { terms       <- tree$terms }

      else        { tree$terms  <- NULL }


      alphas[[i]] <- alpha
      trees[[i]]  <- tree

      # save alpha, tree of 3 ways
      temp.alphas[[n_count]] <- alpha
      temp.trees[[n_count]]  <-tree


      result                 <- list(terms  = terms,
                                     trees  = trees,
                                     alphas = unlist(alphas))

      class(result) <- "mnj"

      y_hat                   <- mnj.pred(result, X, "response", NULL)


      # save train accuracy
      temp.result[[n_count]]  <- sum(y == y_hat) / length(y)
    }

    # clear waste memory
    w[[i]] <- NULL

    acc.result[[i]] <- max(unlist(temp.result))

    if(type == 1){
      if (max(unlist(temp.result)) == 1){
        alphas[[i]] <- temp.alphas[[which.max(temp.result)]]
        trees[[i]]  <- temp.trees[[which.max(temp.result)]]
        acc.result[[i]] <- 1
        break
      }
      w[[i+1]] <- temp.w[[which.max(temp.result)]]
      k.path[[i]] <- k.list[[which.max(temp.result)]]
      trees[[i]] <- temp.trees[[which.max(temp.result)]]
      alphas[[i]] <- temp.alphas[[which.max(temp.result)]]
    }else if(type == 2){
      temp.k <- max(which((max(unlist(temp.result)) == temp.result) == TRUE))
      if (max(unlist(temp.result)) == 1){
        alphas[[i]] <- temp.alphas[[temp.k]]
        trees[[i]]  <- temp.trees[[temp.k]]
        acc.result[[i]] <- 1
        break
      }
      w[[i+1]] <- temp.w[[temp.k]]
      k.path[[i]] <- k.list[[temp.k]]
      trees[[i]] <- temp.trees[[temp.k]]
      alphas[[i]] <- temp.alphas[[temp.k]]
    }


  }

  result        <- list(terms  = terms,
                        trees  = trees,
                        alphas = unlist(alphas),
                        acc = unlist(acc.result))

  class(result) <- "mnj"

  return(result)
}



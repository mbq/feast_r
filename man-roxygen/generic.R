#' @param X Predictor table, given as a matrix or data frame with only factor or logical columns. \code{NA}s are not allowed.
#' @param Y Decision attribute; must be either a factor or a logical vector. \code{NA}s are not allowed.
#' @param k Number of attributes to select; first element will be used. Must not exceed \code{ncol(X)}.
#' @return A vector of names of the selected features; note that it may be shorter than \code{k}, but should always contain at least one feature.

Seasonal_Tau = function(x, y, block = 1) {
  if (anyNA(x)) {
    stop("x variable contains NA values")
  }
  if (anyNA(y)) {
    stop("y variable contains NA values")
  }
  if (anyNA(block)) {
    stop("block variable contains NA values")
  }
  if (length(x) != length(y)) {
    stop("x and y must be the same length")
  }
  if (!(length(block) %in% c(length(x), 1))) {
    stop(
      "block variable must either be a single value or a vector the same length as x and y variables"
    )
  }
  blocks = unique(block)
  n_blocks = length(blocks)
  sign2=function(x1,x2){
    x_diff=x2-x1
    return((x_diff>0)-(x_diff<0))
  }
  signs = vector(mode = "list", length = n_blocks)
  
  for (i in 1:n_blocks) {
    x_b = x[block == blocks[i]]
    y_b = y[block == blocks[i]]
    od = order(x_b)
    x_b = x_b[od]
    y_b = y_b[od]
    sign_x = outer(x_b, x_b, FUN = sign2)
    sign_y = outer(y_b, y_b, FUN = sign2)
    ind = upper.tri(sign_x)
    signs[[i]] = sign_x[ind] * sign_y[ind]
  }
  return(mean(unlist(signs)))
}
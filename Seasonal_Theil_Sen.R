Seasonal_Theil_Sen = function(x, y, block = 1) {
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
  slopes = vector(mode = "list", length = n_blocks)
  
  for (i in 1:n_blocks) {
    x_b = x[block == blocks[i]]
    y_b = y[block == blocks[i]]
    od = order(x_b)
    x_b = x_b[od]
    y_b = y_b[od]
    d_x = outer(x_b, x_b, FUN = "-")
    d_y = outer(y_b, y_b, FUN = "-")
    ind = lower.tri(d_x) & (d_x != 0)
    slopes[[i]] = d_y[ind] / d_x[ind]
  }
  return(median(unlist(slopes)))
}
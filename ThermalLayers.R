thermal.layers = function(temp,
                          depth,
                          max_grad = -1,
                          min_length = 4,
                          min_range = 1,
                          spline_approx = TRUE,
                          epsilon = .5) {
  if (anyNA(depth) |
      anyNA(temp)) {
    stop("temp or depth contains NA Values")
  }
  if (any(duplicated(depth))) {
    stop("depth values must be unique")
  }
  if (any(sort(depth) != depth)) {
    stop("depth values must be sorted in ascending order")
  }
  n = length(depth)
  if ((n < min_length) | (diff(range(temp)) < min_range)) {
    return(rep("No Thermocline", n))
  }
  else {
    if (spline_approx) {
      t_spline = smooth.spline(depth, temp)
      t_grad = rep(NA, n)
      depth_m = min(depth)
      depth_M = max(depth)
      for (i in 1:n) {
        depths = c(max(c(depth[i] - epsilon, depth_m)), min(c(depth[i] + epsilon, depth_M)))
        t_vals = predict(t_spline, depths)$y
        t_grad[i] = diff(t_vals) / diff(depths)
      }
    }
    else {
      t_grad = diff(temp) / diff(depth)
    }
    if ((min(t_grad) > max_grad) | (any(is.na(t_grad)))) {
      return(rep("No Thermocline", n))
    }
    else {
      thermo_ind = which(t_grad <= max_grad)
      thermo_1 = depth[min(thermo_ind)]
      thermo_2 = depth[max(thermo_ind)]
      layers = rep(NA, n)
      layers[(depth >= thermo_1) &
               (depth <= thermo_2)] = "Thermocline"
      layers[depth < thermo_1] = "Epilimnion"
      layers[depth > thermo_2] = "Hypolimnion"
      return(layers)
    }
  }
}

Bootstrap_SCI = function(theta_samp, alpha = .05, ...) {
  theta_rank = sapply(theta_samp, rank, ... = ...)
  max_rank = apply(theta_rank, MARGIN = 1, max)
  q_u = quantile(max_rank, 1 - alpha / 2)
  phi = theta_samp[max_rank <= q_u, ]
  phi_rank = sapply(phi, rank, ... = ...)
  min_rank = apply(phi_rank, MARGIN = 1, min)
  q_l = quantile(min_rank, alpha / (2 - alpha))
  psi = phi[min_rank >= q_l, ]
  
  SCI_L = sapply(psi, min)
  SCI_U = sapply(psi, max)
  SCI = data.frame(names(theta_samp), SCI_L, SCI_U, row.names = NULL)
  names(SCI) = c("Parameter", paste((alpha / 2) * 100, "%"), paste((1 - alpha / 2) * 100, "%"))
  return(SCI)
}
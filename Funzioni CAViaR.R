############################## 
# This script contains the functions to implement the CAViaR models described in
# "CAViaR: Conditional Autoregressive Value at Risk by Regression Quantiles"
# by R. F. Engle and S. Manganelli (2004), Journal of Business & Economic Statistics
############################## 

# CAViaR specifications
CAViaR.fun = function(beta, ret, tau, model, G) {
  
  N = length(ret)
  VaR = matrix(NA, N, 1)
  N1 = min(300, N/3)
  VaR[1] = quantile(ret[1:N1], probs = tau)
  
  if (model == "AD") {
    for(t in 2:N) {
      VaR[t] = VaR[t-1] + beta * ((1 + exp(G * (ret[t-1] - VaR[t-1])))^(-1) - tau)
    }
  } else if (model == "SAV") {
    for(t in 2:N) {
      VaR[t] = beta[1] + beta[2] * VaR[t-1] + beta[3] * abs(ret[t-1])
    }
  } else if (model == "AS") {
    for(t in 2:N) {
      VaR[t] = beta[1] + beta[2] * VaR[t-1] + (beta[3] * (ret[t-1] > 0) + beta[4] * (ret[t-1] < 0)) * abs(ret[t-1])
    }
  } else if (model == "IG") {
    for(t in 2:N) {
      VaR[t] = - sqrt(abs(beta[1] + beta[2] * VaR[t-1]^2 + beta[3] * ret[t-1]^2))
    }
  }
  
  return(VaR)
}

# Quantile Loss
QL = function(beta, ret, tau, model, G){
  
  N = length(ret)
  if(N < 300) stop("Length of ret should be at least 300")
  if (!model %in% c("AD", "SAV", "AS", "IG")) stop("Wrong MODEL selected")
  
  VaR = CAViaR.fun(beta, ret, tau, model, G)
  
  Hit = ifelse(ret <= VaR, 1, 0)
  out = (tau - Hit) * (ret - VaR)
  
  return(mean(out))
}

# CAViaR estimation function
est.CAViaR = function(ret, tau, model, n.start, G) {
  
  if(missing(n.start)) n.start = 10
  if(model == "AD") start_value = matrix(runif(n.start, -1, 1), n.start, 1)
  if(model == "SAV") start_value = matrix(runif(n.start * 3, -1, 1), n.start, 3)
  if(model == "AS") start_value = matrix(runif(n.start * 4, -1, 1), n.start, 4)
  if(model == "IG") start_value = matrix(runif(n.start * 3, -1, 1), n.start, 3)
  QL_value = matrix(NA, n.start, 1)
  pars_vec = matrix(NA, n.start, ncol(start_value))
  
  if(missing(G)) G = 10
  if(model == "AD") {
    method = "Brent"
    low = -10
    up = 10
  } else {
    method = "Nelder-Mead"
    low = -Inf
    up = Inf
  }
  
  for(i in 1:n.start){
    optim.out = optim(start_value[i,], fn = QL, ret = ret, tau = tau, model = model, G = G, 
                      method = method, lower = low, upper = up, control = list(reltol = 1e-10, abstol = 1e-10, maxit = 1e4)) 
    QL_value[i] = optim.out$value
    pars_vec[i,] = optim.out$par
  }
  QL_max = QL_value[which.min(QL_value)]
  pars_optim = pars_vec[which.min(QL_value),]
  names(pars_optim) = paste("beta", 1:length(pars_optim), sep = "")
  
  fitted_VaR = CAViaR.fun(pars_optim, ret = ret, tau = tau, model = model, G = G)
  Hit = ifelse(ret <= fitted_VaR, 1, 0)
  
  cat("Quantile level:", tau,
      "\nCoefficients:", pars_optim,
      "\nQL:", QL_max,
      "\nProportion of VaR violations:", mean(Hit))
  
  out = list(tau = tau, pars = pars_optim, QL = QL_max, fitted = fitted_VaR, VaR_violations = Hit)
  return(out)
}

#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: Ordinary Least Squares Exponential Regression to calculate Space Time Separable Covariance Model
#study area: Southeast

# objective function using a series of equations recursively by alpha to calculate
# a nested gaussian exponential fxn that minimizes SSE and then                 
# averages the alpha1, alpha2, ar1, ar2, at1, at2 by month to get a stable model 
# per month using a plausible range of variables as ar = c(0.1,100) and at = c(0.1,1000)

library(here)
here::i_am("scripts/04_covariance_heat_index.R")

# Load Libraries & Functions ----
source(here("scripts", "01_library.R"))

## Monthly ----
year_month <- yearmo(start.year = 1940,end.year = 2023)

# Create data frames
stm <- data.frame(year_mo = year_month, sill = NA,
                  alpha1 = NA, alpha2 = NA,
                  at1 = NA, ar1 = NA, 
                  at2 = NA, ar2 = NA, 
                  sse = NA,
                  stm1.numerator = NA, stm1.denominator = NA,
                  stm1 = NA, stm2 = NA, model = NA)

stm.exp.exp <- data.frame(year_mo = year_month, sill = NA,
                          alpha1 = NA, alpha2 = NA,
                          at1 = NA, ar1 = NA, 
                          at2 = NA, ar2 = NA, 
                          sse = NA,
                          stm1.numerator = NA, stm1.denominator = NA,
                          stm1 = NA, stm2 = NA)

stm.gau.exp <- data.frame(year_mo = year_month, sill = NA,
                          alpha1 = NA, alpha2 = NA,
                          at1 = NA, ar1 = NA, 
                          at2 = NA, ar2 = NA, 
                          sse = NA,
                          stm1.numerator = NA, stm1.denominator = NA,
                          stm1 = NA, stm2 = NA)

stm.exp <- data.frame(year_mo = year_month, sill = NA,
                      at = NA, ar = NA,  
                      sse = NA, stm = NA)

stm.gau <- data.frame(year_mo = year_month, sill = NA,
                      at = NA, ar = NA,  
                      sse = NA, stm = NA)

# create figure list
fitted.cov.plots <- list()

# define index
stm.index <- 1

# define plausible range for ar and at values
lower.bounds <- c(0.1,0.1,0.1,0.1) # at1, ar1, at2, ar2
upper.bounds <- rep(500,50,1000,100) # at1, ar1, at2, ar2

# Initial guesses for the parameters
initial_guess_2 <- rep(25,2)  # Initial guesses for a, b
initial_guess_4 <- rep(25,4)  # Initial guesses for a, b, c, d

for (year in year_month){
  
  # debug
  # year <- seq(1940,2023,1)[1]
  print(year)
  
  # Read in experimental covariance
  space <- read.csv(here::here("data", "output", "02_covariance", "02_experimental_covariance", "heat_index", "month",
                               paste0("heat_index_experimental_covariance_space_", year, ".txt")))
  time <- read.csv(here::here("data", "output", "02_covariance", "02_experimental_covariance", "heat_index", "month",
                              paste0("heat_index_experimental_covariance_time_", year, ".txt"))) 
  
  # Convert into df
  colnames(space) <- NULL
  colnames(time) <- NULL
  
  # Time
  time <- data.frame(lag = unlist(as.vector(time[1:(length(time)/2)])),
                     cov.experimental = unlist(as.vector(time[(length(time)/2+1):length(time)])))
  
  # Space
  space <- data.frame(lag = unlist(as.vector(space[1:(length(space)/2)])),
                      cov.experimental = unlist(as.vector(space[(length(space)/2+1):length(space)])))
  
  # Define covariance sill
  space.sill <- space$cov.experimental[1]
  time.sill  <- time$cov.experimental[1]
  
  # Test whether sills are equal
  if (space.sill != time.sill){
    
    print('Sills are NOT equal; using spatial sill')
    
    sill <- space.sill
    
  } else(sill <- space.sill)
  
  # OLS Regression
  
  ### exp exp ----
  
  # repeat with idealized initial values
  # Setup list to fill with all results
  exp.exp.sse <- list()
  exp.exp.params <- list()
  index <- 1
  
  # Define alpha values, determined by professional judgement
  for (i in rev(seq(0.5,1,0.01))) {
    
    # Define alpha values
    alpha1 <- i
    alpha2 <- 1 - alpha1
    
    # Minimize the SSE
    result <- optim(
      par = initial_guess_4,          # Initial parameter guesses
      fn = sse_function_exp_exp,    # Function to minimize
      method = "L-BFGS-B",          # Optimization method with bounds
      lower = lower.bounds,         # Lower bounds for parameters: make physically plausible
      upper = upper.bounds          # Upper bounds for parameters: make physically plausible
    )
    
    # Save result to list
    exp.exp.sse[[index]] <- result$value
    exp.exp.params[[index]] <- result$par
    
    # Update list index
    index <- index + 1
    
  }
  
  # Determine optimal values
  optimal.exp.exp.sse <- unlist(exp.exp.sse)
  optimal.exp.exp.pos <- which.min(optimal.exp.exp.sse)
  optimal.exp.exp.params <- exp.exp.params[[optimal.exp.exp.pos]]
  optimal.exp.exp.alpha1 <- rev(seq(0.5,1,0.01))[[optimal.exp.exp.pos]]
  optimal.exp.exp.alpha2 <- 1 - optimal.exp.exp.alpha1
  
  # Update original df's with modeled values
  exp.exp.model.time <- data.frame(lag = seq(min(time$lag),max(time$lag),0.1))
  exp.exp.model.time$covariance <- sill * (optimal.exp.exp.alpha1 * exp((-3 * (exp.exp.model.time$lag)^2) /
                                                                          (3 * (optimal.exp.exp.params[1])^2)) +
                                             optimal.exp.exp.alpha2 * exp((-3 * exp.exp.model.time$lag) /
                                                                            optimal.exp.exp.params[3]))
  
  exp.exp.model.space <- data.frame(lag = seq(min(space$lag),max(space$lag),0.1))
  exp.exp.model.space$covariance <- sill * (optimal.exp.exp.alpha1 * exp((-3 * (exp.exp.model.space$lag)^2) /
                                                                           (3 * (optimal.exp.exp.params[2])^2)) +
                                              optimal.exp.exp.alpha2 * exp((-3 * exp.exp.model.space$lag) /
                                                                             optimal.exp.exp.params[4]))
  
  # plot
  exp.exp.plot.time <- ggplot() +
    geom_line(data = exp.exp.model.time,aes(x = lag, y = covariance)) + 
    geom_point(data = time,aes(x = lag, y = cov.experimental)) + 
    xlab('Temporal Lag (days)') +
    ylab(bquote('Covariance of Heat Index '~('°C'^2))) +
    ylim(0,sill) + 
    xlim(0,10) + 
    guides(colour="none") +
    annotate("text", x=15/2, y=sill,
             label= paste0(year,' SSE = ',
                           round(min(optimal.exp.exp.sse),2))) +
    theme_bw()
  
  exp.exp.plot.space <- ggplot() +
    geom_line(data = exp.exp.model.time,aes(x = lag, y = covariance)) + 
    geom_point(data = space,aes(x = lag, y = cov.experimental)) + 
    xlab('Spatial Lag (quarter degree)') +
    ylab(bquote('Covariance of Heat Index '~('°C'^2))) +
    ylim(0,sill) + 
    xlim(0,12) + 
    guides(colour="none") +
    theme_bw()
  
  exp.exp.plot <- ggpubr::ggarrange(exp.exp.plot.space,exp.exp.plot.time,nrow=1,ncol=2)
  exp.exp.plot <- ggpubr::annotate_figure(exp.exp.plot,                                                                  top = text_grob(substitute(
    C[x] * (r~","~τ) == cov.sill %*% (alpha1 %*% exp(frac(-3 * r, ar1)) %*% exp(frac(-3 * τ, at1)) + alpha2 %*% exp(frac(-3 * r, ar2)) %*% exp(frac(-3 * τ, at2))), 
    list(cov.sill = round(sill,1),
         alpha1 = optimal.exp.exp.alpha1,
         alpha2 = optimal.exp.exp.alpha2,
         ar1 = round(optimal.exp.exp.params[2],0),
         at1 = round(optimal.exp.exp.params[1],0),
         ar2 = round(optimal.exp.exp.params[4],0),
         at2 = round(optimal.exp.exp.params[3],0)))))
  
  # update stm dataframe
  stm.exp.exp[stm.index,2] <- sill
  stm.exp.exp[stm.index,3] <- optimal.exp.exp.alpha1
  stm.exp.exp[stm.index,4] <- optimal.exp.exp.alpha2
  stm.exp.exp[stm.index,5] <- optimal.exp.exp.params[1] # at1
  stm.exp.exp[stm.index,6] <- optimal.exp.exp.params[2] # ar1
  stm.exp.exp[stm.index,7] <- optimal.exp.exp.params[3] # at2
  stm.exp.exp[stm.index,8] <- optimal.exp.exp.params[4] # ar2
  stm.exp.exp[stm.index,9] <- min(optimal.exp.exp.sse)
  stm.exp.exp[stm.index,10] <- optimal.exp.exp.alpha1*optimal.exp.exp.params[2]+optimal.exp.exp.alpha2*optimal.exp.exp.params[1]
  stm.exp.exp[stm.index,11] <- optimal.exp.exp.alpha1*optimal.exp.exp.params[4]+optimal.exp.exp.alpha2*optimal.exp.exp.params[3]
  stm.exp.exp[stm.index,12] <- (optimal.exp.exp.alpha1*optimal.exp.exp.params[2]+optimal.exp.exp.alpha2*optimal.exp.exp.params[1]) /
    (optimal.exp.exp.alpha2*optimal.exp.exp.params[4]+optimal.exp.exp.alpha2*optimal.exp.exp.params[3])
  stm.exp.exp[stm.index,13] <- optimal.exp.exp.alpha1*(optimal.exp.exp.params[2]/optimal.exp.exp.params[1])+
    optimal.exp.exp.alpha2*(optimal.exp.exp.params[4]/optimal.exp.exp.params[3])
  
  ## exp ----
  
  # Minimize the SSE
  result <- optim(
    par = initial_guess_2,      # Initial parameter guesses
    fn = sse_function_exp,    # Function to minimize
    method = "L-BFGS-B",      # Optimization method with bounds
    lower = lower.bounds,     # Lower bounds for parameters: make physically plausible
    upper = upper.bounds      # Upper bounds for parameters: make physically plausible
  )
  
  # Determine optimal values
  optimal.exp.sse <- result$value
  optimal.exp.params <- result$par # at1, ar1
  
  # Update original df's with modeled values
  exp.model.time <- data.frame(lag = seq(min(time$lag),max(time$lag),0.1))
  exp.model.time$covariance <- sill * exp(-3 * (exp.model.time$lag) / optimal.exp.params[1])
  
  exp.model.space <- data.frame(lag = seq(min(space$lag),max(space$lag),0.1))
  exp.model.space$covariance <- sill * exp(-3 * (exp.model.space$lag) / optimal.exp.params[2])
  
  # plot
  exp.plot.time <- ggplot() +
    geom_line(data = exp.model.time,aes(x = lag, y = covariance)) + 
    geom_point(data = time,aes(x = lag, y = cov.experimental)) + 
    xlab('Temporal Lag (days)') +
    ylab(bquote('Covariance of Heat Index '~('°C'^2))) +
    ylim(0,sill) + 
    xlim(0,10) + 
    guides(colour="none") +
    annotate("text", x=15/2, y=sill,
             label= paste0(year,' SSE = ',
                           round(optimal.exp.sse,2))) +
    theme_bw()
  
  exp.plot.space <- ggplot() +
    geom_line(data = exp.model.space,aes(x = lag, y = covariance)) + 
    geom_point(data = space,aes(x = lag, y = cov.experimental)) + 
    xlab('Spatial Lag (quarter degree)') +
    ylab(bquote('Covariance of Heat Index '~('°C'^2))) +
    ylim(0,sill) + 
    xlim(0,12) + 
    guides(colour="none") +
    theme_bw()
  
  exp.plot <- ggpubr::ggarrange(exp.plot.space,exp.plot.time,nrow=1,ncol=2)
  exp.plot <- ggpubr::annotate_figure(exp.plot,
                                      top = text_grob(substitute(
                                        C[x] * (r~","~τ) == cov.sill %*% exp(frac(-3 * r, ar1)) %*% exp(frac(-3 * τ, at1)), 
                                        list(cov.sill = round(sill,1),
                                             ar1 = round(optimal.exp.params[2],0),
                                             at1 = round(optimal.exp.params[1],0)))))
  
  # update stm dataframe
  stm.exp[stm.index,2] <- sill
  stm.exp[stm.index,3] <- optimal.exp.params[1]
  stm.exp[stm.index,4] <- optimal.exp.params[2]
  stm.exp[stm.index,5] <- optimal.exp.sse
  stm.exp[stm.index,6] <- optimal.exp.params[2] / optimal.exp.params[1]
  
  ## gau ----
  
  # Minimize the SSE
  result <- optim(
    par = initial_guess_2,      # Initial parameter guesses
    fn = sse_function_gau,    # Function to minimize
    method = "L-BFGS-B",      # Optimization method with bounds
    lower = lower.bounds,     # Lower bounds for parameters: make physically plausible
    upper = upper.bounds      # Upper bounds for parameters: make physically plausible
  )
  
  # Determine optimal values
  optimal.gau.sse <- result$value
  optimal.gau.params <- result$par # at1, ar1
  
  # Update original df's with modeled values
  gau.model.time <- data.frame(lag = seq(min(time$lag),max(time$lag),0.1))
  gau.model.time$covariance <- sill * exp((-3 * (gau.model.time$lag)^2) / (3 * (optimal.gau.params[1])^2))
  
  gau.model.space <- data.frame(lag = seq(min(space$lag),max(space$lag),0.1))
  gau.model.space$covariance <- sill * exp((-3 * (gau.model.space$lag)^2) / (3 * (optimal.gau.params[2])^2)) 
  
  # plot
  gau.plot.time <- ggplot() +
    geom_line(data = gau.model.time,aes(x = lag, y = covariance)) + 
    geom_point(data = time,aes(x = lag, y = cov.experimental)) + 
    xlab('Temporal Lag (days)') +
    ylab(bquote('Covariance of Heat Index '~('°C'^2))) +
    ylim(0,sill) + 
    xlim(0,10) + 
    guides(colour="none") +
    annotate("text", x=15/2, y=sill,
             label= paste0(year,' SSE = ',
                           round(optimal.gau.sse,2))) +
    theme_bw()
  
  gau.plot.space <- ggplot() +
    geom_line(data = gau.model.space,aes(x = lag, y = covariance)) + 
    geom_point(data = space,aes(x = lag, y = cov.experimental)) + 
    xlab('Spatial Lag (quarter degree)') +
    ylab(bquote('Covariance of Heat Index '~('°C'^2))) +
    ylim(0,sill) + 
    xlim(0,12) + 
    guides(colour="none") +
    theme_bw()
  
  gau.plot <- ggpubr::ggarrange(gau.plot.space,gau.plot.time,nrow=1,ncol=2)
  gau.plot <- ggpubr::annotate_figure(gau.plot,
                                      top = text_grob(substitute(
                                        C[x] * (r~","~τ) == cov.sill %*% exp(frac(-3 * r^2, ar1)) %*% exp(frac(-3 * τ^2, at1)), 
                                        list(cov.sill = round(sill,1),
                                             ar1 = round(3*optimal.gau.params[2]^2,0),
                                             at1 = round(3*optimal.gau.params[1]^2,0)))))
  
  # update stm dataframe
  stm.gau[stm.index,2] <- sill
  stm.gau[stm.index,3] <- optimal.gau.params[1]
  stm.gau[stm.index,4] <- optimal.gau.params[2]
  stm.gau[stm.index,5] <- optimal.gau.sse
  stm.gau[stm.index,6] <- optimal.gau.params[2] / optimal.gau.params[1]
  
  ## gau exp ----
  
  # Setup list to fill with all results
  gau.exp.sse <- list()
  gau.exp.params <- list()
  index <- 1
  
  # Define alpha values, determined by professional judgement
  for (i in rev(seq(.5,1,0.01))) {
    
    # Define alpha values
    alpha1 <- i
    alpha2 <- 1 - alpha1
    
    # Minimize the SSE
    result <- optim(
      par = initial_guess_4,          # Initial parameter guesses
      fn = sse_function_gau_exp,    # Function to minimize
      method = "L-BFGS-B",          # Optimization method with bounds
      lower = lower.bounds,         # Lower bounds for parameters: make physically plausible
      upper = upper.bounds          # Upper bounds for parameters: make physically plausible
    )
    
    # Save result to list
    gau.exp.sse[[index]] <- result$value
    gau.exp.params[[index]] <- result$par
    
    # Update list index
    index <- index + 1
    
  }
  
  # Determine optimal values
  optimal.gau.exp.sse <- unlist(gau.exp.sse)
  optimal.gau.exp.pos <- which.min(optimal.gau.exp.sse)
  optimal.gau.exp.params <- gau.exp.params[[optimal.gau.exp.pos]]
  optimal.gau.exp.alpha1 <- rev(seq(.5,1,0.01))[[optimal.gau.exp.pos]]
  optimal.gau.exp.alpha2 <- 1 - optimal.gau.exp.alpha1
  
  # Update original df's with modeled values
  gau.exp.model.time <- data.frame(lag = seq(min(time$lag),max(time$lag),0.1))
  gau.exp.model.time$covariance <- sill * (optimal.gau.exp.alpha1 * exp((-3 * (gau.exp.model.time$lag)^2) /
                                                                          (3 * (optimal.gau.exp.params[1])^2)) +
                                             optimal.gau.exp.alpha2 * exp((-3 * gau.exp.model.time$lag) /
                                                                            optimal.gau.exp.params[3]))
  
  gau.exp.model.space <- data.frame(lag = seq(min(space$lag),max(space$lag),0.1))
  gau.exp.model.space$covariance <- sill * (optimal.gau.exp.alpha1 * exp((-3 * (gau.exp.model.space$lag)^2) /
                                                                           (3 * (optimal.gau.exp.params[2])^2)) +
                                              optimal.gau.exp.alpha2 * exp((-3 * gau.exp.model.space$lag) /
                                                                             optimal.gau.exp.params[4]))
  
  
  # plot
  gau.exp.plot.time <- ggplot() +
    geom_line(data = gau.exp.model.time,aes(x = lag, y = covariance)) + 
    geom_point(data = time,aes(x = lag, y = cov.experimental)) + 
    xlab('Temporal Lag (days)') +
    ylab(bquote('Covariance of Heat Index '~('°C'^2))) +
    ylim(0,sill) + 
    xlim(0,10) + 
    guides(colour="none") +
    annotate("text", x=15/2, y=sill,
             label= paste0(year,' SSE = ',
                           round(min(optimal.gau.exp.sse),2))) +
    theme_bw()
  
  gau.exp.plot.space <- ggplot() +
    geom_line(data = gau.exp.model.space,aes(x = lag, y = covariance)) + 
    geom_point(data = space,aes(x = lag, y = cov.experimental)) + 
    xlab('Spatial Lag (quarter degree)') +
    ylab(bquote('Covariance of Heat Index '~('°C'^2))) +
    ylim(0,sill) + 
    xlim(0,12) + 
    guides(colour="none") +
    theme_bw()
  
  gau.exp.plot <- ggpubr::ggarrange(gau.exp.plot.space,gau.exp.plot.time,nrow=1,ncol=2)
  gau.exp.plot <- ggpubr::annotate_figure(gau.exp.plot,top = text_grob(substitute(
    C[x] * (r~","~τ) == cov.sill %*% (alpha1 %*% 
                                        exp(frac(-3 * r^2,ar1)) %*% 
                                        exp(frac(-3 * τ^2,at1)) + 
                                        alpha2 %*% 
                                        exp(frac(-3 * r, ar2)) %*% 
                                        exp(frac(-3 * τ, at2))), 
    list(cov.sill = round(sill,1),
         alpha1 = optimal.gau.exp.alpha1,
         alpha2 = optimal.gau.exp.alpha2,
         ar1 = round(3*optimal.gau.exp.params[2]^2,0),
         at1 = round(3*optimal.gau.exp.params[1]^2,0),
         ar2 = round(optimal.gau.exp.params[4],0),
         at2 = round(optimal.gau.exp.params[3],0)))))
  
  # update stm dataframe
  stm.gau.exp[stm.index,2] <- sill
  stm.gau.exp[stm.index,3] <- optimal.gau.exp.alpha1
  stm.gau.exp[stm.index,4] <- optimal.gau.exp.alpha2
  stm.gau.exp[stm.index,5] <- optimal.gau.exp.params[1] # at1
  stm.gau.exp[stm.index,6] <- optimal.gau.exp.params[2] # ar1
  stm.gau.exp[stm.index,7] <- optimal.gau.exp.params[3] # at2
  stm.gau.exp[stm.index,8] <- optimal.gau.exp.params[4] # ar2
  stm.gau.exp[stm.index,9] <- min(optimal.gau.exp.sse)
  stm.gau.exp[stm.index,10] <- optimal.gau.exp.alpha1*optimal.gau.exp.params[2]+optimal.gau.exp.alpha2*optimal.gau.exp.params[1]
  stm.gau.exp[stm.index,11] <- optimal.gau.exp.alpha1*optimal.gau.exp.params[4]+optimal.gau.exp.alpha2*optimal.gau.exp.params[3]
  stm.gau.exp[stm.index,12] <- (optimal.gau.exp.alpha1*optimal.gau.exp.params[2]+optimal.gau.exp.alpha2*optimal.gau.exp.params[1]) /
    (optimal.gau.exp.alpha2*optimal.gau.exp.params[4]+optimal.gau.exp.alpha2*optimal.gau.exp.params[3])
  stm.gau.exp[stm.index,13] <- optimal.gau.exp.alpha1*(optimal.gau.exp.params[2]/optimal.gau.exp.params[1])+
    optimal.gau.exp.alpha2*(optimal.gau.exp.params[4]/optimal.gau.exp.params[3])
  
  # Identify optimal model
  model.sse <- c(min(optimal.exp.exp.sse),
                 optimal.exp.sse,
                 optimal.gau.sse,
                 min(optimal.gau.exp.sse))
  
  # # update sse dataframe
  # sse[stm.index,2] <- model.sse[2]
  # sse[stm.index,3] <- model.sse[1]
  # sse[stm.index,4] <- model.sse[3]
  # sse[stm.index,5] <- model.sse[4]
  
  # determine optimal model
  if (model.sse[1] == min(model.sse)){
    
    # print statement
    print('Nested Exponential minimizes SSE')
    
    # update stm dataframe
    stm[stm.index,2] <- sill
    stm[stm.index,3] <- optimal.exp.exp.alpha1
    stm[stm.index,4] <- optimal.exp.exp.alpha2
    stm[stm.index,5] <- optimal.exp.exp.params[1]
    stm[stm.index,6] <- optimal.exp.exp.params[2]
    stm[stm.index,7] <- optimal.exp.exp.params[3]
    stm[stm.index,8] <- optimal.exp.exp.params[4]
    stm[stm.index,9] <- min(optimal.exp.exp.sse)
    stm[stm.index,10] <- optimal.exp.exp.alpha1*optimal.exp.exp.params[2]+optimal.exp.exp.alpha2*optimal.exp.exp.params[1]
    stm[stm.index,11] <- optimal.exp.exp.alpha1*optimal.exp.exp.params[4]+optimal.exp.exp.alpha2*optimal.exp.exp.params[3]
    stm[stm.index,12] <- (optimal.exp.exp.alpha1*optimal.exp.exp.params[2]+optimal.exp.exp.alpha2*optimal.exp.exp.params[1]) /
      (optimal.exp.exp.alpha2*optimal.exp.exp.params[4]+optimal.exp.exp.alpha2*optimal.exp.exp.params[3])
    stm[stm.index,13] <- optimal.exp.exp.alpha1*(optimal.exp.exp.params[2]/optimal.exp.exp.params[1])+
      optimal.exp.exp.alpha2*(optimal.exp.exp.params[4]/optimal.exp.exp.params[3])
    stm[stm.index,14] <- 'Nested Exponential'
    
    # update figure list
    fitted.cov.plots[[stm.index]] <- exp.exp.plot
    
  } else if (model.sse[2] == min(model.sse)){
    
    # print statement
    print('Single Exponential minimizes SSE')
    
    # update stm dataframe
    stm[stm.index,2] <- sill
    stm[stm.index,3] <- NA
    stm[stm.index,4] <- NA
    stm[stm.index,5] <- optimal.exp.params[1]
    stm[stm.index,6] <- optimal.exp.params[2]
    stm[stm.index,7] <- NA
    stm[stm.index,8] <- NA
    stm[stm.index,9] <- min(optimal.exp.sse)
    stm[stm.index,10] <- NA
    stm[stm.index,11] <- NA
    stm[stm.index,12] <- optimal.exp.params[2]/optimal.exp.params[1]
    stm[stm.index,13] <- optimal.exp.params[2]/optimal.exp.params[1]
    stm[stm.index,14] <- 'Single Exponential'
    
    # update figure list
    fitted.cov.plots[[stm.index]] <- exp.plot
    
  } else if (model.sse[3] == min(model.sse)){
    
    # print statement
    print('Single Gaussian minimizes SSE')
    
    # update stm dataframe
    stm[stm.index,2] <- sill
    stm[stm.index,3] <- NA
    stm[stm.index,4] <- NA
    stm[stm.index,5] <- optimal.gau.params[1]
    stm[stm.index,6] <- optimal.gau.params[2]
    stm[stm.index,7] <- NA
    stm[stm.index,8] <- NA
    stm[stm.index,9] <- min(optimal.gau.sse)
    stm[stm.index,10] <- NA
    stm[stm.index,11] <- NA
    stm[stm.index,12] <- optimal.gau.params[2]/optimal.gau.params[1]
    stm[stm.index,13] <- optimal.gau.params[2]/optimal.gau.params[1]
    stm[stm.index,14] <- 'Single Gaussian'
    
    # update figure list
    fitted.cov.plots[[stm.index]] <- gau.plot
    
  } else {
    
    # print statement
    print('Nested Gaussian Exponential minimizes SSE')
    
    # update stm dataframe
    stm[stm.index,2] <- sill
    stm[stm.index,3] <- optimal.gau.exp.alpha1
    stm[stm.index,4] <- optimal.gau.exp.alpha2
    stm[stm.index,5] <- optimal.gau.exp.params[1]
    stm[stm.index,6] <- optimal.gau.exp.params[2]
    stm[stm.index,7] <- optimal.gau.exp.params[3]
    stm[stm.index,8] <- optimal.gau.exp.params[4]
    stm[stm.index,9] <- min(optimal.gau.exp.sse)
    stm[stm.index,10] <- optimal.gau.exp.alpha1*optimal.gau.exp.params[2]+optimal.gau.exp.alpha2*optimal.gau.exp.params[1]
    stm[stm.index,11] <- optimal.gau.exp.alpha1*optimal.gau.exp.params[4]+optimal.gau.exp.alpha2*optimal.gau.exp.params[3]
    stm[stm.index,12] <- (optimal.gau.exp.alpha1*optimal.gau.exp.params[2]+optimal.gau.exp.alpha2*optimal.gau.exp.params[1]) /
      (optimal.gau.exp.alpha2*optimal.gau.exp.params[4]+optimal.gau.exp.alpha2*optimal.gau.exp.params[3])
    stm[stm.index,13] <- optimal.gau.exp.alpha1*(optimal.gau.exp.params[2]/optimal.gau.exp.params[1])+
      optimal.gau.exp.alpha2*(optimal.gau.exp.params[4]/optimal.gau.exp.params[3])
    stm[stm.index,14] <- 'Nested Gaussian Exponential'
    
    # update figure list
    fitted.cov.plots[[stm.index]] <- gau.exp.plot
    
  }
  
  # update index
  stm.index <- stm.index + 1
  
}

### Results ----

# subset to complete cases
stm <- stm[complete.cases(stm),]
stm.exp <- stm.exp[complete.cases(stm.exp),]
stm.gau <- stm.gau[complete.cases(stm.gau),]
stm.exp.exp <- stm.exp.exp[complete.cases(stm.exp.exp),]
stm.gau.exp <- stm.gau.exp[complete.cases(stm.gau.exp),]

# save STM
write.csv(stm, here("data", "output", "02_covariance", "03_space_time_metric", "heat_index", "month", "heat_index_space_time_metric_optimal.txt"))
write.csv(stm.exp,here("data", "output", "02_covariance", "03_space_time_metric", "heat_index", "month", "heat_index_space_time_metric_single_exponential.txt"))
write.csv(stm.gau,here("data", "output", "02_covariance", "03_space_time_metric", "heat_index", "month", "heat_index_space_time_metric_single_gaussian.txt"))
write.csv(stm.exp.exp,here("data", "output", "02_covariance", "03_space_time_metric", "heat_index", "month", "heat_index_space_time_metric_nested_exponential.txt"))
write.csv(stm.gau.exp,here("data", "output", "02_covariance", "03_space_time_metric", "heat_index", "month", "heat_index_space_time_metric_nested_gaussian_exponential.txt"))

# # save figures
# for (fig in 1:length(fitted.cov.plots)){
#   
#   print(fig)
#   
#   save.figure.png(figure = fitted.cov.plots[[fig]],
#                   figure.number = '3',
#                   figure.name = paste0('Heat_Index_Covariance_Model_',seq(1940,2023,1)[fig]),
#                   pixels.x = 6000,pixels.y = 3000,resolution = 600)
#   
#   save.figure.svg(figure = fitted.cov.plots[[fig]],
#                   figure.number = '3',
#                   figure.name = paste0('Heat_Index_Covariance_Model_',seq(1940,2023,1)[fig]),
#                   pixels.x = 6,pixels.y = 3)
#   
# }

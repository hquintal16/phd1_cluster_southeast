#Setup----
#Updated April 2025
#Linked to GitHub
#Hunter Quintal
#purpose: plot Space Time Separable Covariance Models
#study area: Southeast

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_02_covariance_models_single.R") 
source(here::here("scripts", "01_library.R"))

## Heat Index ----
year_month <- yearmo(start.year = 1940,end.year = 2023)

# Create data frames
stm <- data.frame(year_mo = year_month, sill = NA,
                  alpha1 = NA, alpha2 = NA,
                  at1 = NA, ar1 = NA, 
                  at2 = NA, ar2 = NA, 
                  sse = NA,
                  stm1.numerator = NA, stm1.denominator = NA,
                  stm1 = NA, stm2 = NA, model = NA)

stm.gau.exp <- data.frame(year_mo = year_month, sill = NA,
                          alpha1 = NA, alpha2 = NA,
                          at1 = NA, ar1 = NA, 
                          at2 = NA, ar2 = NA, 
                          sse = NA,
                          stm1.numerator = NA, stm1.denominator = NA,
                          stm1 = NA, stm2 = NA)

# create figure list
fitted.cov.plots.heat.index <- list()

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
  space <- read.csv(paste0('V:/users/hquintal/phd1_cluster_southeast/data/output/02_covariance/02_experimental_covariance/heat_index/month/heat_index_experimental_covariance_space_',year,'.txt'))
  time <- read.csv(paste0('V:/users/hquintal/phd1_cluster_southeast/data/output/02_covariance/02_experimental_covariance/heat_index/month/heat_index_experimental_covariance_time_',year,'.txt'))
  
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
  
  ### gau exp 
  
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
    annotate("text", x=5, y=0,
             label= paste0(year,' SSE = ',
                           round(min(optimal.gau.exp.sse),2))) +
    theme_bw()
  
  gau.exp.plot.space <- ggplot() +
    geom_line(data = gau.exp.model.space,aes(x = lag, y = covariance)) + 
    geom_point(data = space,aes(x = lag, y = cov.experimental)) + 
    xlab('Spatial Range (0.25°)') +
    ylab(bquote('Covariance of Heat Index '~('°C'^2))) +
    ylim(0,sill) + 
    xlim(0,12) + 
    guides(colour="none") +
    theme_bw()
  
  gau.exp.plot <- ggpubr::ggarrange(gau.exp.plot.space,gau.exp.plot.time,nrow=1,ncol=2)
  gau.exp.plot <- ggpubr::annotate_figure(gau.exp.plot,                                                                  top = text_grob(substitute(
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
  
  
    fitted.cov.plots.heat.index[[stm.index]] <- gau.exp.plot
  
  # update index
  stm.index <- stm.index + 1
  
}
# fitted.cov.plots.heat.index[[1]]
## Precipitation ----

# Create data frames
stm <- data.frame(year_mo = year_month, sill = NA,
                  alpha1 = NA, alpha2 = NA,
                  at1 = NA, ar1 = NA, 
                  at2 = NA, ar2 = NA, 
                  sse = NA,
                  stm1.numerator = NA, stm1.denominator = NA,
                  stm1 = NA, stm2 = NA, model = NA)

stm.gau.exp <- data.frame(year_mo = year_month, sill = NA,
                          alpha1 = NA, alpha2 = NA,
                          at1 = NA, ar1 = NA, 
                          at2 = NA, ar2 = NA, 
                          sse = NA,
                          stm1.numerator = NA, stm1.denominator = NA,
                          stm1 = NA, stm2 = NA)

# create figure list
fitted.cov.plots.precipitation <- list()

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
  space <- read.csv(paste0('V:/users/hquintal/phd1_cluster_southeast/data/output/02_covariance/02_experimental_covariance/precipitation/month/precipitation_experimental_covariance_space_',year,'.txt'))
  time <- read.csv(paste0('V:/users/hquintal/phd1_cluster_southeast/data/output/02_covariance/02_experimental_covariance/precipitation/month/precipitation_experimental_covariance_time_',year,'.txt'))
  
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
  
  ### gau exp
  
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
    xlab('Temporal Lag (hours)') +
    ylab(bquote('Covariance of Precipitation '~('mm'^2))) +
    ylim(0,sill) + 
    xlim(0,10) + 
    guides(colour="none") +
    annotate("text", x=5, y=0,
             label= paste0(year,' SSE = ',
                           round(min(optimal.gau.exp.sse),2))) +
    theme_bw()
  
  gau.exp.plot.space <- ggplot() +
    geom_line(data = gau.exp.model.space,aes(x = lag, y = covariance)) + 
    geom_point(data = space,aes(x = lag, y = cov.experimental)) + 
    xlab('Spatial Range (0.25°)') +
    ylab(bquote('Covariance of Precipitation '~('mm'^2))) +
    ylim(0,sill) + 
    xlim(0,12) + 
    guides(colour="none") +
    theme_bw()
  
  gau.exp.plot <- ggpubr::ggarrange(gau.exp.plot.space,gau.exp.plot.time,nrow=1,ncol=2)
  gau.exp.plot <- ggpubr::annotate_figure(gau.exp.plot,                                                                  top = text_grob(substitute(
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
  
  
  fitted.cov.plots.precipitation[[stm.index]] <- gau.exp.plot
  
  # update index
  stm.index <- stm.index + 1
  
}

# Save ----
for (i in seq_along(year_month)) { 
  # Combine the heat index and precipitation plots vertically
  combined_plot <- fitted.cov.plots.heat.index[[i]] / fitted.cov.plots.precipitation[[i]] +
    plot_annotation(tag_levels = "a")  # this will label panels as "a", "b", etc.
  # 
  # print(combined_plot)
  
  # Dynamically create file names using the current year_month value
  current_ym <- year_month[i]
  print(current_ym)
  png_path <- here("figures", paste0("02_covariance_model_", current_ym, ".png"))
  svg_path <- here("figures", paste0("02_covariance_model_", current_ym, ".svg"))
  
  # Save the combined plot as PNG and SVG
  ggsave(filename = png_path, plot = combined_plot, width = 6, height = 7, dpi = 300)
  ggsave(filename = svg_path, plot = combined_plot, width = 6, height = 7, device = "svg")
}

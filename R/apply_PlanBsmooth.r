#' Applies the Plan B smooth approach.
#' 
#' Smooths data (Year and Biomass Index) using loess, applies log linear regression to most recent three years, and retransforms back to estimate multiplier for catch advice.
#' @param dat data frame of Year and avg (the biomass index)
#' @param od output directory where plots are saved (default=working directory)
#' @param my.title title for time series plot with loess smooth (default = "")
#' @param terminal.year last year used in smooth (allows easy retro analysis) (default = NA = most recent)
#' @param nyears number of years to use in loess (default = 33)
#' @param loess.span proportion time series used in smoothing (default = NA, calculates span=9.9/nyears)
#' @param saveplots true/false flag to save output to od (default=FALSE)
#' @export

ApplyPlanBsmooth <- function(dat,
                             od            = ".\\",
                             my.title      = "",
                             terminal.year = NA,
                             nyears        = 33,
                             loess.span    = NA,
                             saveplots     = FALSE){
  
  # select data to use
  if(is.na(terminal.year)) terminal.year <- max(dat$Year, na.rm=T)
  dat.use <- filter(dat, Year <= terminal.year, Year >= (terminal.year - nyears + 1)) %>%
    drop_na()  # removes years with missing index values
  nyears <- max(dat.use$Year) - min(dat.use$Year) + 1 # in case fewer years than (e.g., during retro)
  
  # apply loess 
  if(is.na(loess.span)) loess.span <- 9.9 / nyears
  lfit <- loess(data=dat.use, avg ~ Year, span=loess.span)
  pred_fit <- predict(lfit, se=TRUE)
  
  # get last three predicted values
  reg.dat <- data.frame(Year = dat.use$Year[(nyears-2):nyears],
                        pred = pred_fit$fit[(nyears-2):nyears])
  
  # log linear regression of last three loess predicted values
  llr_fit <- lm(log(pred) ~ Year, data=reg.dat)
  llr_fit.df <- data.frame(Year = reg.dat$Year,
                          llfit = exp(predict(llr_fit)))
  
  # convert back to regular scale
  multiplier <- exp(llr_fit$coefficients[2])
  
  # make plot
  windows(record=T)
  ribbon <- data.frame(Year = dat.use$Year,
                       avg  = dat.use$avg,
                       pred = pred_fit$fit,
                       loci = pred_fit$fit - 1.96 * pred_fit$se.fit,
                       hici = pred_fit$fit + 1.96 * pred_fit$se.fit)

  if(saveplots) write.csv(ribbon, paste0(od,"PlanBsmooth_table.csv"), row.names = FALSE)
    
  tsplot <- ggplot(ribbon, aes(x=Year, y=avg)) +
    geom_point() +
    geom_ribbon(aes(x=Year, ymin=loci, ymax=hici), fill="grey50", alpha=0.3) +
    geom_line(aes(x=Year, y=pred), color="blue", size=1.3) +
    geom_line(data=llr_fit.df, aes(x=Year, y=llfit), color="red", size=1.3, linetype="dashed") +
    scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
    ylab("Biomass Index") +
    labs(title = my.title, subtitle = paste0("Multiplier =", round(multiplier,3))) +
    theme_bw()
  
  print(tsplot)
  if(saveplots) savePlot(paste0(od,"time_series_with_loess_smooth.png"), type='png')

  # list of results
  res <- list()
  res$dat.use    <- dat.use
  res$lfit       <- lfit
  res$pred_fit   <- pred_fit
  res$reg.dat    <- reg.dat
  res$llr_fit    <- llr_fit
  res$multiplier <- multiplier
  
  return(res)

}

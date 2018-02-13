# source("libraries.R")

add.loess <- function(df, span.set) {
  # appends the loess smoothed values to the df
  
  wt.lo <- df %>%
    loess(wt ~ date.nm, data = ., span = span.set)
  
  df %>%
    add_predictions(wt.lo) %>%
    add_residuals(wt.lo)
  
}


resid.boxplot <- function(df, span.set) {
  # side-effect boxplot of the residuals
  
  df <- add.loess(df, span.set)
  
  df %>%
    group_by(period, wk.day) %>%
    mutate(resid.mean = mean(resid, na.rm = TRUE)) %>%
    ggplot() +
    geom_boxplot(aes(x = wk.day, y = resid, color = wk.day)) +
    geom_hline(yintercept = 0, colour = "red") +
    geom_point(aes(x = wk.day, y = resid.mean, color = wk.day)) +
    facet_grid(. ~ period) +
    coord_cartesian(ylim = c(-2, 2)) +
    labs(title = "Spread of the residuals",
         subtitle = paste0("Span = ", span.set),
         x = "",
         y = "Difference from local smoothed line")
  
}

acf.plot <- function(df, span.set) {
  # side-effect ACF plot of the residuals
  
  df <- add.loess(df, span.set)
  
  df %>%
    filter(date <= ymd("2017/12/24")) %>%
    arrange(date) %>%
    select(resid) %>%
    as.ts() %>%
    Acf(main = "",
        xlab = "",
        ylab = "", 
        cex.lab = 0.8,
        cex.axis = 0.8)
  mtext(side=1, text="Lag between pairs of days", line=1.8)
  mtext(side=2, text="Correlation factor of weights between pairs of days", line=1.8)
  mtext(side=3, text= paste0("Autocorrelation of the residuals: Span = ", span.set), line=0.5)
  
}

scatter.plot <- function(df, span.set, lag, scatt.col, span.best) {
  # Scatter plot with selected colour scheme
  
  # Determine which column to use for the colour scale
  col.col <- case_when(scatt.col == "Month" ~ "Mon",
                       scatt.col == "Weekday" ~ "wk.day",
                       TRUE ~ "period")
  
  # Is this the base span for smoothing?
  resid.cols <- if (span.set == span.best) {
    c("resid.best", "resid.best.pl")
  } else {
    c("resid", "resid.pl") 
  }
                    
  
  # Set up colour column and columns to plot
  df <- df %>%
    filter(date <= ymd("2017/12/24")) %>%
    mutate(Mon = month(date, label = TRUE),
           col.col = !!as.name(col.col),
           resid.1 = !!as.name(resid.cols[1]),
           resid.2 = !!as.name(resid.cols[2])) %>%
    na.omit()
  
  # Calculate correlation factors
  wt.lag.cor <- cor(df$resid.1, df$resid.2)
  
  # Identify maximum residuals for annotation
  resid.max <- df %>%
    pull(resid.2) %>%
    which.max()
  
  # Scatter plot
  df %>%
    ggplot(aes(resid.1, resid.2)) +
    geom_point(aes(colour = col.col), na.rm = TRUE) +
    geom_vline(xintercept = 0, colour = "grey") +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_smooth(method = "lm", na.rm = TRUE) +
    labs(title = paste0("Smoothing span = ", as.character(span.set), " with lag ", lag,
                        ". R = ", format(wt.lag.cor, digits = 2)),
         x = "residual",
         y = paste0("residual with lag ", lag)) +
    theme(legend.title=element_blank()) +
    ggrepel::geom_label_repel(aes(label = paste0("eg: ", format(date, "%d/%m"), " vs ",
                                                 format(date + lag, "%d/%m"))), 
                              data = slice(df, resid.max),
                              alpha = 0.5,
                              size = 3) +
    coord_fixed()
  
}

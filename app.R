
# Load libraries ----------------------------------------------------------

source("libraries.R")

data <- "./data"
span.best <- 0.2

source("helper.R")

# Load weight data ------------------------------------------------------

wkdays <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

wts.clean <- read_csv(paste0(data, "/", "James weight.csv"),
                    col_types = cols(
                      WcComm = col_date(format = "%d-%b-%y"),
                      Sun = col_double(),
                      Mon = col_double(),
                      Tue = col_double(),
                      Wed = col_double(),
                      Thu = col_double(),
                      Fri = col_double(),
                      Sat = col_double(),
                      Comment = col_character(),
                      WkMin = col_double(),
                      WkMax = col_double()
                    )) %>%
  gather(Sun:Sat, key = 'wk.day', value = wt, convert = TRUE,
         na.rm = TRUE) %>%
  mutate(wk.day = factor(wk.day, levels = wkdays),
         wk.day.val = as.numeric(wk.day),
         date = WcComm + wk.day.val - 1,
         date.nm = as.numeric(date),
         period = factor(case_when(
           date < dmy("1/8/2017") ~ "Pre-Aug",
           date < dmy("25/12/2017") ~ "Aug-Dec",
           TRUE ~ "Post Xmas"), levels = c("Pre-Aug", "Aug-Dec", "Post Xmas"))) %>%
  arrange(date) %>%
  select(date, wt, wk.day, date.nm, period)

wt.lo <- wts.clean %>%
  loess(wt ~ date.nm, data = ., span = span.best)

wts.pred <- wts.clean %>%
  add_predictions(wt.lo) %>%
  add_residuals(wt.lo) %>%
  mutate(pred.best = pred,
         resid.best = resid)


# Define UI for application that draws a histogram -----
ui <- fluidPage(
   
   # Application title
   titlePanel("Weighing up the data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("span",
                     "Span for Smooth Curve:",
                     min = 0.05,
                     max = 1,
                     value = .4),
         width = 2
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("wt.ts"),
         
         plotOutput("dow.resid.best"),
           
         splitLayout(
           plotOutput("acf.best"),
           plotOutput("acf.sel")
         ),
         
         splitLayout(
           sliderInput("lag",
                       "Select lag for scatter plot:",
                       min = 1, max = 15, value = 1),
           
           radioButtons("scatt.col",
                        label = "Select colour reference in scatter plot",
                        choices = c("Month" = "Month",
                                    "Day of Week" = "Weekday",
                                    "Period" = "Period"))
         ),
         
         splitLayout(
           plotOutput("resid.scatt.best"),
           plotOutput("resid.scatt.sel")
         ),

         width = 10
         
      )
   )
)

# Define server logic required to draw a histogram -------
server <- function(input, output) {
   
   output$wt.ts <- renderPlot({
     # Simple time-series of weight with smoothing factor
     
     wts.clean %>%
       ggplot(aes(x = date, y = wt)) +
       geom_point(size = 0.5) +
       geom_smooth(span = span.best, method = "loess", aes(colour = "blue")) +
       geom_smooth(span = input$span, method = "loess", aes(colour = "red"), se = FALSE) +
       scale_colour_manual("Smoothing span",
                           values = c(blue = "blue", red = "red"),
                           labels = c(as.character(span.best), as.character(input$span))) +
       labs(title = "Daily weight with line of best fit",
            subtitle = "Varying span for the smoothing function",
            x = "",
            y = "Weight (kg)") +
       scale_x_date(date_breaks = "1 month",
                    date_labels = "%b-%y") +
       scale_y_continuous(breaks = c(84:96)) +
       coord_cartesian(ylim = c(84, 96))
   })
   
   new.span.preds <- reactive({
     # Add residuals from input span
     add.loess(wts.pred, input$span)
     
   })
   
   output$dow.resid.best <- renderPlot({
     # Residuals box plot by day of week
     
     # Add residuals from input span
     # wts.pred <- add.loess(wts.pred, input$span)
     
     new.span.preds() %>%
       select(date.nm, period, wk.day, resid, resid.best) %>%
       gather(key = "Span", value = "resid", resid, resid.best) %>%
       mutate(Span = if_else(Span == "resid", paste0("Span=", input$span),
                             paste0("Span=", span.best))) %>%
       group_by(period, wk.day, Span) %>%
       mutate(resid.mean = mean(resid, na.rm = TRUE)) %>%
       ggplot() +
       geom_boxplot(aes(x = wk.day, y = resid, color = wk.day)) +
       geom_hline(yintercept = 0, colour = "black", alpha = 0.5) +
       geom_point(aes(x = wk.day, y = resid.mean, color = wk.day)) +
       facet_grid(Span ~ period) +
       coord_cartesian(ylim = c(-2, 2)) +
       labs(title = "Spread of the residuals",
            # subtitle = paste0("Span = ", span.set),
            x = "",
            y = "Difference from local smoothed line")
     
   })
   
   
   output$acf.best <- renderPlot({
     # Residuals box plot by day of week
     
     acf.plot(wts.pred, span.best)
     
   })
   
   output$acf.sel <- renderPlot({
     # Residuals box plot by day of week
     
     acf.plot(wts.pred, input$span)
     
   })
   
   wt.lag.list <- reactive({
     # Add residuals from input span

     new.span.preds() %>%
       filter(date <= ymd("2017/12/24")) %>%
       arrange(date) %>%
       mutate(resid.pl = lead(resid, input$lag),
              resid.best.pl = lead(resid.best, input$lag)) %>%
       na.omit()
     
   })
   
   
   output$resid.scatt.best <- renderPlot({
     # Scatter plot of the residuals with chosen lag
     
     scatter.plot(df = wt.lag.list(), span.set = span.best, 
                  lag = input$lag, scatt.col = input$scatt.col, span.best)

   })
   
   output$resid.scatt.sel <- renderPlot({
     # Scatter plot of the residuals with chosen lag
     
     scatter.plot(df = wt.lag.list(), span.set = input$span, 
                  lag = input$lag, scatt.col = input$scatt.col, span.best)
     
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)


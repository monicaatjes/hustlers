#################################### function to create funnel plot with ING format #######################

funnel_plot_format_function <- function(x) {
  
    # plotly object
    p1 <- 
      plot_ly() %>%
      
      add_trace(type = "funnel",
                name= "Mobile",
                y = c("PP", "SF", "FF", "AO"),
                x = c(
                  #round(x$`Category page`[x$Device=="Mobile"], digits=2),
                  round(x$`Product page`[x$Device=="Mobile"], digits=2), 
                  round(x$`Start application`[x$Device=="Mobile"], digits=2), 
                  round(x$`Finish application`[x$Device=="Mobile"], digits=2),
                  round(x$`Account opening`[x$Device=="Mobile"], digits=2)),
                textposition = "inside",
                textinfo = "value",
                opacity = 1.0,
                marker = list(color = c("rgb(82, 81, 153)", "rgb(82, 81, 153)", "rgb(82, 81, 153)",
                                        "rgb(82, 81, 153)", "rgb(82, 81, 153)")),
                textfont = list(family = "ING me", size = 14, color = "white"),
                connector = list(fillcolor = c("white", "white", "white", "white", "white"))) %>%
      
      add_trace(
        type = "funnel",
        name = 'Desktop',
        y = c("PP", "SF", "FF", "AO"),
        x = c(
          #round(x$`Category page`[x$Device=="Desktop"], digits=2), 
          round(x$`Product page`[x$Device=="Desktop"], digits=2), 
          round(x$`Start application`[x$Device=="Desktop"], digits=2), 
          round(x$`Finish application`[x$Device=="Desktop"], digits=2),
          round(x$`Account opening`[x$Device=="Mobile"], digits=2)),
        textposition = "inside",
        textinfo = "value",
        opacity = 1.0,
        marker = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)",
                                "rgb(255,098,000)", "rgb(82, 81, 153)")),
        textfont = list(family = "ING me", size = 14, color = "white"),
        connector = list(fillcolor = c("white", "white", "white", "white", "white"))) %>%
      
      layout(yaxis = list(categoryarray = c("PP", "SF", "FF", "AO"),
                          tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             xaxis = list(tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
    
    
    # output <-list(p1,p2) # return the pair of plots
    print(p1)
  
}
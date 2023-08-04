#################################### function to create multiple funnel plots #######################

funnel_plot_groups_function <- function(x) {
  
  # a vector of names of regions to loop over
  group <- unique(c(x$NewvsCurrent)) 
  
  # a loop to produce graphs 
  for (i in seq_along(group)) {
    
    # plotly object
    p1 <- 
      plot_ly() %>%
      
      add_trace(type = "funnel",
                name= "Mobile",
                y = c("Category page", "Product page", "Start application", "Finish application"),
                x = c(
                  #round(x$`Category page`[x$Device=="Mobile"], digits=2),
                  round(x$`Product page`[x$Device=="Mobile"], digits=2), 
                  round(x$`Start application`[x$Device=="Mobile"], digits=2), 
                  round(x$`Finish application`[x$Device=="Mobile"], digits=2)),
                textposition = "inside",
                textinfo = "value",
                opacity = 1.0,
                marker = list(color = c("rgb(82, 81, 153)", "rgb(82, 81, 153)", "rgb(82, 81, 153)",
                                        "rgb(82, 81, 153)")),
                textfont = list(family = "ING me", size = 14, color = "white"),
                connector = list(fillcolor = c("white", "white", "white", "white"))) %>%
      add_trace(
        type = "funnel",
        name = 'Desktop',
        y = c("Product page", "Start application", "Finish application"),
        x = c(
          #round(x$`Category page`[x$Device=="Desktop"], digits=2), 
          round(x$`Product page`[x$Device=="Desktop"], digits=2), 
          round(x$`Start application`[x$Device=="Desktop"], digits=2), 
          round(x$`Finish application`[x$Device=="Desktop"], digits=2)),
        textposition = "inside",
        textinfo = "value",
        opacity = 1.0,
        marker = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)",
                                "rgb(255,098,000)")),
        textfont = list(family = "ING me", size = 14, color = "white"),
        connector = list(fillcolor = c("white", "white", "white", "white"))) %>%
      
      layout(yaxis = list(categoryarray = c("Product page", "Start application", "Finish application"),
                          tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             xaxis = list(tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
    
    
    # output <-list(p1,p2) # return the pair of plots
    print(p1)
  }
}
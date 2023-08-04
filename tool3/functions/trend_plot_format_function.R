#################################### function to create time plot with ING format #######################

trend_plot_format_function <- function(x)  {
  
  # plotly object

trend_plot <- 
  plot_ly(x = as.character(unique(x$quarter))) %>%
  add_trace(y = ~x$`Product page`[x$Device=="Desktop"], 
            type='scatter', 
            mode ='lines',
            name="visits_desktop",
            text = x$`Product page`[x$Device=="Desktop"],
            hovertemplate = paste('%{x}', '<br>x$`Product page`: %{text}<br>'),
            texttemplate = '%{y}', textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
            marker = list(color = 'rgb(255,098,000)'),
            line = list(color = 'rgb(255,098,000)'))  %>%
  add_trace(y =~x$`Product page`[x$Device=="Mobile"], 
            type='scatter', 
            mode ='lines',
            name="visits_mobile",
            text = x$`Product page`[x$Device=="Mobile"],
            hovertemplate = paste('%{x}', '<br>x$`Product page`: %{text}<br>'),
            texttemplate = '%{y}', textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
            marker = list(color = 'rgb(96,166,218)'),
            line = list(color = 'rgb(96,166,218)')) %>%
  layout(title = "",
         xaxis = list(title = "",
                      textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                      showline = TRUE,
                      linecolor = 'rgb(204, 204, 204)',
                      linewidth = 2,
                      tickfont = list(family = "ING me",
                                      size = 10,
                                      color = 'rgb(105, 105, 105)')),
         yaxis = list(title = "",
                      #range =c(0,100),
                      showticklabels = TRUE,
                      linecolor = 'rgb(204, 204, 204)',
                      linewidth = 2,
                      # autotick = TRUE,
                      # dtick = 10,
                      ticks = 'inside',
                      tickcolor = 'rgb(204, 204, 204)',
                      # tickwidth = 1,
                      # ticklen = 10,
                      tickfont = list(family = "ING me",
                                      size = 10,
                                      color = 'rgb(105, 105, 105)')),
         legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))

print(trend_plot)

}
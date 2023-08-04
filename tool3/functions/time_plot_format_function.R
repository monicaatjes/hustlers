#################################### function to create time plot with ING format #######################

time_plot_format_function <- function(x)  {
    
    # plotly object
  time_plot <- 
  plot_ly(x = as.character(unique(x$quarter))) %>%
  add_trace(y = ~x$con_1[x$Device=="Desktop"], 
            type='bar', 
            name="con_1_Desktop",
            text = x$con_1[x$Device=="Desktop"],
            hovertemplate = paste('%{x}', '<br>x$con_1: %{text}<br>'),
            texttemplate = '%{y}', textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
            marker = list(color = 'rgb(255,098,000)')) %>%
  add_trace(y =~x$con_1[x$Device=="Mobile"], 
            type='bar', 
            name="con_1_Mobile",
            text = x$con_1[x$Device=="Mobile"],
            hovertemplate = paste('%{x}', '<br>x$con_1: %{text}<br>'),
            texttemplate = '%{y}', textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
            marker = list(color = 'rgb(96,166,218)')) %>%
  add_trace(y =~x$con_2[x$Device=="Desktop"], 
            type='bar', 
            name="con_2_Desktop",
            text = x$con_2[x$Device=="Desktop"],
            hovertemplate = paste('%{x}', '<br>x$con_1: %{text}<br>'),
            texttemplate = '%{y}', textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
            marker = list(color = 'rgb(171,0,102)')) %>%
  add_trace(y =~x$con_2[x$Device=="Mobile"], 
            type='bar', name="con_2_Mobile",
            text = x$con_2[x$Device=="Mobile"],
            hovertemplate = paste('%{x}', '<br>x$con_1: %{text}<br>'),
            texttemplate = '%{y}', textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
            marker = list(color = 'rgb 168, 168, 168')) %>%
  add_trace(y =~x$conv_tot[x$Device=="Desktop"], 
            type='bar', 
            name="con_tot_Desktop",
            text = x$conv_tot[x$Device=="Desktop"],
            hovertemplate = paste('%{x}', '<br>x$con_1: %{text}<br>'),
            texttemplate = '%{y}', textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
            marker = list(color = 'rgb(52,150,81)')) %>%
  add_trace(y =~x$conv_tot[x$Device=="Mobile"], 
            type='bar', 
            name="con_tot_Mobile",
            text = x$conv_tot[x$Device=="Mobile"],
            hovertemplate = paste('%{x}', '<br>x$con_1: %{text}<br>'),
            texttemplate = '%{y}', textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
            marker = list(color = 'rgb(82,81,153)')) %>%
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
                      range =c(0,100),
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
         barmode = 'group',
         legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
  
  print(time_plot)

}



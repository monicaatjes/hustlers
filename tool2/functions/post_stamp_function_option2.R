#################################### function to create time plot with ING format #######################

post_stamp_function_option2 <- function(x)  {
  
  # plotly object
  post_stamp <- 
    plot_ly(y = as.character(unique(x$comb_name))) %>%
    add_trace(x = ~x$Desktop_con_1 *100, 
              type='bar', 
              name="con_1_Desktop",
              text = x$Desktop_con_1,
              hovertemplate = paste('%{y}', '<br>x$con_1: %{text}<br>'),
              texttemplate = '%{x}', textposition = 'outside',
              hoverinfo = 'text',
              textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
              
              marker = list(color = 'rgb(255,098,000)')) %>%
    add_trace(x = ~x$Desktop_con_2 *100, 
              type='bar', 
              name="con_2_Desktop",
              text = x$Desktop_con_2,
              hovertemplate = paste('%{y}', '<br>x$con_2: %{text}<br>'),
              texttemplate = '%{x}', textposition = 'outside',
              hoverinfo = 'text',
              textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
              marker = list(color = 'rgb(171,0,102)')) %>%
    
    add_trace(x = ~x$Mobile_con_1 *100, 
              type='bar', 
              name="con_1_Mobile",
              text = x$Mobile_con_1,
              hovertemplate = paste('%{y}', '<br>x$con_1: %{text}<br>'),
              texttemplate = '%{x}', textposition = 'outside',
              hoverinfo = 'text',
              textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
              
              marker = list(color = 'rgb(96,166,218)')) %>%
    add_trace(x = ~x$Mobile_con_2 *100, 
              type='bar', 
              name="con_2_Mobile",
              text = x$Mobile_con_2,
              hovertemplate = paste('%{y}', '<br>x$con_2: %{text}<br>'),
              texttemplate = '%{x}', textposition = 'outside',
              hoverinfo = 'text',
              textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
              marker = list(color = 'rgb(208,217,60)')) %>%
    
    layout(title = "",
           xaxis = list(title = "",
                        textfont = list(color = 'rgb(105, 105, 105)', size = 12, family = "ING me"),
                        range =c(0,100),
                        showline = TRUE,
                        linecolor = 'rgb(204, 204, 204)',
                        linewidth = 2,
                        tickfont = list(family = "ING me",
                                        size = 12,
                                        color = 'rgb(105, 105, 105)')),
           yaxis = list(title = "",
                        # range =c(0,100),
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
                                        size = 12,
                                        color = 'rgb(105, 105, 105)')),
           barmode = 'group',
           legend =list(x= 0.6, y= 0.9, font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
  
  print(post_stamp)
  
}
#################################### function to create table with ING format #######################

tab_function <- function(x)  {
  tab <- plot_ly(
    type = 'table',
    columnwidth = c(0.5, 0.4, 0.4),
    columnorder = c(0, 1, 2),
    header = list(
      values = c("Product", "Conversion1", "Conversion2"),
      align = c("left"),
      line = list(width = 1, color = 'black'),
      fill = list(color = "rgb(255,098,000)"),
      font = list(family = "ING me", size = 14, color = "white")
    ),
    cells = list(
      values = rbind(unique(x$comb_name),
                     round(x$Mobile_QoQ_con_1, digits=2) *100,
                     round(x$Mobile_QoQ_con_2, digits=2) *100
      ),
      align = c("left", "right", "right"),
      line = list(color = c("rgb 168, 168, 168"), width = 1),
      font = list(family = "ING me", size = 12, color = c("rgb 105, 105, 105"))
    ))
  print(tab)
  
}
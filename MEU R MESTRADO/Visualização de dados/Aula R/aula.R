library(plotly)

arquivo2 = url("http://usuarios.upf.br/~holbig/aulas/MCA016/temperatura.Rda")

load (arquivo2)

#exemplo1

plot_ly(temperatura, x = ~ano, y)

p1 <- plot_ly(temperatura, x = ~ano, y = ~menor_minima) %>%
  add_lines(name = ~"Menor Minima")
p2 <- plot_ly(temperatura, x= ~ano, y = ~maio_maxima) %>%
  add_lines(name = ~"Maior Maxima")
subplot(nrows = 2, sharex = TRUE, p1, p2)

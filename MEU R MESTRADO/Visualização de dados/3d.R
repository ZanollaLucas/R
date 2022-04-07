library(plotly)
library(readxl)


##aeronave_csv <- read_excel("C:/Users/LucasZanolla/Desktop/Mestrado/Visualização de dados/aeronave.csv.xlsx")
ocorrencia_csv <- read_excel("C:/Users/LucasZanolla/Desktop/Mestrado/Visualização de dados/ocorrencia.csv.xlsx")

ocorrencia_csv$classificacao[which(ocorrencia_csv$classificacao == "ACIDENTE")] <- 'ACIDENTE'
ocorrencia_csv$classificacao[which(ocorrencia_csv$classificacao == "INCIDENTE GRAVE")] <- 'INCIDENTE GRAVE'
ocorrencia_csv$classificacao <- as.factor(ocorrencia_csv$classificacao)

p <- plot_ly(ocorrencia_csv, x = ~ocorrencia_csv$dia_ocorrencia, y = ~ocorrencia_csv$tipo, z = ~ocorrencia_csv$uf, color = ~ocorrencia_csv$classificacao, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Data'),
                      yaxis = list(title = 'Tipo'),
                      zaxis = list(title = 'Local')))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
p
chart_link <- api_create(p, filename="scatter3d-basic")
chart_link
library(plotly)

aeronave_csv <- read.csv("aeronave.csv")

fatalidades = aeronave_csv$quantidade_fatalidades
fase = aeronave_csv$fase_operacao
df= data.frame(table(fase,fatalidades))
df = df[order(df$fatalidades, decreasing = TRUE), ]
df <- df [!(df$Freq == 0), ]

df$fatalidades = as.character(df$fatalidades)
df <- df %>% replace(.=="NULL", 0)

p <- plot_ly(df, x = ~df$Freq, y = ~df$fatalidades, z = ~df$fase, color = ~df$fatalidades, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Frequencia'),
                      yaxis = list(title = 'Fatalidades'),
                      zaxis = list(title = 'Fase de Voo'))) %>%
  layout(title ="GRÁFICO 3D SOBRE AS FATALIADES E FASE DO OCORRIDO" )
p

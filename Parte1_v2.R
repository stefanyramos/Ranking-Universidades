library(hrbrthemes)
library(gganimate)
library(gapminder)
library(babynames)
library(ggthemes)
library(cowplot)
library(ggplot2)

# Data Manipulation
library(dplyr)

# Statistics
library(DescTools)

# Loading the database 
df <- read.csv("D:/Users/Stefany/Desktop/vgsales.csv", stringsAsFactors = FALSE)


#Ver os tipos dos dados
glimpse(df)

#Mudando o tipo de algumas colunas para fator
df$Rank <- as.factor(df$Rank)
df$Year <- as.integer(df$Year)
df$NA_Sales <- as.double(df$NA_Sales)
df$EU_Sales <- as.double(df$EU_Sales)
df$JP_Sales <- as.double(df$JP_Sales)
df$Other_Sales <- as.double(df$Other_Sales)
df$Global_Sales <- as.double(df$Global_Sales)


#Filtrando linhas
df <- df %>% na.omit
df <- df[df$Year >= 2008 & df$Year <= 2011, ]



# histograma
hist(df$Other_Sales,
     main="Histograma Vendas-Outros",
     xlab="Vendas Outros",
     col="purple",
     border="violet",
     breaks=100
)


# hist 2
options(repr.plot.width = 14, repr.plot.height = 6)
a <- ggplot(data = df, mapping = aes(x = Global_Sales)) +
  geom_histogram(bins = 80, fill = "purple", color = "violet") +
  xlab("Venda Global (em milhões)") +
  ylab("Frequency") +
  ggtitle("Venda Global") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    legend.position = "none")

df2 <- df[df$Global_Sales < 2, ]
b <- ggplot(data = df2, mapping = aes(x =  Global_Sales)) +
  geom_histogram(bins = 80, fill = "purple", color = "violet") +
  xlab("Venda Global") +
  ylab("") +
  ggtitle("Venda Global < 2 milhões") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    legend.position = "none")

plot_grid(a, b, nrow = 1, ncol = 2)

# Plataform

#Criar tabela de frequencia 
frequencia = function(dados, categoria, intervalo){
  
  dados <- dados %>% na.omit
  
  if(intervalo){
    r <- range(dados)
    n <- nclass.Sturges(dados)
    dados <- cut(dados, seq(0, r[2], l = n+1))
  }
  
  dados.plot <- data.frame(table(dados), table(dados)/sum(table(dados)),
                           cumsum(prop.table(table(dados))))
  
  dados.plot <- dados.plot[, -3]
  
  names(dados.plot) <- c(categoria ,"Absoluta", "Relativa", "Acumulada")
  
  
  dados.plot$Relativa <- as.double(dados.plot$Relativa)
  n <- nrow(dados.plot)
  
  for(i in 1:n){
    x <- as.double(dados.plot$Relativa[i])
    y <- as.double(dados.plot$Acumulada[i])
    dados.plot$Relativa[i] <- format(round(x, 5), nsmall = 5)
    dados.plot$Acumulada[i] <- format(round(y, 5), nsmall = 5)
  }
  
  return(dados.plot)
  
}


# criar matriz com medidas de posicao e dispercao
posDisp = function(dados){
  
  dados <- dados %>% na.omit
  
  media <- mean(dados)
  mediana <- median(dados)
  moda <- moda(dados)
  q1 <-quantile(dados)[2]
  q3 <-quantile(dados)[4]
  variancia <- var(dados)
  dp <- sqrt(variancia)
  cv <- dp/media
  
  m <- matrix(NA, nrow = 8, ncol = 2, dimnames = list(c("Media", "Mediana","Moda","Q1","Q3","Var","DP","CV"), 
                                                      c("valores", "-")))
  for(i in 1:8){
    m[i,2] <- " - "
  }
  
  m[1,1] <- format(round(media, 3), nsmall = 3)
  m[2,1] <- mediana
  m[3,1] <- moda
  m[4,1] <- q1
  m[5,1] <- q3
  m[6,1] <- format(round(variancia, 3), nsmall = 3)
  m[7,1] <- format(round(dp, 3), nsmall = 3)
  m[8,1] <- format(round(cv, 3), nsmall = 3)
  
  return(m);
  
}

# Plataform
frPlataform <- frequencia(df$Platform, "Plataform", FALSE)
write.xlsx(frPlataform, file="Plataform-fr.xlsx") 
moda(df$Platform) #DS
###

# Year
frYear <- frequencia(df$Year, "Year", FALSE)
write.xlsx(frYear, file="Year-fr.xlsx")
moda(df$Year) #2009
###

# Genre
frGenre <- frequencia(df$Genre, "Genre", FALSE)
write.xlsx(frGenre, file="Genre-fr.xlsx")
moda(df$Genre) # Action
###

# Publisher
frPublisher <- frequencia(df$Publisher, "Publisher", FALSE)
write.xlsx(frPublisher, file="Publisher-fr.xlsx")
moda(df$Publisher) # Eletronic Arts
###

# NA_Sales
frNA_Sales <- frequencia(df$NA_Sales, "NA_Sales", TRUE)
pdNA_Sales <- posDisp(df$NA_Sales)
write.xlsx(frNA_Sales, file="NA_Sales-fr.xlsx")
write.xlsx(pdNA_Sales, file="NA_Sales-pd.xlsx")
###

# Eu_Sales
frEU_Sales <- frequencia(df$EU_Sales, "EU_Sales", TRUE)
pdEU_Sales <- posDisp(df$EU_Sales)
write.xlsx(frEU_Sales, file="EU_Sales-fr.xlsx")
write.xlsx(pdEU_Sales, file="EU_Sales-pd.xlsx")
###

# JP_Sales
frJP_Sales <- frequencia(df$JP_Sales, "Jp_Sales", TRUE)
pdJP_Sales <- posDisp(df$JP_Sales)
write.xlsx(frJP_Sales, file="JP_Sales-fr.xlsx")
write.xlsx(pdJP_Sales, file="JP_Sales-pd.xlsx")
###

# Other_Sales
frOther_Sales <- frequencia(df$Other_Sales, "Other_Sales", TRUE)
pdOther_Sales <- posDisp(df$Other_Sales)
write.xlsx(frOther_Sales, file="Other_Sales-fr.xlsx")
write.xlsx(pdOther_Sales, file="Other_Sales-pd.xlsx")
###

# Global_Sales
frGlobal_Sales <- frequencia(df$Global_Sales, "Global_Sales", TRUE)
pdGlobal_Sales <- posDisp(df$Global_Sales)
write.xlsx(frGlobal_Sales, file="Global_Sales-fr.xlsx")
write.xlsx(pdGlobal_Sales, file="Global_Sales-pd.xlsx")
###

lfr <- list(frNA_Sales, frEU_Sales,
            frJP_Sales, frOther_Sales, frGlobal_Sales)
lpd <- list(pdNA_Sales, pdEU_Sales,
            pdJP_Sales, pdOther_Sales, pdGlobal_Sales)
write.xlsx(lfr, file="SalesFrequency.xlsx")
write.xlsx(lpd, file="SalesPD.xlsx")

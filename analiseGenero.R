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

# Removing the Rank column
# data$Rank <- NULL

# Filtering only the records of interest for this study, removing the records with Year = NaN and records with the year above 2016
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

# Viewing the first 6 DataFrame records
head(data, 10)
view(data)

# sumary data
summary(data)

g_name_NA <- aggregate(list(NA_Sales = df$NA_Sales), list(Genre = df$Genre), sum)
g_name_NA <- g_name_NA[order(g_name_NA$NA_Sales, decreasing = T), ]

# EU_Sales
g_name_EU <- aggregate(list(EU_Sales = df$EU_Sales), list(Genre = df$Genre), sum)
g_name_EU <- g_name_EU[order(g_name_EU$EU_Sales, decreasing = T), ]


# JP_Sales
g_name_JP <- aggregate(list(JP_Sales = df$JP_Sales), list(Genre = df$Genre), sum)
g_name_JP <- g_name_JP[order(g_name_JP$JP_Sales, decreasing = T), ]

# Other_Sales
g_name_Other <- aggregate(list(Other_Sales = df$Other_Sales), list(Genre = df$Genre), sum)
g_name_Other <- g_name_Other[order(g_name_Other$Other_Sales, decreasing = T), ]

# Global_Sales
g_name_Global <- aggregate(list(Global_Sales = df$Global_Sales), list(Genre = df$Genre), sum)
g_name_Global <- g_name_Global[order(g_name_Global$Global_Sales, decreasing = T), ]


options(repr.plot.width = 20, repr.plot.height = 20)
a <- ggplot(data = g_name_NA, mapping = aes(x = Genre, y = NA_Sales)) +
  geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 2.3, alpha = .8) +
  geom_point(mapping = aes(fill = Genre), size = 7, shape = 21) +
  xlab("") +
  ylab("") +
  ggtitle("Numero de vendas nos EUA (em milhões)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = "none")

b <- ggplot(data = g_name_EU, mapping = aes(x = Genre, y = EU_Sales)) +
  geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 2.3, alpha = .8) +
  geom_point(mapping = aes(fill = Genre), size = 7, shape = 21) +
  xlab("") +
  ylab("") +
  ggtitle("Numero de vendas na Europa (em milhões)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = "none")

c <- ggplot(data = g_name_JP, mapping = aes(x = Genre, y = JP_Sales)) +
  geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 2.3, alpha = .8) +
  geom_point(mapping = aes(fill = Genre), size = 7, shape = 21) +
  xlab("") +
  ylab("") +
  ggtitle("Numero de vendas no Japão(em milhões)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = "none")
d <- ggplot(data = g_name_Other, mapping = aes(x = Genre, y = Other_Sales)) +
  geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 2.3, alpha = .8) +
  geom_point(mapping = aes(fill = Genre), size = 7, shape = 21) +
  geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
  xlab("") +
  ylab("") +
  ggtitle("Numero de vendas no resto do mundo (em milhões)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = "none")

e <- ggplot(data = g_name_Global, mapping = aes(x = Genre, y = Global_Sales)) +
  geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 2.3, alpha = .8) +
  geom_point(mapping = aes(fill = Genre), size = 7, shape = 21) +
  geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
  xlab("") +
  ylab("") +
  ggtitle("Numero de vendas no resto do mundo (em milhões)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = "none")

plot_grid(e, nrow = 1, ncol = 1)

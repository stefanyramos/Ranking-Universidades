library(hrbrthemes)
library(gganimate)
library(gapminder)
library(babynames)
library(ggthemes)
library(cowplot)
library(ggplot2)
library(smbinning)
library(InformationValue)

# Data Manipulation
library(dplyr)

# Statistics
library(DescTools)

# Loading the database 
df <- read.csv("D:/Users/Stefany/Desktop/vgsales2.csv", stringsAsFactors = FALSE)


#Ver os tipos dos dados
glimpse(df)

# qtd de 0s e 1s
table(df$Plataforma)

#Mudando o tipo de algumas colunas para fator
df$Rank <- as.factor(df$Rank)
df$Ano <- as.integer(df$Ano)
df$Vendas_AmericaDoNorte <- as.double(df$Vendas_AmericaDoNorte)
df$Vendas_EU <- as.double(df$Vendas_EU)
df$Vendas_JP <- as.double(df$Vendas_JP)
df$Vendas_Outras <- as.double(df$Vendas_Outras)
df$Vendas_Global <- as.double(df$Vendas_Global)



# colocando a mesma proporcao de 0s e 1s
# Create Training Data
input_ones <- df[which(df$Plataforma == 1), ]  # all 1's
input_zeros <- df[which(df$Plataforma == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 




# converts a continuous variable into a categorical variable 
factor_vars <- c ("Publicadora", "Genero")
continuous_vars <- c("Vendas_Global")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(3))  # init for IV results

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="Plataforma", x=factor_var, maxcat = 15)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="Plataforma", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df




# Build Logit Models and Predict
logitMod <- glm(Plataforma ~ Genero + Vendas_Global, data=trainingData, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores



# Model Diagnostics
summary(logitMod)



misClassError(testData$Plataforma, predicted, threshold = optCutOff)


plotROC(testData$Plataforma, predicted)

Concordance(testData$Plataforma, predicted)


confusionMatrix(testData$Plataforma, predicted, threshold = optCutOff)




predicted.data <- data.frame(
  probability.of.Plataforma=logitMod$fitted.values,
  Plataforma=trainingData$Plataforma)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.Plataforma, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.Plataforma)) +
  geom_point(aes(color=Plataforma), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Probabibilade prevista para ser um PC")



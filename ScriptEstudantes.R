#Carregando pacotes
library(tidyverse)
library(C50)
library(gmodels)
library(ROSE)
library(caret)
library(knitr)
library(kableExtra)

#carregando Dataframes
df1 <- read.table(file.choose(), sep=",", header=TRUE, stringsAsFactors = TRUE)
df2 <- read.table(file.choose(), sep=",", header=TRUE, stringsAsFactors = TRUE)

#Juntando os dois dataframes em um só eliminando alunos que apareciam nos
chaves <- c("school", "sex", "age", "address", "famsize", "Pstatus", 
            "Medu", "Fedu", "Mjob", "Fjob", "reason", "guardian")
df_final <- df2 %>%
  anti_join(df1, by = chaves) %>%
  bind_rows(df1)

cat(sprintf("O dataset tinha %d valores nulos e depois do tratamento passou a ter %d linhas.", 
            sum(is.na(df_final)), nrow(df_final)))

#Estatística descritiva
numericas <- df_final %>% select(where(is.numeric))
binarias <- df_final %>% select(where(~ all(. %in% c(0, 1, "yes", "no"))))
summary_transposed <- t(summary(numericas))

kable(summary_transposed, caption = "Resumo Estatístico das Variáveis Numéricas", format = "latex", booktabs = TRUE, longtable = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "repeat_header"), full_width = FALSE, bootstrap_options = c("striped", "hover"))

kable(summary(binarias), caption = "Resumo Estatístico das Variáveis Binárias")

#Gráfico 1
ggplot(df_final, aes(x = higher, fill = higher)) +
  geom_bar() +
  labs(x = "Deseja cursar ensino superior?", 
       y = "Contagem") +
  theme_minimal()

#Gráfico 2
ggplot(df_final, aes(x = studytime, y = G3)) +
  geom_jitter(alpha = 0.5, width = 0.2) +
  geom_smooth(method = "lm", color = "red")

#Gráfico 3
ggplot(df_final, aes(x = factor(Medu), y = G3)) +
  geom_boxplot(fill = "lightgreen") +
  labs(x = "Nível de Educação da Mãe")

#Criando modelo
set.seed(123)
index <- sample(1:nrow(df_final), 0.7 * nrow(df_final))
train <- df_final[index, ]
test <- df_final[-index, ]

modelo <- C5.0(higher ~ ., data = train)
summary(modelo)

#Avaliando modelo
pred <- predict(modelo, test)
conf_matrix <- confusionMatrix(pred, test$higher)
print(conf_matrix)

#Balanceando o modelo utilizando o pacote caret
df_balanced <- ovun.sample(higher ~ ., data = df_final, method = "over")$data
table(df_balanced$higher)

#Criando modelo 2
set.seed(123)
index <- sample(1:nrow(df_balanced), 0.7 * nrow(df_balanced))
train2 <- df_balanced[index, ]
test2 <- df_balanced[-index, ]

modelo2 <- C5.0(higher ~ ., data = train2)
summary(modelo2)

#Avaliando modelo 2

pred <- predict(modelo2, test)
conf_matrix2 <- confusionMatrix(pred, test$higher)
print(conf_matrix2)
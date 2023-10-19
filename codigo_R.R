library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gglm)
library(writexl)


df <- read_excel("dados/wilson_expl (2).xlsx")
view(df)
glimpse(df)


# Queremos uma equação conc = a (ln(dia)) + b
# A regressão linear será feita usando a tranformação x = ln(x)
# Deste modo acharemos uma equação conc = ax + b

df$ln_dia <-log(df$dia)



ggplot(df, aes(x = ln_dia, y = conc_c1)) +
  geom_point()

# Excluir linhas Faltantes
df <- na.omit(df)
# Figuras

ggplot(data = df) +
  geom_point(aes(x = dia, y = conc_c1, color = "C1"), size = 2) +
  geom_point(aes(x = dia, y = conc_c2, color = "C2"), size = 2) +
  geom_point(aes(x = dia, y = conc_c3, color = "C3"), size = 2) +
  scale_color_manual(values = c("C1" = "blue", "C2" = "red", "C3" = "yellow")) +
  labs(x = "Tempo (dias)",
       y = "Concentração (mg/L)",
       title = "Concentração de fenólicos na cachaça") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 20))


#library(dplyr)

# Filtro para o número ln(0), que é nan
df_filtered <- df %>%
  filter(dia > 0) 

ggplot(data = df_filtered) +
  geom_point(aes(x = dia, y = conc_c1, color = "C1"), size = 2) +
  geom_point(aes(x = dia, y = conc_c2, color = "C2"), size = 2) +
  geom_point(aes(x = dia, y = conc_c3, color = "C3"), size = 2) +
  geom_smooth(aes(x = dia, y = conc_c1, color = "C1"), method = "lm", formula = y ~ log(x), se = FALSE) +
  geom_smooth(aes(x = dia, y = conc_c2, color = "C2"), method = "lm", formula = y ~ log(x), se = FALSE) +
  geom_smooth(aes(x = dia, y = conc_c3, color = "C3"), method = "lm", formula = y ~ log(x), se = FALSE) +
  scale_color_manual(values = c("C1" = "blue", "C2" = "red", "C3" = "yellow"), 
                     breaks = c("C1", "C2", "C3")) +
  labs(x = "Tempo (dias)",
       y = "Concentração (mg/L)",
       title = "Concentração de fenólicos na cachaça") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 20))

# LINEARIZAÇÂO

# Sem Linha de tendência linear

view(df)

ggplot(data = df) +
  geom_point(aes(x = ln_dia, y = conc_c1, color = "C1"), size = 2) +
  geom_point(aes(x = ln_dia, y = conc_c2, color = "C2"), size = 2) +
  geom_point(aes(x = ln_dia, y = conc_c3, color = "C3"), size = 2) +
  scale_color_manual(values = c("C1" = "blue", "C2" = "red", "C3" = "yellow")) +
  labs(x = "ln(tempo)",
       y = "Concentração (mg/L)",
       title = "Concentração de fenólicos na cachaça") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 20))

#Com linha de tendÊncia linear

ggplot(data = df) +
  geom_point(aes(x = ln_dia, y = conc_c1, color = "C1"), size = 2) +
  geom_point(aes(x = ln_dia, y = conc_c2, color = "C2"), size = 2) +
  geom_point(aes(x = ln_dia, y = conc_c3, color = "C3"), size = 2) +
  geom_smooth(aes(x = ln_dia, y = conc_c1, color = "C1"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = ln_dia, y = conc_c2, color = "C2"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = ln_dia, y = conc_c3, color = "C3"), method = "lm", se = FALSE) +
  scale_color_manual(values = c("C1" = "blue", "C2" = "red", "C3" = "yellow")) +
  labs(x = "ln(tempo)",
       y = "Concentração (mg/L)",
       title = "Concentração de fenólicos na cachaça") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 20))



df_filtered <- df %>%
  filter(dia > 0)
df_filtered <- na.omit(df_filtered)



view(df_filtered)

# MODELO PARA C1

fit_df1 <- lm(conc_c1 ~ ln_dia, data = df_filtered)
fit_df1

sum_fit1 <- summary(fit_df1)
sum_fit1

fit_an1 <- anova(fit_df1)
fit_an1
 
# testes t e f ok

#Predição dos novos valores 7,14,42,77

dias_novos <- c(7, 14, 42, 77)

novo_df <- data.frame(dia = dias_novos)
novo_df$valores_ln_dia <- log(dias_novos)


novo_df$previsao <- predict(fit_df1, newdata = data.frame(ln_dia = novo_df$valores_ln_dia))

view(novo_df)

# Exportar para um arquivo Excel

write_xlsx(novo_df, "dados/dados_tratados/novos_dados_c1.xlsx")



library(dplyr)
library(foreign)
library(haven)
library(tidyr)
library(rlang)
library(nnet)
library(stargazer)

# Model with dummy-coded colonial legacy

df_col <- Reduce(function(x,y) merge(x = x, y = y, by = "ccodecow", all.x = TRUE, all.y = FALSE), 
                   list(partyreg, ELF_df, PSI_df, col_legacy_df, rules_df, dem_df, edu_df, oil_df))

df_col$ccodecow <- as.character(df_col$ccodecow)
df_col$year.x <- as.character(df_col$year.x)
df_col$edu <- as.numeric(df_col$edu)

df_col1 <- df_col %>%
  subset(., select = which(!duplicated(names(.))))%>%
  select(ccodecow, country_name.x, country_text_id.x, year.x, partyreg, ELF, PSI = v2xps_party, housesys, dem = v2x_polyarchy, oil.x, edu, col_legacy = `max(col_legacy)`)%>%
  distinct()%>%
  mutate_all(~replace(., is.na(.), 0))
  df_col1[df_col1$ccodecow== 403, "ELF"] <- 0.189797
  df_col1[df_col1$ccodecow== 591, "ELF"] <- 0.3028665
  df_col1[df_col1$ccodecow== 402, "ELF"] <- 0.4949435
  
df_col1$col_legacy.f <- factor(df_col1$col_legacy)
df_col1$partyreg <- factor(df_col1$partyreg)

df_col_m1 <- multinom(partyreg ~ ELF + PSI + housesys + col_legacy.f + dem + oil.x + edu, data = df_col1)
summary(df_col_m1)
stargazer(df_col_m1, type="text")

df_col_m2 <- multinom(partyreg ~ ELF + housesys, data = df_col1)
summary(df_col_m2)
stargazer(df_col_m2, type="text")

# Model with double the size of the sample

df_double <- Reduce(function(x,y) merge(x = x, y = y, by = "ccodecow", all.x = TRUE, all.y = FALSE), 
                 list(partyreg, ELF_df, PSI_df, col_legacy_df, rules_df, dem_df, edu_df, oil_df))

df_double$ccodecow <- as.character(df_double$ccodecow)
df_double$year.x <- as.character(df_double$year.x)
df_double$edu <- as.numeric(df_double$edu)


df_double1 <- df_double %>%
  subset(., select = which(!duplicated(names(.))))%>%
  select(ccodecow, country_name.x, country_text_id.x, year.x, partyreg, ELF, PSI = v2xps_party, housesys, dem = v2x_polyarchy, oil.x, edu, col_legacy = `max(col_legacy)`)%>%
  distinct()%>%
  mutate_all(~replace(., is.na(.), 0))
  df_double1[df_double1$ccodecow== 403, "ELF"] <- 0.189797
  df_double1[df_double1$ccodecow== 591, "ELF"] <- 0.3028665
  df_double1[df_double1$ccodecow== 402, "ELF"] <- 0.4949435
  df_double1$col_legacy <- factor(df_double1$col_legacy)
  df_double1$partyreg <- factor(df_double1$partyreg)

df_double_ <- df_double1[rep(seq_len(nrow(df_double1)), each = 2), ]

df_double_m1 <- multinom(partyreg ~ ELF + PSI + housesys + col_legacy + dem + oil.x + edu, data = df_double_)
summary(df_double_m1)

stargazer(df_double_m1, type="text")

# Model with ENP dependent variable

df_enp <- merge(df_col1, enp, by.x = c('year.x','ccodecow'), by.y = c('year','COWcode'))%>%
  select(-v2panumbseat, -pf_party_id)%>%
  distinct()

df_enp_m1 <- lm(ENPP ~ ELF + PSI + housesys + col_legacy + dem + oil.x + edu, data = df_enp)
summary(df_enp_m1)

stargazer(df_enp_m2, type="text")

df_enp_m2 <- lm(ENPP ~ ELF + PSI + housesys + col_legacy + dem + oil.x + edu, data = df_enp)
summary(df_enp_m2)

stargazer(df_enp_m2, type="text")

# Full model 



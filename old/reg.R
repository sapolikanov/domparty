install.packages('mlogit')
install.packages('sjlabelled')
install.packages('olsrr')
install.packages('olsrr')
install.packages('MASS')
install.packages('Hmisc')
install.packages('stargazer')

# Install packages

library(foreign)
library(nnet)
library(ggplot2)
library(mlogit)
library(ggpubr)
library(sjlabelled)
library(olsrr)
library(MASS)
library(Hmisc)
library(stargazer)

dir.create(tempdir())
# Read master dataset

getwd()

df <- readRDS('df.RDS')
str(df)
df$ccodecow <- as.character(df$ccodecow)
df$year <- as.character(df$year.x)
df$edu <- as.numeric(df$edu)
df$partyreg <- factor(df$partyreg)
df1 <- df
summary(df1)

# Multinom from NNET

df1$partyreg <- as.factor(df1$partyreg)

df$partyreg1 <- relevel(df$partyreg, ref = '0')
test <- multinom(partyreg ~ ELF + PSI + housesys + dem + oil.x + edu, data = df1)
summary(test)

stargazer(test, type="text", out="test.text")


z <- summary(test)$coefficients/summary(test)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(test))

head(pp <- fitted(test))

dff <- data.frame(housesys = c("majority", "proportional"), write = mean(df$write))

# mlogit

df1 <- mlogit.data(df, varying = NULL, choice = 'partyreg', shape = 'wide')
head(df1)

m1 <- mlogit(partyreg ~ 0 | ELF + PSI + housesys + dem+ oil.x + edu, data = df1, reflevel = 1)
summary(m1)

m3 <- mlogit(partyreg ~ 0 |  dem+ oil.x + edu, data = df1, reflevel = 1)
summary(m3)

m2 <- mlogit(partyreg ~ 0 | ELF + PSI + housesys, data = df1, reflevel = 1)
summary(m2)

vars <- data.frame(df$ELF, df$PSI, df$housesys, df$oil.x, df$edu, df$dem)

vars$df.partyreg <- as.numeric(vars$df.partyreg)
vars$df.housesys <- as.numeric(vars$df.housesys)
unlabel(vars$df.housesys)

cor(x = vars, method = 'spearman')
round(cor(vars), digits = 2)

# Ordinal

m <- polr(partyreg ~ ELF + PSI + housesys + dem+ oil.x + edu, data = df, Hess = TRUE)
summary(m)

(ci <- confint(m))

# Install packages
install.packages('caret')
install.packages("corrplot")
install.packages('performance')
install.packages('DescTools')

# Load packages
library(dplyr)
library(caret)
library(nnet)
library(mlogit)
library(car)
library(corrplot)
library(DescTools)
library(effects)
source("http://www.sthda.com/upload/rquery_cormat.r")
da <- read_dta('Smith.Waldner.2020.Replication.dta'
)

# Merge predictor datasets
df_col <- Reduce(function(x,y) merge(x = x, y = y, by = "ccodecow", all.x = TRUE, all.y = FALSE), 
                 list(partyreg, ELF_df, ELF_df1, PSI_df, col_legacy_df, rules_df, dem_df, edu_df, inc_df, oil_df))

  # convert to correct classes
  df_col$ccodecow <- as.character(df_col$ccodecow)
  df_col$year.x <- as.character(df_col$year.x)
  df_col1$edu <- as.numeric(df_col1$edu)

# Clean data
df_col1 <- df_col %>%
  subset(., select = which(!duplicated(names(.))))%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  dplyr::select(ccodecow, country_name.x, country_text_id.x, year.x, partyreg, 
                ELF, PSI = v2xps_party, housesys, dem = v2x_polyarchy, oil.x, 
                edu, statuss, col_legacy = `max(col_legacy)`, inc = v2eltvrig)%>%
  group_by(ccodecow)%>%
  distinct()

  # fill in NA's with hand-picked numbers as described in the text
  df_col1[df_col1$ccodecow== 403, "ELF"] <- 0.189797
  df_col1[df_col1$ccodecow== 591, "ELF"] <- 0.3028665
  df_col1[df_col1$ccodecow== 402, "ELF"] <- 0.4949435

  # Modify incumbency to reflect two levels
  df_col1$inc[df_col1$inc==1] <- ' no incumbency'
  df_col1$inc[df_col1$inc==2] <- ' no incumbency'
  df_col1$inc[df_col1$inc==0] <- ' incumnbency'
  
  # Modify the dependent variable to be easier on the eyes
  df_col1$partyreg[df_col1$partyreg==0] <- 'non-dominant'
  df_col1$partyreg[df_col1$partyreg==1] <- 'dominant'
  df_col1$partyreg[df_col1$partyreg==2] <- 'dominant'
  
  # Convert all discrete variables to factors
  df_col1$col_legacy.f <- factor(df_col1$col_legacy)
  df_col1$partyreg <- factor(df_col1$partyreg)
  df_col1$inc <- factor(df_col1$inc)
  
# Save new data frame for modeling
df <- df_col1
df <- as.data.frame(df)
str(df)
# Perform k-fold cross-validation to find the best fit
  # No data partitioning due to small n

  # Re level the dependent varible with reference - 'non-dominant'
  df$partyreg <- relevel(df$partyreg, ref = 'non-dominant')

  # Specify parameters
  cntrlspecs <- trainControl(method = 'cv', number = 5,
                           savePredictions = 'all',
                           classProbs = TRUE)

  # Set a random seed
  set.seed(1845)

  # Specify the model
  model_k <- train(partyreg ~ ELF + PSI + housesys + col_legacy.f + oil.x + dem + edu, 
                 data = df, Hess = TRUE,
                 method = 'multinom', 
                 .trControl = cntrlspecs
                 )
  
  # Goodness-of-fit
  print(model_k)
  summary(model_k)
  
  # Predictor importance
  varImp(model_k)
  
  # Get predictions
  predictions <- predict(model_k, df)
  confusionMatrix(data = predictions, df$partyreg)
 
# Model 1 - reference is 'non-dominant'
model1 <- multinom(formula = partyreg ~ ELF + PSI + housesys + col_legacy.f + dem + inc + 
                     edu + oil.x, data = df, Hess = TRUE)
summary(model1)

exp(coef(model1))
PseudoR2(model1, which = 'McFadden')


  # Extract results
  stargazer::stargazer(model1, type = 'latex', out = 'out.tex')
  
  # Multicollinearity
  
    # Prepare data for correlation 
    pred <- df %>% ungroup(ccodecow)%>%
    select(-ccodecow, -country_name.x, -country_text_id.x, -year.x, -partyreg, -inc, -partyreg,-col_legacy)%>%
    mutate_if(is.factor, ~ as.numeric(as.character(.x)))
    
    # Build a corellogram
    rquery.cormat(pred)
    
    # VIF
    vif(model1)
  
    
model_log <- glm(partyreg ~ ELF + PSI + housesys + col_legacy.f + dem + inc + 
                   edu + oil.x, data = df, family = binomial)
summary(model_log)
stargazer::stargazer(model_log, type = 'text', out = 'outl.txt')

exp(coef(model1))
head(pp <- fitted(model1))

dwrite <- data.frame(ses = rep(c("non-dominant", "autocratic dominant", "democratic dominant"), each = 41), write = rep(c(30:70),
                                                                                   3))
pp.write <- cbind(dwrite, predict(model1, newdata = dwrite, type = "probs", se = TRUE))

ggplot(df, aes(x = partyreg, y = PSI, colour = ses)) + geom_line() + facet_grid(variable ~
                                                                                        ., scales = "free")
plot(allEffects(model1))

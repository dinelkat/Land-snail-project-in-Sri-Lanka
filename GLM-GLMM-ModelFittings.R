setwd("/Users/dinelkathilakarathnegmail.com/Library/CloudStorage/OneDrive-UniversityofNebraska-Lincoln/Education/Publications/InReview/ByShalikaMadam/2024-CropDamage/DataAnalysis")

#loading datasheet and checking summary
env <-read.csv("all.csv", header = TRUE, sep = ',', na.strings = "NA") 
head(env)
summary(env)


# Load necessary libraries
library(tidyr)
library(dplyr)
library(tidyverse)
library(lme4)
library(pscl)
library(MASS)
library(MuMIn)
library(DHARMa)
library(car)
library(glmmTMB)

#finding column names
colnames(env)

#making some columns as character/factors
env$Species <-as.factor(env$Species)
env$Site <-as.factor(env$Site)
env$Crop <-as.factor(env$Crop)
env$Stage <-as.factor(env$Stage)
env$Field.Type <-as.factor(env$Field.Type)
env$Part <-as.factor(env$Part)
env$Season <-as.factor(env$Season)

summary(env)


#Extracting total abundance of pest gastropods
data <- env %>% group_by(Site) %>% summarise(total.abun=sum(Abundance), damage=sum(Damage))
data
plot((data$damage ~ data$total.abun))
abline(lm(data$damage ~ data$total.abun))
summary(lm(data$damage ~ data$total.abun))

#Check multicolinearity
vif(lm(Damage ~ Abundance + Crop + Part + Season + Species, data = env))#factors are not colinear



#GLM# check the fixed effects of crop, stage, field type, parts, species, abundance on damage

f1 <- glm(data = env, Damage ~ Abundance + Crop + Stage + Field.Type + Part + Season + Species)

f2 <- glm(data = env, Damage ~ Abundance + Crop + Stage + Part + Season + Species)
#Best model, since the intercept is not significant, only all of these variables are enough to explained the degree of damage
summary(f2)
drop1(f2, test = "Chisq")#evaluate the relationship of each variables on the model

f3 <- glm(data = env, Damage ~ Abundance + Crop + Part + Season + Species)
f4 <- glm(data = env, damage ~ Abundance + Crop + Stage + Part + Species)
f5 <- glm(data = env, damage ~ Abundance + Crop + Stage + Season + Species)
f6 <- glm(data = env, damage ~ Abundance + Crop + Stage + Species)

AIC(f1, f2, f3, f4, f5, f6)
#model comparison
anova(f1, f2, f3, f4, f5, f6)



#model evaluation normality and residual dispersion
# Pseudo-R² measures
pR2(f2)
par(mfrow = c(2, 2))
plot(f2) 
res <- residuals(f2, type = "deviance")
fit <- fitted(f2)

plot(fit, res,
     xlab = "Fitted values",
     ylab = "Deviance residuals")
abline(h = 0, lty = 2, col = "red")


# Ratio of residual deviance to df
dispersion <- deviance(f2) / df.residual(f2)
dispersion #residuals are over dispersed. So try to use negative binomial distribution to fit the error


#f2 model in negative binomial distribution
f2_nb <- glm.nb(data = env, Damage ~ Abundance + Crop + Stage + Part + Season + Species)
summary(f2_nb)#best fit model
# Extract coefficient table
coef_table <- coef(summary(f2_nb))

# Save to CSV
write.csv(coef_table, "f2_nb_summary.csv", row.names = TRUE)

# Make a data frame with extra info
model_info <- data.frame(
  AIC = AIC(f2_nb),
  BIC = BIC(f2_nb),
  logLik = logLik(f2_nb)
)

write.csv(model_info, "f2_nb_fit.csv", row.names = FALSE)


data1 <- drop1(f2_nb, test = "Chisq")
write.csv(data1, "SummaryNegativeBinomialGLM.csv")


#check the residuals variance
par(mfrow = c(2, 2))
plot(f2_nb) 
res_nb <- residuals(f2_nb, type = "deviance")
fit_nb <- fitted(f2_nb)

plot(fit_nb, res_nb,
     xlab = "Fitted values",
     ylab = "Deviance residuals")
abline(h = 0, lty = 2, col = "red")

dispersion_nb <- deviance(f2_nb) / df.residual(f2_nb)#this is closer to 1, so this model is better than the f2
dispersion_nb

AIC(f2, f2_nb)

# R² (pseudo-R² for GLM)
r2_glm <- r.squaredGLMM(f2_nb)

# Make a table row
glm_table <- data.frame(
  Model = "GLM (NegBin)",
  R2_marginal = r2_glm[1],  # same as McFadden-style
  R2_conditional = NA,      # not applicable for GLM
  Dispersion = dispersion_nb
)
write.csv(glm_table, "GLMNegativeFit.csv")






#GLMM# checking the random effect from the sampling locations
#Adding random effect from the sampling location to the model
f7 <- glmer(Damage ~ Abundance + Crop + Stage + Field.Type + Part + Season + Species + (1|Site),
                family = gaussian(link = "identity"), data = env)
summary(f7)
drop1(f7, test = "Chisq")


f8 <- glmer(Damage ~ Abundance + Crop + Stage + Part + Season + Species + (1|Site),
                family = gaussian(link = "identity"), data = env)#best model under gaussian distribution


anova(f7, f8)

AIC(f1, f7, f2_nb, f8) #Still f2_nb is the best among all the models, no random effect from sites


#Checking model fits - dispersion and zero inflation
par(mfrow = c(2, 2))
plot(f8)
r.squaredGLMM(f8)


sim <- simulateResiduals(fittedModel = f8)
plot(sim)
testDispersion(sim)#check dispersion
testZeroInflation(sim)#based on zero inflation, it is significant



#dealing with zero inflation
f8_zi <- glmmTMB(
  Damage ~ Abundance + Crop + Stage + Part + Season + Species + (1|Site),
  ziformula = ~1,                # models the probability of "extra" zeros
  family = nbinom2(),
  data = env
)


sim_zi <- simulateResiduals(f8_zi)
plot(sim_zi)
testDispersion(sim_zi)
testZeroInflation(sim_zi)#Still have zero inflation, predict zero > observed zero


f8_zi2 <- glmmTMB(
  Damage ~ Abundance + Crop + Stage + Part + Season + Species + (1|Site),
  ziformula = ~ Crop + Season,   # zero inflation varies by crop & season
  family = nbinom2(),
  data = env
)

sim_zi2 <- simulateResiduals(f8_zi2)
plot(sim_zi2)
testDispersion(sim_zi2)
testZeroInflation(sim_zi2)


f8_zi3 <- glmmTMB(
  Damage ~ Abundance + Crop + Stage + Part + Season + Species + (1|Site),
  ziformula = ~ Crop + Part + Season,   # zero inflation varies by crop, part & season
  family = nbinom2(),
  data = env
)

sim_zi3 <- simulateResiduals(f8_zi3)
plot(sim_zi3)
testDispersion(sim_zi3)
testZeroInflation(sim_zi3)


f8_zi4 <- glmmTMB(
  Damage ~ Abundance + Crop + Stage + Part + Season + Species + (1|Site),
  ziformula = ~ Abundance,   # zero inflation varies by abundance
  family = nbinom2(),
  data = env
)#This is much best model than f2_nb but still zero inflation matters

sim_zi4 <- simulateResiduals(f8_zi4)
plot(sim_zi4)
testDispersion(sim_zi4)
testZeroInflation(sim_zi4)

AIC(f7, f8, f8_zi, f8_zi2, f8_zi3, f8_zi4, f8_zi_reduced)#can't remove zero inflation


summary(f8_zi4)
data3 <- drop1(f8_zi4, test="Chisq")
write.csv(data3, "GLMMChiSq.csv")


#Extract parts from summary
s <- summary(f8_zi4)

cond_coefs <- as.data.frame(s$coefficients$cond)
cond_coefs$Model <- "Conditional"

zi_coefs <- as.data.frame(s$coefficients$zi)
zi_coefs$Model <- "Zero-inflation"

disp_coefs <- as.data.frame(s$coefficients$disp)

if (!is.null(disp_coefs) && nrow(disp_coefs) > 0) {
  disp_coefs$Model <- "Dispersion"
}

#Combine only non-empty data frames
all_coefs <- dplyr::bind_rows(cond_coefs, zi_coefs, disp_coefs)

#Save to CSV
write.csv(all_coefs, "f8_zi4_coefficients.csv")



model <- f8_zi4  

#Extract values
aic_val  <- AIC(model)
bic_val  <- BIC(model)
ll_val   <- logLik(model)

r2_vals  <- tryCatch({
  r.squaredGLMM(model)
}, error = function(e) data.frame(R2m = NA, R2c = NA))

disp_val <- tryCatch({
  deviance(model) / df.residual(model)
}, error = function(e) NA)

#Build a one-row data frame
fit_stats <- data.frame(
  Model          = "GLM (NegBin)",
  AIC            = aic_val,
  BIC            = bic_val,
  logLik         = as.numeric(ll_val),
  R2_marginal    = r2_vals[1,1],
  R2_conditional = ifelse(ncol(r2_vals) > 1, r2_vals[1,2], NA),
  Dispersion     = disp_val
)

#Save to CSV
write.csv(fit_stats, "GLMMmodel_fit_summary.csv", row.names = FALSE)

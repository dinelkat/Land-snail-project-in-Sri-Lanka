setwd("/Users/dinelkathilakarathnegmail.com/Library/CloudStorage/OneDrive-UniversityofNebraska-Lincoln/Education/Publications/InReview/ByShalikaMadam/2024-CropDamage/DataAnalysis")

#loading datasheet and checking summary
economy <-read.csv("economic.csv", header = TRUE, sep = ',') 
economy
summary(economy)

#loading packages
library(lme4)
library(agridat)
library(lmtest)


# Convert the variables to their natural logarithms
# Remove rows with NA, negative, or zero values in any of the relevant variables
economy_filtered <- subset(economy, Yield > 0 & IC > 0 & F > 0 & M > 0 & P > 0 & LC > 0)

# Fit the model with the filtered data without environmental variables
f1 <- lm(log(Yield) ~ log(IC) + log(F) + log(M) + log(P) + log(LC), data=economy_filtered)
summary(f1)
anova(f1, test = "Chisq")
drop1(f1, test="Chisq") #Check likelihood of parameters

f2 <- lm(log(Yield) ~ log(IC) * log(F) * log(M) * log(P) * log(LC), data=economy_filtered)

f3 <- lm(log(Yield) ~ log(IC) + log(M) + log(F), data=economy_filtered)

f4 <- lm(log(Yield) ~ log(IC) + log(M), data=economy_filtered)#best model

f5 <- lm(log(Yield) ~ log(M), data=economy_filtered)#Best fit model

f6 <- lm(log(Yield) ~ 1, data=economy_filtered)#intercept model


AIC(f1,f2,f3,f4,f5, f6)
summary(f5)
anova(f5, test = "Chisq")

#Write csv files to f1 and f5 models
f1a <- summary(f1)
coefficients_f1_df <- as.data.frame(f1a$coefficients)
write.csv(coefficients_f1_df, "f1.csv", row.names = FALSE)

f5a <- summary(f5)
coefficients_df1 <- as.data.frame(f5a$coefficients)
write.csv(coefficients_df1, "f5.csv", row.names = FALSE)



#Model with environmental variables
f7 <- lm(log(Yield) ~ log(IC) + log(F) + log(M) + log(P) + log(LC) + E + PH + RH + RF + T, data=economy_filtered)
f7a <- summary(f7)
df_summary <- anova(f7, test = "Chisq")
coefficients_df7 <- as.data.frame(f7a$coefficients)
write.csv(coefficients_df7, "f7.csv", row.names = FALSE)
write.csv(df_summary, "SummaryCobbweb.csv")

f8 <- lm(log(Yield) ~ log(M) + T, data=economy_filtered)

AIC(f1, f2, f3, f4, f5, f5_glm, f5_glm_null, f6, f7, f8)

#check the model fits (normality and dispersion, etc.)
summary(f5)$r.squared#R²
summary(f5)$adj.r.squared#Adjusted R²

# Basic residual plots
par(mfrow=c(2,2))   # show 4 plots in one window
plot(f5)
par(mfrow=c(1,1))   # reset to single plot

res5 <- residuals(f5, type = "deviance")
fit5 <- fitted(f5)

plot(fit5, res5,
     xlab = "Fitted values",
     ylab = "Deviance residuals")
abline(h = 0, lty = 2, col = "red")


# Ratio of residual deviance to df
dispersion <- deviance(f5) / df.residual(f5)
dispersion

shapiro.test(residuals(f5))#data are not normally distributed


bptest(f5)#Breusch-Pagan test...error variance is constant

anova(f5, f6)#compare best model vs intercept-only, f5 is better than f6 (intercept only model)


f5_glm <- glm(log(Yield) ~ log(M), data = economy_filtered, 
              family = Gamma(link="log"))#better than f5
summary(f5_glm)

AIC(f5, f5_glm)

deviance(f5_glm) / df.residual(f5_glm)

shapiro.test(residuals(f5_glm))

#Null model
f5_glm_null <- glm(log(Yield) ~ 1, family = Gamma(link="log"), data = economy_filtered)

#McFadden pseudo R2
R2_McFadden <- 1 - (logLik(f5_glm) / logLik(f5_glm_null))
R2_McFadden


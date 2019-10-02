# Variable Selecation
# Eyeballing Varibales
summary(model_2)
# Weight, Height, Adiposity and Knee all has a large p-value.

# Mallow's Cp
X <- model.matrix(model_2)[,-1]
Y <- bodyfat_2[,1]

library(leaps)
library(faraway)

g <- leaps(X,Y)
Cpplot(g)

# (1,3,7,13,14) seems to be a good choice.
cp.choice <- c(1,3,7,13,14)+1 # column 1 responds to bodyfat.
bodyfat_cp <- bodyfat_2[,c(1,cp.choice)]
head(bodyfat_cp)

# Adjusted R^2
g <- leaps(X,Y, nbest=1, method="adjr2")
plot(g$adjr2)

(g$which)[which(g$adjr2 == max(g$adjr2)),]

r2.choice <- which((g$which)[which(g$adjr2 == max(g$adjr2)),]==T)+1 # column 1 responds to bodyfat.
bodyfat_r2 <- bodyfat_2[,c(1,r2.choice)]
head(bodyfat_r2)

summary(model_r2 <- lm(BODYFAT ~ ., data=bodyfat_r2))

# AIC and BIC
library(MASS)
model_AIC_back <- stepAIC(model_2, direction="backward", k = 2)
summary(model_AIC_back)

model_BIC_back <- stepAIC(model_2, direction="backward", k = log(nrow(bodyfat_2)))
summary(model_BIC_back)

model_AIC_for <- stepAIC(model_2, direction="forward", k = 2)
summary(model_AIC_for)

model_BIC_for <- stepAIC(model_2, direction="forward", k = log(nrow(bodyfat_2)))
summary(model_BIC_for)

model_AIC_step <- stepAIC(model_2, direction="both", k = 2)
summary(model_AIC_step)

model_BIC_step <- stepAIC(model_2, direction="both", k = log(nrow(bodyfat_2)))
summary(model_BIC_step)
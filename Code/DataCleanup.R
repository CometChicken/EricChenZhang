# Data Clean Up
bodyfat_raw <- read.csv("BodyFat.csv")
head(bodyfat_raw)

bodyfat <- bodyfat_raw[,-1]
head(bodyfat)

library(corrplot)
corrplot(cor(bodyfat), method = "circle")
summary(bodyfat)

plot(bodyfat[,'HEIGHT'],ylab = 'Height')
d1 = which.min(bodyfat[,'HEIGHT'])
d1_height = sqrt(bodyfat[d1,'WEIGHT']*703/(bodyfat[d1,'ADIPOSITY']))
d1_height
bodyfat[d1,'HEIGHT']
bodyfat[d1,'HEIGHT'] =  d1_height
plot(bodyfat[,'HEIGHT'],ylab = 'Height')

plot(bodyfat[,'BODYFAT'],ylab = 'Bodyfat')
d2 = which(bodyfat$BODYFAT==sort(bodyfat$BODYFAT,partial=1)[1])
d2_bodyfat = (495/bodyfat[d2,'DENSITY']-450)
d2_bodyfat
bodyfat[d2,'BODYFAT']

d3 = which(bodyfat$BODYFAT==sort(bodyfat$BODYFAT,partial=nrow(bodyfat))[nrow(bodyfat)])
d3_bodyfat = (495/bodyfat[d3,'DENSITY']-450)
d3_bodyfat
bodyfat[d3,'BODYFAT']

d4 = which(bodyfat$BODYFAT==sort(bodyfat$BODYFAT,partial=2)[2])
d4_bodyfat = (495/bodyfat[d4,'DENSITY']-450)
d4_bodyfat
bodyfat[d4,'BODYFAT']

d5 = which.max(bodyfat[,'ADIPOSITY'])
bodyfat[d5,'ADIPOSITY']

eyeballing_cleanup = c(d2,d3,d4,d5)
bodyfat_1 = bodyfat[-eyeballing_cleanup,-2]
summary(model_1 <- lm(BODYFAT ~ ., data=bodyfat_1))

layout(matrix(1:4, ncol=2))
plot(model_1)

# Plot the Cook's Distance using the traditional 4/(n-p) criterion
cooksd <- cooks.distance(model_1)
sample_size <- nrow(bodyfat_1)
para_size <- ncol(bodyfat_1)-1
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cook\'s distance", ylab = 'Cook\'s Distance')  # plot cook's distance
abline(h = 4/(sample_size-para_size), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/(sample_size-para_size), names(cooksd),""), col="red")  # add labels

#influential <- as.numeric(names(cooksd)[(cooksd > (4/(sample_size-para_size)))])
influential <- c(86,221)
bodyfat_2 <- bodyfat[-c(eyeballing_cleanup,influential),-2]
summary(model_2 <- lm(BODYFAT ~ ., data=bodyfat_2))

cooksd <- cooks.distance(model_2)
sample_size <- nrow(bodyfat_2)
para_size <- ncol(bodyfat_2)-1
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cook\'s distance", ylab = 'Cook\'s Distance')  # plot cook's distance
abline(h = 4/(sample_size-para_size), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/(sample_size-para_size), names(cooksd),""), col="red")  # add labels

layout(matrix(1:4, ncol=2))
plot(model_2)

library(car)
outlierTest(model_2)

lm.reg.dffits = dffits(model_2)
plot(lm.reg.dffits, type = "h", ylab = "DFFITS", ylim = c(-2.5,4.5),cex.lab=1.5) 
abline(h = c(-1,-2*sqrt(4/19), 0, 2*sqrt(4/19), 1), lty = 2)

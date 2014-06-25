library(dismo)
#p<-raster('TerClass.asc')
#elev<-raster('subbathy.tif')

p1 <- aggregate(p,4,fun=modal)

depth <- aggregate(elev,4)

############################################################################
# Response data processing

dataset<-read.csv('respdata1.csv')
coordinates(dataset)<-c(3,4)

# overlay onto grid and assign response values to grid cells (go from point
# to polygon data)

dataset1 <- as(dataset,"SpatialPoints")
rasterset<-rasterize(dataset1,depth)

#rasterset1<-rasterize(dataset,depth,'Lpertusa')

k<-which(!is.na(rasterset1[]))

# split into training and testing sets


###########################################################################
# Predictors

predictors<-brick(p1,depth)
names(predictors)<-c('terrclass','depth')

v <- data.frame(cbind(rasterset1[k], predictors[k]))

aggregate(v$terrclass, list(value=v$V1), length)

aggregate(v$V1, list(class=v$terrclass), max) # this tells me if the max
  # value of presence is 0, indicating only absences for that class



############################################################################
# Fitting the model

lpertusa.tc5.lr01 <- gbm.step(data=v, gbm.x = 2:3, gbm.y = 1,
                            family = "bernoulli",
                            tree.complexity = 5, learning.rate = 0.01,
                            bag.fraction = 0.5)

lpertusa.tc5.lr005 <- gbm.step(data=v, 
                             gbm.x = 2:3,
                             gbm.y = 1,
                             family = "bernoulli",
                             tree.complexity = 5,
                             learning.rate = 0.005,
                             bag.fraction = 0.5)

lpertusa.simp <- gbm.simplify(lpertusa.tc5.lr005, n.drops = 1)

par(mfrow=c(3,4))
gbm.plot(lpertusa.tc5.lr005, n.plots=12, write.title = F) # adjust this...

gbm.plot.fits(lpertusa.tc5.lr005) # adjust this...

## code to interrogate and visualize interactions here, or otherwise interpret models

############################################################################
# Making predictions (NOT TESTED YET!!!)

eval.data <- read.csv("eval.data.csv", as.is=T) # sites for which
  # predictions will be made

eval.data$Method <- factor(eval.data$Method,
                           levels = levels(model.data$Method))

preds <- predict.gbm(angaus.tc5.lr005, eval.data,
                     n.trees=angaus.tc5.lr005$gbm.call$best.trees,
                     type="response")


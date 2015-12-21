install.packages("rpart")
install.packages("C50")
install.packages("randomForest")
this.file <- parent.frame(2)$ofile
this.dir <- dirname(this.file)
setwd(this.dir)
directory = list.dirs('./agaricus-lepiota.data')
mr <- read.table("./agaricus-lepiota.data", header = FALSE, sep = ",")

names(mr)<-c("class", "cshape", "csurface", "ccolor", "bruises", "odor", 
             "gattach", "gspace", "gsize", "gcolor", "sshape", "sroot", 
             "ssabove", "ssbelow", "scabove", "scbelow", "vtype", "vcolor", 
             "rnumber", "rtype", "spcolor", "popnum", "habitat")

install.packages("rpart.plot")
library(rpart.plot)
fit <- rpart(class ~ cshape + csurface + ccolor , data = mr)
prp(fit)

fit <- rpart(class ~ bruises + odor + gattach + gspace , data = mr)
prp(fit)

fit <- rpart(class ~ gsize + gcolor + sshape + sroot , data = mr)
prp(fit)

fit <- rpart(class ~ ssabove + ssbelow + scabove + scbelow , data = mr)
prp(fit)

fit <- rpart(class ~ vtype + vcolor + rnumber + rtype , data = mr)
prp(fit)

fit <- rpart(class ~ spcolor + popnum + habitat , data = mr)
prp(fit)


mr$vtype <- NULL
mr.small <- mr[,2:22]
mr.c50<-C5.0.default(x=mr.small,y=mr$class)
summary(mr.c50)


class.rf <- randomForest(class ~., mr, do.trace=10, ntree=100)
print(class.rf)
importance(class.rf)
varImpPlot(class.rf)

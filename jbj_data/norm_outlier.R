############################################################################
######################### Outlier Detection_ Z-score #######################
############################################################################

### data : 4 column data('House', 'Date', 'Hour', 'W')
names(data)[4]<-'W'
data_out<-data
data_out[which(data_out$W<0),"W"]<-NA

### 정규화 : Yeo-johnson Transformation
library(VGAM)
library(car)
library(bestNormalize)

data_trans<-yeojohnson(data_out$W,eps=0.01,standardize=TRUE)
str(data_trans)

### Z-score method(threshold = 3)
library(outliers)

outnum_z<-c(which(data_trans$x.t>3))
data_out$W[c(outnum_z)]<-NA
summary(data_out)

data_sp<-data_out


d=read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
d=d[-1]
table(d$diagnosis)
d$diagnosis=factor(d$diagnosis,levels=c("B","M"),labels=c("benign","malignant"))
round(prop.table(table(d$diagnosis)) * 100, digits = 1)
summary(d[c("radius_mean", "area_mean", "smoothness_mean")])
#normalization(min max scaling)
normalize= function(x){return ((x - min(x)) / (max(x) - min(x)))}
d_n= as.data.frame(lapply(d[2:31], normalize))
summary(d_n$area_mean)
#train and test data division
d_train=d_n[1:469, ]
d_test=d_n[470:569, ]
d_train_labels=d[1:469, 1]
d_test_labels =d[470:569, 1]
#training model on data--KNN
d_test_predictn=knn(train=d_train,test=d_test,cl=d_train_labels,k=80)
#evaluating the model
CrossTable(x = d_test_labels, y = d_test_predictn,prop.chisq=FALSE)
#improving models
#z-score
d_z=as.data.frame(scale(d[-1]))
summary(d_z$area_mean)
d_train_z= d_z[1:469, ]
d_test_z = d_z[470:569, ]
d_train_labels=d[1:469, 1]
d_test_labels=d[470:569, 1]
d_test_predictz=knn(train = d_train_z, test = d_test_z,cl =d_train_labels, k = 21)
CrossTable(x =d_test_labels,y =d_test_predictz,prop.chisq = FALSE)



#boxcox
d1=d[2:31]
d1_test_predictbcx=preProcess(d1,method=c("BoxCox"))
d1_boxcox=predict(d1_test_predictbcx,d1)
d_train_bcx= d1_boxcox[1:469, ]
d_test_bcx = d1_boxcox[470:569, ]
d_train_labels=d[1:469, 1]
d_test_labels=d[470:569, 1]
d_test_predictbcx=knn(train = d_train_bcx, test = d_test_bcx,cl =d_train_labels, k = 21)
CrossTable(x =d_test_labels,y =d_test_predictbcx,prop.chisq = FALSE)



#normalization resampling
d_n= as.data.frame(lapply(d[2:31], normalize))
d_resample= sort(sample(nrow(d_n), nrow(d_n)*.82))
train_resample = d_n[d_resample,]
test_resample = d_n[-d_resample,]
x= d[d_resample,]
y=d[-d_resample,]
d_train_label=x[1:nrow(train_resample), 1]
d_test_label =y[1:nrow(test_resample), 1]
d_test_pred=knn(train=train_resample,test=test_resample,cl=d_train_label,k=11)
CrossTable(x =d_test_label, y =d_test_pred,prop.chisq=FALSE)
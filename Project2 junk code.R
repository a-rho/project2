#clustering
pam_dat1<-Crabs%>%select(Age,Weight)
sil_width<-vector()
for(i in 2:10){  
  pam_fit <- pam(pam_dat1, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
ggplot()+geom_line(aes(x=1:10,y=sil_width))+scale_x_continuous(name="k",breaks=1:10) #k=2
swiss_pam <- swiss %>% pam(k = 2)
plot(swiss_pam, which = 2)

pam_dat1 %>% scale %>% pam(k=2) -> pam1
plot(pam1, which=2)
pamclust1<-pam_dat1 %>% mutate(cluster=as.factor(pam1$clustering))
pamclust1 %>% ggplot(aes(Age,Weight,color=cluster)) + geom_point()

pam_dat2 <- Crabs %>% select(Length, Weight)
sil_width<-vector()
for(i in 2:10){  
  pam_fit <- pam(pam_dat2, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
ggplot()+geom_line(aes(x=1:10,y=sil_width))+scale_x_continuous(name="k",breaks=1:10) #k=2

pam_dat2 %>% scale %>% pam(k=2) -> pam2
pamclust2 <- pam_dat2 %>% mutate(cluster=as.factor(pam2$clustering))
pamclust2 %>% ggplot(aes(Length, Weight, color=cluster)) + geom_point()


#linear classifier
x<- Crabs$Weight %>% na.omit()
y<-Crabs %>% na.omit() %>% mutate(Sex=ifelse(Sex=="F", 1, 0))
y<- y$Sex
accuracy <- vector()
cutoff <- seq(min(x),max(x))
for(i in cutoff){
  y_hat1 <- ifelse(x>i, 1, 0)
  accuracy[i] <- mean(y==y_hat1) 
}
max(accuracy) #0.54
cutoff[which.max(accuracy)] #35.44
y_hat<-ifelse(x>35.43942, 1,0)
mean(y==y_hat) #0.54

table(actual=y, predicted = y_hat) %>% addmargins #confusion matrix



class_diag(score = x,truth = y, positive = 1, cutoff = 35.43942)

y<- Crabs$Sex %>% na.omit()
y<- factor(y,levels = c("female","male"))

y_hat<- sample(c("female","male"), size = length(y), replace = T) %>% na.omit()
Crabs %>% select(Weight, Sex) %>% na.omit() %>% mutate(predict=y_hat) %>% head

mean(y==y_hat)
y_hat <- factor(y_hat, levels=c("malignant","benign"))
table(actual = y, predicted = y_hat)

qplot(y=accuracy)+geom_line()+scale_x_continuous(breaks=10:80) #break at x=9

#nonparametric
#knn_fit <- knn3(Sex ~ . , data=class_dat, family="binomial")


Crabs%>% ggplot(aes(Weight,Sex))+geom_point()+geom_smooth(method="lm", se=F)
fit <- lm(Sex ~ ., data=class_dat)
score <- predict(fit)
score %>% round(3)
class_diag(score,truth=class_dat$Sex, positive=1)

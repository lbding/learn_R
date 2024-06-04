#### reduce dimension

### 生成模拟数据
set.seed(1988)
library(MASS)
n<-100
Sigma<-matrix(c(9,9*0.9,9*0.92,9*1),2,2)
x<-rbind(mvrnorm(n/2,c(69,69),Sigma),
           mvrnorm(n/2,c(55,55),Sigma))
### 50对儿童双胞胎的身高；50对成人双胞胎的身高

d <- dist(x)
image(as.matrix(d))

## we need a distance based on just one dimension
# 直接只保留一个dimension
z <- x[,1]
plot(d,dist(z))
# underestimation
sd(dist(x)-dist(z)*sqrt(2))
mean(dist(x)-dist(z)*sqrt(2))

plot(d/sqrt(2),dist(z))
abline(0,1)


# 另一种方法
z<-cbind((x[,2]+x[,1])/2,x[,2]-x[,1])
plot(z[,1],z[,2])
# using the 1st dimension of z we can obtain an even better approximation
sd(dist(x)-dist(z[,1])*sqrt(2))
mean(dist(x)-dist(z[,1])*sqrt(2))

qplot(z[,1],bins=30)

cor(x[,1],x[,2])
cor(z[,1],z[,2])



#########################
######### PCA ###########
pca <- prcomp(x)
names(pca)
pca$rotation
dim(pca$x)

a <- sweep(x,2,colMeans(x))
b <- pca$x %*% t(pca$rotation)
plot(a,b)

## not just for two dimension
## but for matrices of any dimension
x <- iris[,1:4] %>% as.matrix()
d <- dist(x)
library(RColorBrewer)
image(as.matrix(d),col=brewer.pal(9,'RdBu'))
cor(x)

pca <- prcomp(x)
summary(pca)

data.frame(pca$x[,1:2],Species=iris$Species) %>% 
  ggplot(aes(PC1,PC2,color=Species)) +
  geom_point() 


## real example: mnist
pca <- prcomp(mnist$train$images)
qplot(1:ncol(pca$x),pca$sdev)

data.frame(pca$x[,1:2],label=factor(mnist$train$labels)) %>%
  sample_n(2000) %>% 
  ggplot(aes(PC1,PC2,color=label)) + 
  geom_point()

train_knn <- train(pca$x[,1:36],factor(mnist$train$labels),
                   method = 'knn', tuneGrid = data.frame(k=seq(5,21,2)))
ggplot(train_knn)
 
x_test <- sweep(mnist$test$images,2,colMeans(mnist$test$images))
x_test <- x_test %*% pca$rotation
x_test <- x_test[,1:36]
confusionMatrix(predict(train_knn,x_test),factor(mnist$test$labels))$overall['Accuracy']


#### recommendation systems
http://blog.echen.me/2011/10/24/winning-the-netflix-prize-a-summary

data('movielens')
movielens %>% as_tibble() %>% summarise(n_users=n_distinct(userId),
                                        n_movies=n_distinct(movieId))

library(caret)
set.seed(755)
test_index <- createDataPartition(y=movielens$rating,p=0.2,list = FALSE)

train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% semi_join(train_set,by='movieId') %>%
  semi_join(train_set, by='userId')

RMSE <- function(true_rate, predicted_rate) {
  sqrt(mean((true_rate-predicted_rate)^2))
}

### first try
# average
mu <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating,mu_hat)

rmse_results <- tibble(method='Just the average',
                       RMSE=naive_rmse)


# regression
# fit <- lm(rating ~ as.factor(movieId) + as.factor(userId), data=movielens)
# movie effects
movie_avgs <- train_set %>% group_by(movieId) %>%
  summarise(b_i=mean(rating-mu))

predicted_ratings <- test_set %>% left_join(movie_avgs,by='movieId') %>%
  pull(b_i) + mu
RMSE(predicted_ratings,test_set$rating)


# user effects
train_set %>% as_tibble() %>% group_by(userId) %>% 
  filter(n()>=100) %>% 
  summarize(b_u=mean(rating)) %>%
  ggplot(aes(b_u)) + geom_histogram(bins=30,color='black')


user_avgs <- train_set %>% as_tibble() %>% left_join(movie_avgs,by='movieId') %>%
  group_by(userId) %>% summarise(b_u=mean(rating-mu-b_i))

predicted_ratings <- test_set %>% left_join(movie_avgs,by='movieId') %>% 
  left_join(user_avgs,by='userId') %>% 
  mutate(pred=mu+b_i+b_u) %>% pull(pred)
RMSE(predicted_ratings,test_set$rating)
# 0.905


### regularization
test_set %>% left_join(movie_avgs,by='movieId') %>% 
  left_join(user_avgs,by='userId') %>% 
  mutate(residual=rating-mu-b_i-b_u) %>%
  arrange(desc(abs(residual))) %>% head(10)

# top 10 best movies
train_set %>% count(movieId) %>% left_join(train_set%>%select(movieId,title)%>%distinct(),by='movieId') %>%
  left_join(movie_avgs,by='movieId') %>% 
  arrange(desc(b_i)) %>% head(10)

# top 10 worst movies
movie_avgs %>% left_join(movielens%>%select(movieId,title)%>%distinct(), by='movieId') %>% 
  arrange(b_i)



## matrix factorization
train_small <- movielens %>% group_by(movieId) %>% 
  filter(n()>=50 | movieId==3252) %>% ungroup() %>%
  group_by(userId) %>% filter(n()>=50) %>% ungroup()

y <- select(train_small,userId,movieId,rating) %>% spread(movieId,rating) %>% as_tibble() %>% 
  as.matrix()

rownames(y) <- y[,1]
y <- y[,-1]
dim(y)

movie_titles <- movielens %>% select(movieId,title) %>% 
  distinct()
colnames(y) <- movie_titles$title[match(colnames(y),movie_titles$movieId)]

y <- sweep(y,2,colMeans(y,na.rm=T))
y<- sweep(y,1,rowMeans(y,na.rm=T))

m_1<-"Godfather, The"
m_2<-"Godfather: Part II, The"
p1<-qplot(y[,m_1],y[,m_2],xlab=m_1,ylab=m_2)

m_1<-"Godfather, The"
m_3<-"Goodfellas"
p2<-qplot(y[,m_1],y[,m_3],xlab=m_1,ylab=m_3)

m_4<-"You've Got Mail"
m_5<-"Sleepless in Seattle"

x<-y[,c(m_1,m_2,m_3,m_4,m_5)]
short_names<-c("Godfather","Godfather2","Goodfellas",
                 "Sleepless")
colnames(x)<-short_names
cor(x,use="pairwise.complete")

## PCA
y[is.na(y)] <- 0
pca <- prcomp(y)

dim(pca$rotation)
dim(pca$x)
dim(y)





### SVD
set.seed(1987)
n<-100
k<-8
Sigma<-64*matrix(c(1,.75,.5,.75,1,.5,.5,.5,1),3,3)
m<-MASS::mvrnorm(n,rep(0,3),Sigma)
m<-m[order(rowMeans(m),decreasing=TRUE),]
y<-m %x% matrix(rep(1,k),nrow=1)+
  matrix(rnorm(matrix(n*k*3)),n,k*3)
colnames(y)<-c(paste(rep("Math",k),1:k,sep="_"),
                 paste(rep("Science",k),1:k,sep="_"),
                 paste(rep("Arts",k),1:k,sep="_"))

my_image<-function(x,zlim=range(x),...){
  colors=rev(RColorBrewer::brewer.pal(9,"RdBu"))
  cols<-1:ncol(x)
  rows<-1:nrow(x)
  image(cols,rows,t(x[rev(rows),,drop=FALSE]),xaxt="n",yaxt="n",
        xlab="",ylab="",col=colors,zlim=zlim,...)
  abline(h=rows+0.5,v=cols+0.5)
  axis(side=1,cols,colnames(x),las=2)
}

my_image(y)

s <- svd(y)
names(s)
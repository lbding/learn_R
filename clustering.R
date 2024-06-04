#### clustering
## unsupervised learning
## algorithms: hierarchical & k-means

data("movielens")

# top 50 movies rated
top <- movielens %>% group_by(movieId) %>%
  summarise(n=n(),title=first(title)) %>% 
  top_n(50,n) %>% pull(movieId)

x <- tibble(movielens) %>% filter(movieId %in% top) %>%
  group_by(userId) %>% filter(n()>=25) %>% 
  ungroup() %>% select(title,userId,rating) %>% 
  spread(userId,rating)

row_names <- str_remove(x$title,": Episode") %>% str_trunc(20)
head(row_names)

x <- x[,-1] %>% as.matrix()
x <- sweep(x,2,colMeans(x,na.rm = T))
x <- sweep(x,1,rowMeans(x,na.rm = T))
rownames(x) <- row_names

d <- dist(x)

h <- hclust(d)
plot(h,cex=.7)

# 对聚类分组
groups <- cutree(h,k = 10)
names(groups)
names(groups)[groups==1]



### k-means
x_0 <- x
x_0[is.na(x_0)] <- 0
k <- kmeans(x_0,centers = 10)

k$cluster
groups <- k$cluster

# repeat several times and average
k <- kmeans(x_0, centers = 10, nstart = 25)


### heatmaps
heatmap(x,col=RColorBrewer::brewer.pal(11,'Spectral'))

# 去除near0variance的变量
library(matrixStats)
#nearZeroVar(x)
sds <- colSds(x,na.rm=T)
qplot(sds,bins=20)

o <- order(sds,decreasing = T)[1:25]
heatmap(x[,o], col=RColorBrewer::brewer.pal(11,'Spectral'))


https://www.codecademy.com/learn/learn-the-command-line


https://www.edx.org/course/introduction-linux-linuxfoundationx-lfs101x-1


https://www.coursera.org/learn/unix

https://www.codecademy.com/learn/learn-git

http://happygitwithr.com/
  
  
pwd
mkdir git-example
cd git-example
git clone https://github.com/rairizarry/murders.git
cd murders

echo "fist commit" >> README.txt
git add README.txt
git commit -m "First commit. Adding README.txt file just to get started"

此材料可能受版权保护。

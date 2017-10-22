# homework 3

##################
# import third-party modules
##################
library("tidyverse")
library("mclust")

##################
# question 1
##################

stock <- read.csv("c:/users/philip/schools/tamu/stat_636/homework/stock_prices.csv")
names(stock) <- tolower(names(stock))
str(stock)

# part a - perform principal components analysis
pc <- princomp(stock)
str(pc)
summary(pc)

# ~53% of the total variance in the data set is explained by pc 1
# ~27% of the total variance in the data set is explained by pc 2

screeplot(pc, type="l")

# part b - report and comment on the coefficients of the linear combo that 
# defines the first pc 
pc$loadings
apply(stock, 2, sd)

# the signs of the coefficients in the linear combination should not be of 
# importance; we can see here that the most important components are 
# shell and exxonmobil.  this indicates that oil companies are reponsible
# for more of the variation in stock prices than the financial companies.
# this pc will highlight weeks with greater fluctutations in oil stock prices
# this makes sense as the oil companies have the greatest distributional spread

# part c report and comment on the coefficients of the linear combo that 
# defines the second pc
pc$loadings
apply(stock, margin=2, fun=summary)
apply(stock, 2, sd)

# the second pc moves to the finance companies, but highlights jp morgan and 
# citibank.  this makes sense, as these companies have a standard deviation
# that is ~33% greater than that of wells fargo.  


# part d - verify that the coefficients on the first two pcs equal the 
# first two eigenvalues of the sample covariance matrix s

s <- var(stock)  
ees <- eigen(s)
ees

# principal component variances - first two components
round(pc$sdev[1:2] ** 2, 4)
# first two eigenvalues
round(ees$values[1:2], 4)


# part e - generate a scatter plot of the first two pcs

apply(pc$scores[, 1:2], 2, summary)
apply(pc$scores[, 1:2], 2, sd)

# as we would expect, the first pc is reporting higher variance

# base r plotting
# ggplot version
pc_scores <- pc$scores %>% data.frame()

p <- ggplot(pc_scores, aes(pc_scores$comp.1, pc_scores$comp.2))
p + geom_point() + 
  coord_cartesian(xlim=c(-.12, .12), ylim=c(-.12, .12)) +
  ggtitle("principal components 1 + 2 scatterplot") +
  xlab("pc 1") + 
  ylab("pc 2")

# note that pc 1 has more variance than pc 2.
# max first
pc_scores[which.max(pc_scores$comp.1), ]
# 63
# min second
pc_scores[which.min(pc_scores$comp.1), ]
# 56
stock            

# week 63 has all negative inputs; week 56 has all positive inputs.  
# given that the pc linear combination has all negative signs, it makes
# sense that all negative inputs have resulted in a large value for pc 1


##################
# question 2
##################

(d <- as.dist(matrix(c(0,3,2,5, 0, 0, 4, 1, 0, 0, 0, 7, 0, 0, 0, 0), nrow=4)))

hc_single <- hclust(d, method="single")
plot(hc_single, main="cluster dendogram - single linkage")
   
hc_complete <- hclust(d, method="complete")
plot(hc_complete, main="cluster dendogram - complete linkage")

hc_average <- hclust(d, method="average")
plot(hc_average,  main="cluster dendogram - average linkage")

hc_ward <- hclust(d, method="ward.d")
plot(hc_ward,  main="cluster dendogram - ward linkage")


##################
# question 3
##################
# import our data set
obs <- matrix(c(5, 1, -1, 3, 4, -2, 1, 1), nrow=4)
colnames(obs) <- c("x1", "x2")
rownames(obs) <- letters[1:4]
obs

k_clust <- kmeans(obs, centers=2, nstart=2)
k_clust$cluster


##################
# question 4
##################

cereal <- read.csv("c:/users/philip/schools/tamu/stat_636/homework/cereal.csv")
names(cereal) <- tolower(names(cereal))
str(cereal)

# drop the cereal name and assign it as an idex label
cereal2 <- cereal[, -1]
rownames(cereal2) <- cereal[, 1]

# create a distance matrix for our cereal
d <- dist(x=cereal2, method="euclidean")
# complete linkage hierarchical clustering
cereal_hclust <- hclust(d=d
                        , method="complete"
                        )

str(cereal_hclust)
plot(cereal_hclust)


# it looks like there are 6 natural clusters

cutree(cereal_hclust, k=6)

plot(cereal_hclust)
rect.hclust(cereal_hclust, k=3, border="blue")

plot(cereal_hclust)
rect.hclust(cereal_hclust, k=6, border="blue")

# part b - carry out kmeans clustering on the cereal data

# run the k-means
cereal_kmeans <- kmeans(cereal2, centers=3)

# create principal components
cereal_pc <- princomp(x=cereal2)

# vertically merge the pc scores with the assigned cluster from kmeans
cereal_pc_scores <- cbind(cereal_pc$scores[, 1:2]
                          , cereal_kmeans$cluster) %>% 
  data.frame()

names(cereal_pc_scores) <- c("pc1", "pc2", "kmeans_cluster")

# create a scatter plot of the first 2 pcs with overlaid clusters
p <- ggplot(cereal_pc_scores
            , aes(cereal_pc_scores$pc1
                  , cereal_pc_scores$pc2)
            )

p + geom_point(aes(color=factor(cereal_pc_scores$kmeans_cluster))) +
  coord_cartesian(xlim=c(-200, 200), ylim=c(-200, 200)) +
  ggtitle("principal components 1 + 2 with k-means overlay") +
  xlab("pc 1") + 
  ylab("pc 2") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name="k-means clusters"
                       , labels=c("1", "2", "3"))
  

# now let's look at the hierarchical clusters with the princomps
# six-cluster model
cereal_pc_scores2 <- cbind(cereal_pc$scores[, 1:2]
                          , cutree(cereal_hclust, k=6)) %>% 
  data.frame()

names(cereal_pc_scores2) <- c("pc1", "pc2", "h_cluster")

p <- ggplot(cereal_pc_scores2
            , aes(cereal_pc_scores2$pc1
                  , cereal_pc_scores2$pc2)
            )

p + geom_point(aes(color=factor(cereal_pc_scores2$h_cluster))) +
  coord_cartesian(xlim=c(-200, 200), ylim=c(-200, 200)) +
  ggtitle("principal components 1 + 2 with h-clust overlay") +
  xlab("pc 1") + 
  ylab("pc 2") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name="h-clust clusters"
                       , labels=c("1", "2", "3", "4", "5", "6"))
# now let's look at the hierarchical clusters with the princomps
# six-cluster model
cereal_pc_scores2 <- cbind(cereal_pc$scores[, 1:2]
                          , cutree(cereal_hclust, k=6)) %>% 
  data.frame()

names(cereal_pc_scores2) <- c("pc1", "pc2", "h_cluster")

p <- ggplot(cereal_pc_scores2
            , aes(cereal_pc_scores2$pc1
                  , cereal_pc_scores2$pc2)
            )

p + geom_point(aes(color=factor(cereal_pc_scores2$h_cluster))) +
  coord_cartesian(xlim=c(-200, 200), ylim=c(-200, 200)) +
  ggtitle("principal components 1 + 2 with h-clust overlay") +
  xlab("pc 1") + 
  ylab("pc 2") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name="h-clust clusters"
                       , labels=c("1", "2", "3", "4", "5", "6"))

# now let's look at the hierarchical clusters with the princomps
# three-cluster model
cereal_pc_scores2 <- cbind(cereal_pc$scores[, 1:2]
                          , cutree(cereal_hclust, k=3)) %>% 
  data.frame()

names(cereal_pc_scores2) <- c("pc1", "pc2", "h_cluster")

p <- ggplot(cereal_pc_scores2
            , aes(cereal_pc_scores2$pc1
                  , cereal_pc_scores2$pc2)
            )

p + geom_point(aes(color=factor(cereal_pc_scores2$h_cluster))) +
  coord_cartesian(xlim=c(-200, 200), ylim=c(-200, 200)) +
  ggtitle("principal components 1 + 2 with h-clust overlay") +
  xlab("pc 1") + 
  ylab("pc 2") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name="h-clust clusters"
                       , labels=c("1", "2", "3"))

# let's try out gaussian mixture modeling
library("mclust")
cereal_mclust <- mclust::mclust(cereal2)
summary(cereal_mclust)
str(cereal_mclust)

cereal_pc_scores3 <- cbind(cereal_pc$scores[, 1:2]
                          , cereal_mclust$classification)%>% 
  data.frame()

names(cereal_pc_scores3) <- c("pc1", "pc2", "mcluster")

p <- ggplot(cereal_pc_scores3
            , aes(pc1, pc2))

p + geom_point(aes(color=factor(cereal_pc_scores3$mcluster))) +
  coord_cartesian(xlim=c(-200, 200), ylim=c(-200, 200)) +
  ggtitle("principal components 1 + 2 with mixturemodel-clust overlay") +
  xlab("pc 1") + 
  ylab("pc 2") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name="m-clust clusters"
                       , labels=c("1", "2", "3"))
 


##################
# question 5
##################

x <- as.matrix(read.table("c:/users/philip/schools/tamu/stat_636/homework/t12-8(1).dat"
           , header=F)
              )
pot_type <- letters[1:4]
pot_site <- paste("p", 0:6, sep="_")
rownames(x) <- pot_site
colnames(x) <- pot_type
x

d <- dist(x, method="euclidean")

# conduct the multidimensional scaling
pot_md <- cmdscale(d=d, k=2)
str(pot_md)

x <- pot_md[, 1]
y <- pot_md[, 2]

plot(x, y
     , main="pottery mds"
     , xlab="coordinate 1"
     , ylab="coordinate 2"
     , type="n"
     )
mtext("site names displayed")
text(x, y, labels=row.names(x))

# part b - construct a biplot and interpret
# first generate the principal components

pot_pc <- princomp(x, cor=TRUE)

biplot(pot_pc
       , main="biplot for pottery data's principal components"
       , col="black"
       )

# the plots are showing mostly similar arrangements.  it appears that 
# the biplot is offering more info, however, as it can show 
# the variables that lead to something being placed where it is
# homework 3

library("tidyverse")

##################
# Question 1
##################


stock <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_636/homework/stock_prices.csv")
names(stock) <- tolower(names(stock))
str(stock)

# Part A - perform principal components analysis
pc <- princomp(stock)
str(pc)
summary(pc)

# ~53% of the total variance in the data set is explained by PC 1
# ~27% of the total variance in the data set is explained by PC 2

screeplot(pc, type="l")

# Part B - report and comment on the coefficients of the linear combo that 
# defines the first PC 
pc$loadings

# the signs of the coefficients in the linear combination should not be of 
# importance; we can see here that the most important components are 
# shell and exxonmobil.  This indicates that oil companies are reponsible
# for more of the variation in stock prices than the financial companies.
# This PC will highlight weeks with greater fluctutations in oil stock prices
# this makes sense as the oil companies have the greatest distributional spread

# part C report and comment on the coefficients of the linear combo that 
# defines the second PC
pc$loadings
apply(stock, MARGIN=2, FUN=summary)
apply(stock, 2, sd)

# The second PC moves to the finance companies, but highlights jp morgan and 
# citibank.  This makes sense, as these companies have a standard deviation
# that is ~33% greater than that of wells fargo.  


# part D - verify that the coefficients on the first two PCs equal the 
# first two eigenvalues of the sample covariance matrix S

S <- var(stock)  
eeS <- eigen(S)
eeS

round(pc$sdev[1:2] ** 2, 4)
round(eeS$values[1:2], 4)


# part E - generate a scatter plot of the first two PCs

apply(pc$scores[, 1:2], 2, summary)

# base R plotting
#plot(pc$scores[,1], pc$scores[,2])

# ggplot version
pc_scores <- pc$scores %>% data.frame()

p <- ggplot(pc_scores, aes(pc_scores$Comp.1, pc_scores$Comp.2))
p + geom_point() + 
  coord_cartesian(xlim=c(-.12, .12), ylim=c(-.12, .12)) +
  ggtitle("Principal Components 1 + 2 Scatterplot") +
  xlab("PC 1") + 
  ylab("PC 2")

# note that PC 1 has more variance than PC 2.
# max first
pc_scores[which.max(pc_scores$Comp.1), ]
# 63
# min second
pc_scores[which.min(pc_scores$Comp.1), ]
# 56
stock            

# week 63 has all negative inputs; week 56 has all positive inputs.  
# Given that the PC linear combination has all negative signs, it makes
# sense that all negative inputs have resulted in a large value for PC 1


##################
# Question 2
##################

D <- as.dist(matrix(c(0,3,2,5, 0, 0, 4, 1, 0, 0, 0, 7, 0, 0, 0, 0), nrow=4))

hc_single <- hclust(D, method="single")
plot(hc_single, main="Cluster Dendogram - Single Linkage")
   
hc_complete <- hclust(D, method="complete")
plot(hc_complete, main="Cluster Dendogram - Complete Linkage")

hc_average <- hclust(D, method="average")
plot(hc_average,  main="Cluster Dendogram - Average Linkage")

hc_ward <- hclust(D, method="ward.D")
plot(hc_ward,  main="Cluster Dendogram - Ward Linkage")


##################
# Question 3
##################

head(iris)
k_clust <- kmeans(x=iris[, 3:4], centers=3, nstart=20)
str(k_clust)

table(k_clust$cluster)

iris2 <- cbind(iris, k_clust$cluster)
names(iris2)[6] <- c("cluster")
table(iris2$cluster, iris2$Species)

boxplot(iris2$Petal.Length ~ iris2$cluster)
boxplot(iris2$Petal.Width ~ iris2$cluster)

# import our data set
obs <- matrix(c(5, 1, -1, 3, 4, -2, 1, 1), nrow=4)
colnames(obs) <- c("x1", "x2")
rownames(obs) <- LETTERS[1:4]
obs

kmeans(obs, centers=c("A", "B"))

k_clust <- kmeans(obs, centers=2, nstart=2)
k_clust$cluster


##################
# Question 4
##################

cereal <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_636/homework/cereal.csv")
names(cereal) <- tolower(names(cereal))
str(cereal)

cereal2 <- cereal[, -1]
rownames(cereal2) <- cereal[, 1]


D <- dist(x=cereal2, method="euclidean")
cereal_hclust <- hclust(d=D
                        , method="complete"
                        )

str(cereal_hclust)
plot(cereal_hclust)


# it looks like there are 6 natural clusters

help("cutree")
cutree(cereal_hclust, k=6)

plot(cereal_hclust)
rect.hclust(cereal_hclust, k=6, border="blue")


# part B - carry out kmeans clustering on the cereal data


cereal_kmeans <- kmeans(cereal2, centers=3)

cereal_pc <- princomp(x=cereal2)

cereal_pc_scores <- cbind(cereal_pc$scores[, 1:2]
                          , cereal_kmeans$cluster) %>% 
  data.frame()
names(cereal_pc_scores) <- c("pc1", "pc2", "kmeans_cluster")



p <- ggplot(cereal_pc_scores
            , aes(cereal_pc_scores$pc1
                  , cereal_pc_scores$pc2)
            )

p + geom_point(aes(color=factor(cereal_pc_scores$kmeans_cluster))) +
  coord_cartesian(xlim=c(-200, 200), ylim=c(-200, 200)) +
  ggtitle("Principal Components 1 + 2 with K-Means Overlay") +
  xlab("PC 1") + 
  ylab("PC 2") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name="K-Means Clusters"
                       , labels=c("1", "2", "3"))
  

# now let's look at the hierarchical clusters with the princomps
cutree(cereal_hclust, k=6)

cereal_pc_scores2 <- cbind(cereal_pc$scores[, 1:2]
                          , cutree(cereal_hclust, k=6)) %>% 
  data.frame()

names(cereal_pc_scores2) <- c("pc1", "pc2", "h_cluster")

head(cereal_pc_scores2)


p <- ggplot(cereal_pc_scores2
            , aes(cereal_pc_scores2$pc1
                  , cereal_pc_scores2$pc2)
            )

p + geom_point(aes(color=factor(cereal_pc_scores2$h_cluster))) +
  coord_cartesian(xlim=c(-200, 200), ylim=c(-200, 200)) +
  ggtitle("Principal Components 1 + 2 with H-Clust Overlay") +
  xlab("PC 1") + 
  ylab("PC 2") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name="H-Clust Clusters"
                       , labels=c("1", "2", "3"))


# let's try out Gaussian Mixture Modeling
library("mclust")
cereal_mclust <- mclust::Mclust(cereal2)

cereal_pc_scores3 <- cbind(cereal_pc$scores[, 1:2]
                          , cereal_mclust$classification)%>% 
  data.frame()

names(cereal_pc_scores3) <- c("pc1", "pc2", "mcluster")

p <- ggplot(cereal_pc_scores3
            , aes(pc1, pc2))

p + geom_point(aes(color=factor(cereal_pc_scores3$mcluster))) +
  coord_cartesian(xlim=c(-200, 200), ylim=c(-200, 200)) +
  ggtitle("Principal Components 1 + 2 with MixtureModel-Clust Overlay") +
  xlab("PC 1") + 
  ylab("PC 2") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name="M-Clust Clusters"
                       , labels=c("1", "2", "3"))
 


##################
# Question 5
##################

X <- as.matrix(read.table("C:/Users/Philip/Schools/TAMU/STAT_636/homework/T12-8(1).DAT"
           , header=F)
              )
pot_type <- LETTERS[1:4]
pot_site <- paste("P", 0:6, sep="_")
rownames(X) <- pot_site
colnames(X) <- pot_type

D <- dist(X, method="euclidean")
pot_md <- cmdscale(d=D, k=2)
str(pot_md)

x <- pot_md[, 1]
y <- pot_md[, 2]

plot(x, y
     , main="Pottery MDS"
     , xlab="Coordinate 1"
     , ylab="Coordinate 2"
     , type="n"
     )
mtext("Site Names Displayed")
text(x, y, labels=row.names(X))

# part B - construct a biplot and interpret
pot_pc <- princomp(X)

biplot(pot_pc, main="BiPlot for Pottery Data's Principal Components")

# the plots are showing mostly similar arrangements.  It appears that 
# the biplot is offering more info, however, as it can show 
# the variables that lead to something being placed where it is
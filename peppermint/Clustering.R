### Clustering
library(data.table)
library(dplyr)
library(mclust)

score <- fread("~/DataCampus/dat/score.csv")
names(score)[1] <- "PERSON_ID"
dat <- left_join(yy, score, by = "PERSON_ID")

for (i in c(2,4, 27:48)) {
  dat[,i] <- as.numeric(dat[,i])
}

quantile(score$tot, 0.177)
quantile(score$tot, 0.355)
quantile(score$tot, 0.566)

dat_c[dat_c$PERSON_ID==65993278,]
colnames(dat)

A <- dat[,-c(1, 5:10, 27, 32, 34:36, 38:42, 49:55)]

mc_BIC <- mclustBIC(A)

mc <- Mclust(A, 3)
dat_c <- data.frame(dat, cluster = mc$classification)
dat_c$sick <- rowSums(dat_c[,c(4, 6:10)])
dat_c %>% group_by(cluster) %>% summarise(mean(sick))

write.csv(dat_c, file = "clustering.csv", row.names = F)

dat_c %>%
  select(bp, dia, obes, hepa, lip, kid, tot, cluster, sick) %>%
  group_by(cluster)%>%
  summarise(mean(tot), mean(bp), mean(dia), mean(obes), mean(hepa), mean(lip), mean(kid))

quantile(dat_c$tot, seq(0, 10)*0.1)
quantile(dat_c$tot)

dt <- dat_c
dt$cluster <- factor(dt$cluster)
dt$t <- factor(ifelse(dt$tot>72.7, "Normal", ifelse(dt$tot<66, "High Risk", "Low Risk")))
dt$q <- factor(ifelse(dt$tot>75.84, "Normal", ifelse(dt$tot<67.68, "High Risk", "Low Risk")))
table(dt$t, dt$q)
confusionMatrix(dt$q, dt$t, dnn = c("Quantile","Clustering"))

data_frame(score = dat_c$tot) %>%
  ggplot(., aes(score)) + 
  geom_density(color="white", fill="Light Pink 1", alpha=0.75) + 
  scale_x_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=25)) +
  geom_vline(aes(xintercept=66.0),
             color="Navy Blue", linetype="dashed", size=1.5)+
  geom_vline(aes(xintercept=72.7),
             color="Navy Blue", linetype="dashed", size=1.5)+
  geom_vline(aes(xintercept=67.68),
             color="Fire Brick", linetype="solid", size=1.5)+
  geom_vline(aes(xintercept=75.84),
             color="Fire Brick", linetype="solid", size=1.5)

dat_c[dat_c$PERSON_ID==10735652,]

## PCA
pca1 <- prcomp(A)
pca1$rotation
plot(pca1, type="l")
summary(pca1)

mcdata <-  data.frame(pca1$x, cluster=as.factor(mc$classification), tot=dat$tot)

ggplot(aes(x = PC1, y = PC2), data = mcdata) +
  geom_point(aes(color = cluster), size=0.5)


# ## tsne
# library("Rtsne")
# tsne_obj <- Rtsne(A)
# tsne_dat <- tsne_obj$Y %>% 
#   data.frame() %>% 
#   setNames(c("X", "Y")) %>% 
#   mutate(cluster = factor(mc$classification),
#          name = dat_c$PERSON_ID)
# 
# ggplot(aes(x = X, y = Y), data = tsne_dat) +
#   geom_point(aes(color = cluster), size=0.5)


# library(plotly) #plotly 패키지 로드
# p <- plot_ly(mcdata, x = pca1$x[,1], y = pca1$x[,2], z = pca1$x[,3],
#              marker = list(color = mcdata$tot, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE), alpha = 0.5) %>%
# add_markers() %>%
# layout(scene = list(xaxis = list(title = 'PC1'), #x축 제목설정
# yaxis = list(title = 'PC2'),  #y축 제목설정
# zaxis = list(title = 'PC3'))) #z축 제목설정
# p

# library(ggfortify)
# autoplot(prcomp(A), data = dat_c, colour = 'tot', size=0.5)
# 
# colors <- c("#abe84f", "#fefa50", "#f81410")
# colors <- colors[as.numeric(mcdata$cluster)]
# s3d <- scatterplot3d::scatterplot3d(mcdata[,1:3], pch = '.', color=colors)
# legend(s3d$xyz.convert(80, 3, 4.5), legend = levels(mcdata$cluster),
#        col = c("#abe84f", "#fefa50", "#f81410"), pch = 16)


library(plot3D)
attach(mcdata)
scatter3D(PC1, PC2, PC3, bty = "g",
          col.var = as.integer(mcdata$cluster),
          col = ramp.col(c("#78e84f", "#fefa50", "#f81410")),
          colkey = list(at = c(1,3,5), side=1, length = 0.15, width = 0.3,
                        labels = c("bad", "normal", "good")),
          main = "GMM Clustering", xlab = "PC1",
          ylab ="PC2", zlab = "PC3",
          ticktype = "simple", phi=0, cex=0.1)

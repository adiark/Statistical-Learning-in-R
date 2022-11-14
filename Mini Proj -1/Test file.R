library(knitr)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(reshape2)

setwd('C:\\Users\\Adi\\Desktop\\Fall-22 Study Material\\STT 810\\Mini Proj -1')

nndb <- read.csv(file = "nndb_flat.csv",header =TRUE,  stringsAsFactors = TRUE,encoding = 'UTF-8', na.strings = "" )
nndb <- as.data.frame(nndb)
head(nndb,5)

col_names <-  names(nndb)
dimensions <- dim(nndb)
paste(dimensions)
paste(col_names)


str(nndb)

summary(nndb)

typeof(nndb)

colSums(is.na(nndb))

cor_data <- rcorr(round(as.matrix(nndb[,8:ncol(nndb)]),2))
cor_data

cor_data_clean <- cor(nndb[,8:30])

melted_cormat <- melt(data = cor_data_clean)

fig = ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white"
  ) +
  xlab(" ") + ylab(" ")+
  labs(title = "Heatmap of Correlation values in NNDB Data Set")+
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = round(value,2)), color = "Black", size = 2)+
  coord_fixed() +
  guides(fill = guide_colourbar(title = "Pearson Cor.",
                                barwidth = 0.5,
                                barheight = 20))+
  theme(axis.text.x = element_text(angle = 70, margin = margin(t = 25)))

fig

ggplot(data = nndb, aes(x = Energy_kcal, y = Fat_g)) + 
  geom_point(size = 2)+ 
  labs(x = "Energy_Kcal", y = "Fat_g", title = "Energy vs Fat")

ggplot(data = nndb, aes(x = Riboflavin_mg, y = Niacin_mg)) + 
  geom_point(size = 2)+ 
  labs(x = "Energy_Kcal", y = "Fat_g", title = "Energy vs Fat")


proximates = subset(nndb, select=c("Protein_g","Fat_g","Carb_g",
                                  "Sugar_g","Fiber_g"))

melt_proximates = melt(proximates)

# Figure 1: Boxplots of proximates in all foods

ggplot(data=melt_proximates) +
  geom_boxplot(aes(x=variable, y=value)) +
  facet_wrap(~variable, scale="free", ncol=5) +
  labs(x="Proximates", y="Values")

rownames(nndb)[order(-nndb$`Protein_g`)[1:5]]

minerals = subset(nndb, select=c("Calcium_mg","Copper_mcg", "Iron_mg", "Magnesium_mg", "Phosphorus_mg", "Zinc_mg", "Manganese_mg", "Selenium_mcg"))

melt_minerals = melt(minerals)

# Figure 2: Boxplots of minerals in all foods

ggplot(data=melt_minerals) +
  geom_boxplot(aes(x=variable, y=value)) +
  facet_wrap(~variable, scale="free", ncol=4) +
  labs(x="Minerals", y="Values")


vitamin = subset(nndb, select=c("VitA_mcg","VitB6_mg","VitB12_mcg", "VitC_mg", "VitE_mg", "Folate_mcg","Niacin_mg","Riboflavin_mg","Thiamin_mg"))

melt_vitamin = melt(vitamin)


ggplot(data=melt_vitamin) +
  geom_boxplot(aes(x=variable, y=value)) +
  facet_wrap(~variable, scale="free", ncol=5) +
  labs(x="Vitamins", y="Values")




pr.out = prcomp(nndb[,9:30], center=TRUE, scale=TRUE)  # With center=TRUE, scale=TRUE
pr.out$x = -pr.out$x
pr.out$rotation = -pr.out$rotation

pve = pr.out$sdev^2 / length(pr.out$sdev)  # Proportion of variance explained


# Figure 4: Proportion of variance explained and their cumulative sum from PCA

p1 = ggplot() +
  geom_line(aes(x=c(1:length(pr.out$sdev)), y=pve)) +
  geom_point(aes(x=c(1:length(pr.out$sdev)), y=pve), size=3) +
  geom_hline(yintercept=pve[7], color='red') +
  labs(x="Principal component", y="Proportion of variance explained")

p2 = ggplot() +
  geom_line(aes(x=c(1:length(pr.out$sdev)), y=cumsum(pve))) +
  geom_point(aes(x=c(1:length(pr.out$sdev)), y=cumsum(pve)), size=3) +
  geom_hline(yintercept=cumsum(pve)[7], color='red') +
  annotate("text", x=20, y=0.59, label=paste(round(cumsum(pve)[7],3))) +
  labs(x="Principal component", y="Cumulative proportion of variance explained")

grid.arrange(p1, p2, ncol=2)


###
### Heatmap
###

melt_pr.out = melt(pr.out$rotation[,c(1:20)])
colnames(melt_pr.out) = c("Nutrient", "PC", "Value")

# Figure 5: Heatmap of nutrients and first 20 principal components

ggplot(data=melt_pr.out) +
  geom_tile(aes(x=PC, y=Nutrient, fill=Value)) +
  scale_fill_gradient2(low='blue', mid='white', high='red', midpoint=0) +
  labs(x="Principal component")

library(reshape2)

melt_pr.out1 = melt(pr.out$rotation[,c(1:2)])
colnames(melt_pr.out1) = c("Nutrient", "PC", "Value")

# Figure 6: Full spectrum of the nutrient loadings in PC1 and PC2

ggplot(data=melt_pr.out1) +
  geom_col(aes(x=Nutrient, y=Value)) +
  facet_wrap(~PC, ncol=1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Nutrients", y="Loading")


pve.mat = matrix(rep(pve, each = 22), nrow=length(pve))

nutrient.impact = apply(pr.out$rotation[,c(1:22)]^2 * pve.mat, 1, sum)
sum(nutrient.impact)  # Should equal 1

###
### Selection of nutrients
###

pve.mat = matrix(rep(pve, each = 7), nrow=length(pve))
nutrient.impact = apply(pr.out$rotation[,c(1:7)]^2 * pve.mat, 1, sum)
melt_NI = melt(nutrient.impact)

# Figure 7: Contribution of nutrients on the first 7 prinicpal components

ggplot(data=melt_NI) +
  geom_col(aes(x=reorder(rownames(melt_NI), -value), y=value)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Nutrients", y="Variable importance")

top6 = rownames(melt_NI)[order(-melt_NI$value)[1:7]]
top6

###
### Biplot
###

slopes = rep(NA, 6)

for(s in c(1:6)) {
  slopes[s] = pr.out$rotation[top6[s],2] / pr.out$rotation[top6[s],1]
}

# Figure 8: Biplot of 6 most significant nutrients impacting the first 6 principal components

ggplot() +
  geom_point(aes(x=pr.out$x[,1], y=pr.out$x[,2])) +
  geom_segment(aes(x=0, y=0, xend=-5, yend=slopes[1]*(-5)), 
               arrow = arrow(length = unit(0.03, "npc")), color="red") +
  annotate("text", x=-4, y=slopes[1]*(-5)+1, label=top6[1], color="red") +
  geom_segment(aes(x=0, y=0, xend=10, yend=slopes[2]*10), 
               arrow = arrow(length = unit(0.03, "npc")), color="red") +
  annotate("text", x=13, y=slopes[2]*10, label=top6[2], color="red") +
  geom_segment(aes(x=0, y=0, xend=12, yend=slopes[3]*12), 
               arrow = arrow(length = unit(0.03, "npc")), color="red") +
  annotate("text", x=12, y=slopes[3]*12-1, label=top6[3], color="red") +
  geom_segment(aes(x=0, y=0, xend=15, yend=slopes[4]*15), 
               arrow = arrow(length = unit(0.03, "npc")), color="red") +
  annotate("text", x=17, y=slopes[4]*15+1, label=top6[4], color="red") +
  geom_segment(aes(x=0, y=0, xend=3, yend=slopes[5]*3), 
               arrow = arrow(length = unit(0.03, "npc")), color="red") +
  annotate("text", x=3, y=slopes[5]*3-1, label=top6[5], color="red") +
  geom_segment(aes(x=0, y=0, xend=30, yend=slopes[6]*30), 
               arrow = arrow(length = unit(0.03, "npc")), color="red") +
  annotate("text", x=30, y=slopes[6]*30+2, label=top6[6], color="red") +
  labs(x="PC1", y="PC2")


sim.crit = 0.4
similar = list()

for(j in c(1:22)) {
  nutj = rownames(pr.out$rotation)[j]
  
  for(i in c(1:22)) {
    nuti = rownames(pr.out$rotation)[i]
    dist.scores = sum(abs(pr.out$rotation[j,c(1:7)] - pr.out$rotation[i,c(1:7)]))
    
    if ((dist.scores <= sim.crit) && (dist.scores != 0)) {
      similar[[nutj]] = append(similar[[nutj]], nuti)
    }
  }
}

for(n in c(1:length(similar))) {
  cat(names(similar[n]),":", similar[[n]],"\n")
}

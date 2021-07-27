library(tidyverse)
library(nflfastR)
library(ggimage)
library(ggrepel)

##getting in data and adding new variables? not sure if 8,9 are necessary
pbp <- nflfastR::load_pbp(2020)
pbp$yacprop = pbp$yards_after_catch/pbp$yards_gained
pbp$airyrdsoverneed = pbp$air_yards - pbp$ydstogo

##sorting through to get target variables
##can this be done in one chunk or is splitting more efficient?
passing <- pbp %>%
   group_by(posteam) %>%
   filter(play_type == "pass" & season_type == "REG") %>%
   filter(wp > .20, wp < .80, half_seconds_remaining > 120) %>%
   filter(yards_gained != 0) %>%
   summarize(avg.yacprop = mean(yacprop, na.rm = T),
             avg.ayoverneed = mean(airyrdsoverneed, na.rm = T))

running <- pbp %>%
   group_by(posteam) %>%
   filter(play_type == "run" & season_type == "REG") %>%
   filter(wp > .20, wp < .80, half_seconds_remaining > 120) %>%
   summarize(total = n(),
             intrun = (sum((run_location == "middle" | run_gap == "guard"), 
                           na.rm = T))/total)
             
rpsplit <- pbp %>%
   group_by(posteam) %>%
   filter((play_type == "run" | play_type == "pass") & season_type == "REG") %>%
   filter(wp > .20, wp < .80, half_seconds_remaining > 120) %>%
   summarize(total = n(),
             rpsplit = sum(rush)/total)

data <- cbind.data.frame(passing, running[,-(1:2)], rpsplit[,-(1:2)])

##finding optimal clustering number, maybe should have been 3
PCA <- prcomp(data[,-1], scale = T)
twss <- numeric(10)
for(i in 1:10){
   twss[i] <- kmeans(scale(data[,-1]), centers = i, nstart = 25)$tot.withinss
}
plot(1:10, twss, main = "Optimal Cluster No.", 
     xlab = "No. of Cluster", ylab = "Total SS within Clusters", type = "b")

clustering <- kmeans(scale(data[,-1]), centers = 4, nstart = 25)

##plotting the PCs, clusters look accurate to knowledge
ggplot(data = as.data.frame(PCA$x)) +
   geom_point(mapping = aes(PCA$x[,1], PCA$x[,2]),
              colour = clustering$cluster) +
   xlab("Principle Component 1") +
   ylab("Principle Component 2") +
   ggtitle("Offensive Clustering") +
   geom_text_repel(aes(x = PCA$x[,1],y = PCA$x[,2], 
                       label = data[,1]))
ggsave("offenseclustering.png", width = 8, height = 4, dpi = 300)
   


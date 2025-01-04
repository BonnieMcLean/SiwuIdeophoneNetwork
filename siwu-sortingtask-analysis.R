# Packages we need
#install.packages("scatterplot3d", repos="http://R-Forge.R-project.org")
library(scatterplot3d)
library(dplyr)

# Read in raw data 
data = read.csv("siwu-sortingtask-data.csv",encoding="UTF-8")
data$ideophone <- colnames(data)

# We just focus on the haptic touch ideophones for this toy visualisation

touch_ideophones <- c("wosoroo",
                      "safaraa",
                      "dekperee",
                      "sinisini",
                      "polopolo",
                      "fiefie",
                      "doboroo",
                      "fuefue",
                      "wurufuu")

use_dat <- data %>%
  arrange(ideophone) %>%
  filter(ideophone %in% touch_ideophones) %>%
  select(all_of(sort(touch_ideophones)))

# Convert matrix to distance object
m.dist = as.dist(use_dat)


# Carry out agglomerative cluster analysis
m.hca = hclust(m.dist,method="average")

# Plot cluster dendrogram
plot(m.hca)

# Create the edges for the Shiny app visualisation from the cluster analysis data

pairs <- as.data.frame(m.hca$merge)
labels <- m.hca$labels
pairs <- pairs %>% 
  rowwise() %>%
  ### labels <0 are labels for actual ideophones (end nodes), labels >0 are for internal nodes
  mutate(V1_ideo = ifelse(V1 < 0, labels[-V1], as.character(V1)))%>%
  mutate(V2_ideo = ifelse(V2 < 0, labels[-V2], as.character(V2)))
  
node_labels <- data.frame(node_id = c(1:max(pairs$V1, pairs$V2)))

## Make some names to label the internal nodes. 
## If you look at the pairs dataframe, each row corresponds to one internal node, 
## which is the internal node joining whatever is in V1_ideo and V2_ideo, e.g. ROUGH
## is the name for the node joining safaraa and wosoroo in row 1 of pairs
nodes <- c("rough","smooth","soft","surface texture","malleable","grainy","touch")

node_labels$node_label <- nodes

pairs <- pairs %>%
  rowwise() %>%
  mutate(V1_ideo = ifelse(V1 > 0, nodes[V1], V1_ideo),
         V2_ideo = ifelse(V2 > 0, nodes[V2], V2_ideo))

## Here you need to add a label for whatever you want to call the topmost node
pairs$nodes <- c(nodes, "haptic touch")

## The edge list is what you need as input for visNetwork in the shiny app
edge_list <- data.frame(from=c(pairs$nodes,pairs$nodes),to=c(pairs$V1_ideo,pairs$V2_ideo))

write.csv(edge_list,"Figure11_edges.csv",row.names=FALSE)


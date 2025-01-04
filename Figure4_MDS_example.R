library(plotly)

data = read.csv("siwu-sortingtask-data.csv",encoding="UTF-8")
data$ideophone <- colnames(data)

# For this toy visualisation, to simplify it we just select a few key ideophones to show
mds_ids <- c("tagbaraa","sodzoloo","giligili","miomio","doboroo","wosoroo","fuefue","wurufuu","fiefie","meremere","tagbaraa","fututu","gelegele")
use_dat <- data %>%
  arrange(ideophone) %>%
  filter(ideophone %in% mds_ids) %>%
  select(all_of(sort(mds_ids)))

# Convert matrix to distance object
m.dist = as.dist(use_dat)

# Reduce to three dimensions
m.mds = cmdscale(m.dist, k =3)

# Assign names to the result vectors
Dim1 <- m.mds[,1]
Dim2 <- m.mds[,2]
Dim3 <- m.mds[,3]

## Make the 3d scatterplot with plotly
plot_ly(x=m.mds[,1],y=m.mds[,2],z=m.mds[,3],text=row.names(m.mds),type="scatter3d",mode="text")


### Code for static, non-plotly plots

# Simple 2D plot
plot(Dim1, Dim2, type="n", xlab="", ylab="", main="cmdscale(m.dist)")
segments(-1500, -0, 1500, 0, lty="dotted")
segments(0, -1500, 0, 1500, lty="dotted")
text(Dim1, Dim2, plotnames, cex=0.8, col="red")

# Simple 3D scatterplot
scatterplot3d(m.mds, color="dark blue", pch=20, main="MDS", sub="3D solution", grid=TRUE, box=TRUE, type="p")

# Getting the names on the scatterplot
s3d <- scatterplot3d(m.mds, color="dark blue", pch=20, main="MDS", sub="3D solution", grid=TRUE, box=TRUE, type="p",angle=45)
text(s3d$xyz.convert(Dim1, Dim2, Dim3), label = colnames(use_dat), pos = 4)

# Try divisive clustering:
# Load library, then feed the output of diana into pltree() to plot dendrogram
library(cluster)
pltree(diana(m.dist))

# Check out to which clusters the variables are assigned
# For number of clusters = 10
cutree(diana(m.dist), 10)


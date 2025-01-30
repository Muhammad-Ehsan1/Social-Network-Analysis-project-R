##load library
library(igraph)

##Import dataset
data <- read.csv('https://raw.githubusercontent.com/bkrai/R-files-from-YouTube/main/networkdata.csv',header=T)
data
y <- data.frame(data$first, data$second)
y

##create network
net <- graph.data.frame(y, directed=T)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

##create Histogram
hist(V(net)$degree,col = "green")

##Network diagram
set.seed(222)
plot(net,vertex.color='green',vertex.size=10,edge.arrow.size=0.1,vertex.label.cex=0.8)

##Highlighting degrees & layouts
plot(net,vertex.color = rainbow(52),vertex.size = V(net)$degree*0.4,edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)

# Hub and authorities
hs <- hub_score(net)$vector
as <- authority.score(net)$vector
par(mfrow=c(1,2))
set.seed(123)
plot(net,vertex.size=hs*30,main = 'Hubs',vertex.color = rainbow(52),edge.arrow.size=0.1,
     layout = layout.kamada.kawai)
plot(net,vertex.size=as*30,main = 'Authorities',vertex.color = rainbow(52),edge.arrow.size=0.1,
     layout = layout.kamada.kawai)
par(mfrow=c(1,1))

# Community detection
net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)
plot(cnet,net,vertex.size=10,vertex.label.cex=0.8)





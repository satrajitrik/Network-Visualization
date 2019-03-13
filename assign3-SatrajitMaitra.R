# NAME: SATRAJIT MAITRA
# ASU ID: 1215097780

library(igraph)
library(scales)

ga.data <- read.csv('modified_email.csv', header=TRUE) 
adj_matrix <- get.adjacency(graph.edgelist(as.matrix(ga.data), directed=TRUE))

g <- graph.adjacency(adj_matrix, mode="directed", weighted=TRUE)

wc <- walktrap.community(g)
modularity(wc)
members <- membership(wc)

# Degree Centrality
centralization.degree(g)

# Betweenness Centrality
centralization.betweenness(g)

# Eigenvector Centrality
eigen_centrality(g)
eig <- eigen_centrality(g)$vector

# Graph Manipulation
E(g)$width <- E(g)$weight / 3
E(g)$arrow.size <- 0.5
E(g)$arrow.width <- 1
V(g)$size <- eig * 50
V(g)$label.font <- 4
V(g)$label.color <- 'black'
fine = 1000
pal = colorRampPalette(c('yellow','dark green'))
graphCol = pal(fine)[as.numeric(cut(eig,breaks = fine))]

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:27, direction=-1, start=0)
dist_labels = c(3,0,2.5,2,1.5,1.5,0,1.5,1.5,1.5,1.5,1.5,2.5,2,0,1.5,0,1.5,1.5,1,2,1.5,1.5,1.5,1.5,2,2)
ldh = layout_with_dh(g, coords = NULL, maxiter = 10, fineiter = max(10,
                     log2(vcount(g))), cool.fact = 0.75, weight.node.dist = 1,
                     weight.border = 0, weight.edge.lengths = edge_density(g)/10,
                     weight.edge.crossings = 1 - sqrt(edge_density(g)),
                     weight.node.edge.dist = 0.2 * (1 - edge_density(g)))

# Plotting
plot(g, vertex.label=V(g)$label, vertex.label.dist=dist_labels, vertex.label.degree=lab.locs, vertex.color=graphCol, layout=ldh)

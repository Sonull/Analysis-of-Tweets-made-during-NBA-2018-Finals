#Social Network Analysis

#Install Relevant Packages

#install.packages('streamR')
#install.packages('ggmap')
#install.packages("qgraph")
library(streamR)
library(tidyverse)
library(scales)
library(tm)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(reshape2)
library(gridExtra)
library(corrplot)
library(ggmap)
library(igraph)
library(leaflet)
library(knitr)
library("qgraph")


#################




#Reading the json file
all_tweets <- readTweets("/Users/sonal/Desktop/SNAProject/TweetsNBA.json", verbose=FALSE)

#Identifies nodes and links
ids <- sapply(all_tweets, function(x) x$id_str)

#an id is the retweet id if the tweet under consideration is actually the retweeted one
ret_ids <- sapply(all_tweets, function(x) if(is.null(x$retweeted_status)) NA else x$retweeted_status$id_str)

data_ids <- data.frame(ids, ret_ids)
head(data_ids)
dim(data_ids)
#51425 2


# Create nodes and edges dataframes
#getting all nodes --- nodes can be ret_ids also
nodes <- unique(append(ids, na.omit(ret_ids)))
#saving nodes
write.csv(nodes,"/Users/sonal/Desktop/SNAProject/nodes.csv", row.names = FALSE)

#getting unique combinations with no missing values
edges <- unique(na.omit(data_ids))
head(edges)
#saving edges
write.csv(edges,"/Users/sonal/Desktop/SNAProject/edges.csv", row.names = FALSE)

#creating the network
net <- graph_from_data_frame(edges, directed=T,vertices=nodes) 

#creating a graphml file which will be used in Gephi
graphml_file <- "NBATweets.graphml"
write.graph(net, file=graphml_file, format="graphml")


######################################################
## DEGREES 
######################################################
#out_degree
out_degree <- degree(net, mode="out")
head(sort(out_degree,decreasing=TRUE))
tail(sort(out_degree,decreasing=TRUE))
hist(out_degree, col='purple',ylab='Frequency', main="Histogram of out-degree")

in_degree <- degree(net, mode="in")
head(sort(in_degree,decreasing=TRUE))
#plotting it on a graph
hist(in_degree, col='purple',ylab='Frequency', main="Histogram of in-degree")
in_degree
#all_degrees
all_degree <- degree(net, mode="all")
#plotting it on a graph
hist(all_degree, col='purple',ylab='Frequency', main="Histogram of node degree")

######################################################
## CENTRALITY MEASURES
######################################################


#number of nodes
vcount(net)

#number of edges
ecount(net)

# Density
# The proportion of present edges from all possible ties.
edge_density(net, loops=F) 
#[1] 1.155514e-05
ecount(net)/(vcount(net)*(vcount(net)-1)) #for a directed network 
#[[1] 1.155514e-05

# Reciprocity
# The proportion of reciprocated ties (for a directed network).
reciprocity(net)
#[1] 0

#Cumulative Frequency
dd_all <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(all_degree), y=1-dd_all, pch=19, cex=0.8, col="blue", 
      xlab="Degree", ylab="Cumulative Frequency")


# Closeness (centrality based on distance to others in the graph)
# Inverse of the node's average geodesic distance to others in the network
close <- closeness(net, mode="all", weights=NA) 
hist(close,
     col='purple',
     main='Histogram of Closeness',
     ylab='Frequency')

# Eigenvector (centrality proportional to the sum of connection centralities)
# Values of the first eigenvector of the graph adjacency matrix
ecentrality <- eigen_centrality(net, directed=T, weights=NA)
head(ecentrality)

# Betweenness (centrality based on a broker position connecting others)
# (Number of geodesics that pass through the node or the edge)
between <- betweenness(net, directed=T, weights=NA)
between
head(between)

ebetween <- edge_betweenness(net, directed=T, weights=NA)
head(ebetween,15)

centr_degree(net,mode="all")

#Hub score and Authority Score
hs <- hub.score(net)$vector
as <- authority.score(net)$vector
hs
as

### POWER LAW

# Degree distribution is the cumulative frequency of nodes with a given degree
# this, like degree() can be specified as "in", "out", or "all"
deg.distr<-degree.distribution(net,cumulative=T,mode="all")

# Using the power.law.fit() function I can fit a power law to the degree distribution
power<-power.law.fit(all_degree)

# The output of the power.law.fit() function tells me what the exponent of the power law is ($alpha)
# and the log-likelihood of the parameters used to fit the power law distribution ($logLik)
# Also, it performs a Kolmogov-Smirnov test to test whether the given degree distribution could have
# been drawn from the fitted power law distribution.
# The function thus gives me the test statistic ($KS.stat) and p-vaule ($KS.p) for that test

# Then I can plot the degree distribution
plot(deg.distr,log="xy",
     ylim=c(.01,10),
     bg="black",pch=21,
     xlab="Degree",
     ylab="Cumulative Frequency")


power$alpha
power$logLik

power$KS.stat
power$KS.p
# And the expected power law distribution
lines(1:20,10*(1:20)^((-power$alpha)+1))

# Graphs typically have a Poisson distribution (if they are random),
# power law (preferential attachment), or truncated power law (many real networks) degree distribution


#################################################################################################################


# Most relevant nodes
top_nodes <- sort(degree(net, mode="in"), decreasing=TRUE)[1:10]
plot(top_nodes,main="Top 10 nodes in terms of their degrees",col='blue',pch=19)
head(top_nodes)

out_nodes <- sort(degree(net, mode="out"), decreasing=TRUE)[1:10]
out_nodes 
#1004531741216989191 1004531743410573312 1004531743272194048 1004531744010272768 1004531744291336193 1004531744337522689 
#1                   1                   1                   1                   1                   1 
#1004531745088303109 1004531747663511552 1004531748481445889 1004531748691009536 
#1                   1                   1                   1 

# Most relevant users and their Tweets
rt_sc_name <- sapply(all_tweets, function(x) if(is.null(x$retweeted_status)) NA else x$retweeted_status$user$screen_name)
rt_text <- sapply(all_tweets, function(x) if(is.null(x$retweeted_status)) NA else x$retweeted_status$text)

top_users <- rt_sc_name[match(names(top_nodes), ret_ids)]
top_tweets <- rt_text[match(names(top_nodes), ret_ids)]
top_users
top_tweets
#subsetting for node with the highest degree
subcomp <- subcomponent(net, "1004532801461456896", mode = "all") 
subnet <- induced_subgraph(net, subcomp) 

#checking for missing data
indx <-lapply(subnet, function(x){any(is.na(x))} )
indx #none

#attempt - 1: colorful ball
V(subnet)$degree <- degree(subnet)
plot(subnet,
     vertex.color=rainbow(52),
     edge.color='orange',
     # vertex.size = V(subnet)$degree*0.4,
     edge.arrow.size=0.04,
     #vertex.label.cex=0.01,
     layout=layout.kamada.kawai)

#Attempt 2- Minimum spanning tree
#g<-graph.adjacency(subnet,mode='undirected',weighted=TRUE)
MST <- minimum.spanning.tree(subnet)
MST
plot(MST,vertex.shape="circle")
A <- get.adjacency(MST,sparse=F)



#Attempt 3 - one blue ball
V(subnet)$degree <- degree(subnet)
plot(subnet,
     vertex.color=rainbow(52),
     edge.color='orange',
     vertex.size = V(subnet)$degree*0.4,
     edge.arrow.size=0.04,
     vertex.label.cex=0.01,
     layout=layout.kamada.kawai)



#Attempt - 4
V(subnet)$degree <- degree(subnet)
plot(subnet,
     vertex.color=rainbow(52),
     edge.color='orange',
     vertex.size = V(subnet)$degree*0.001,
     edge.arrow.size=0.2,
     vertex.label.cex=0.4,
     layout=layout.kamada.kawai)

#Attempt - 5
plot(subnet,
     vertex.color=rainbow(52),
     edge.color='orange',
     vertex.size = V(subnet)$degree*0.01,
     edge.arrow.size=0.02,
     vertex.label.cex=0.4,
     layout=layout.kamada.kawai)

#Attempt 6 
plot(subnet,
     vertex.color=rainbow(52),
     edge.color='orange',
     vertex.size = V(subnet)$degree*0.04,
     edge.arrow.size=0.4,
     vertex.label=NA,
     #vertex.label.cex=0.01,
     layout=layout.fruchterman.reingold)

#Attempt 7
V(subnet)$color <- adjustcolor("SkyBlue2",alpha.f=.8)
V(subnet)$degree <- degree(subnet)
V(subnet)$color[V(subnet$label=='1004536940706689024')]<-adjustcolor("firebrick1",alpha.f=.5)
plot(subnet,
     vertex.color=rainbow(52),
     vertex.size = V(subnet)$degree*0.04,
     edge.arrow.size=0.4,
     vertex.label.cex=0.0000000000000001,
     layout=layout.kamada.kawai)

#Diameter
diam <- get_diameter(subnet, directed=T, weights=NA)
diam 

## NOTHING WORKS FOR THE ONE WITH DEGREE AS 8406 THEREFORE TRYING FOR THE NODE WITH DEGREE = 642
 


#works


#how baout this:

V(subnet)$color <- adjustcolor("SkyBlue2",alpha.f=.8)
V(subnet)$color[V(subnet$label=='1004532801461456896')]<-adjustcolor("firebrick1",alpha.f=.5)
plot(subnet,
     vertex.color=V(subnet)$color,
     vertex.size = V(subnet)$degree*0.04,
     edge.arrow.size=0.4,
     vertex.label.cex=0.0000000000000001,
     layout=layout.kamada.kawai)



subcomp <- subcomponent(net, "1004532801461456896", mode = "all") 
subnet <- induced_subgraph(net, subcomp) 


##trying with the one that has second highest degree

V(subnet)$label <- V(subnet)$name




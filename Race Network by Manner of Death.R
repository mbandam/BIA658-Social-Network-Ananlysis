# load the required packages
library(readr)
library(dplyr)
library(tidytext)
library(igraph)

# Load the data in the data frame
Death_records = read_csv("Deaths_in_US_2014.csv")

# Calculating the size of the node
Race_Size = Death_records %>% group_by(Race) %>% summarise(Size = n()) 

# creating the adjacency list
Race_unigram = Death_records %>% group_by(Race) %>% summarise(MannerOfDeath=paste(MannerOfDeath,collapse=" "))
Race_unigram = Race_unigram %>% unnest_tokens(tokens, MannerOfDeath, to_lower = TRUE)
Race_unigram = unique(Race_unigram)

create_adj_list = function(df){
  # Input: a dataframe with a column "tokens"
  # Output: all possible 2-combinations (sorted) of the unique tokens
  unique_tokens = unique(df$Race)
  adj_list = data.frame()
  if(length(unique_tokens) >= 2) {
    all_combins = t(combn(unique_tokens, 2))
    all_combins = t(apply(all_combins, 1, sort))
    adj_list = data.frame(all_combins, stringsAsFactors = FALSE)
  }
  return(adj_list)
}
adj_list = Race_unigram %>% group_by(tokens) %>% do(create_adj_list(.))

# creatig weighted adjacency list
adj_list_weighted = data.frame(adj_list) %>% group_by(X1, X2) %>% summarise(weight = n())

# Creating the weighted graph
Race_graph_weighted = graph.data.frame(adj_list_weighted[, c("X1", "X2")], directed = FALSE)
E(Race_graph_weighted)$weight = adj_list_weighted$weight

# Giving color to the nodes on the basis of race
V(Race_graph_weighted)$color <- ifelse(V(Race_graph_weighted)$name == "American Indian", "saddlebrown", 
                                       ifelse(V(Race_graph_weighted)$name == "Asian Indian","brown",
                                              ifelse(V(Race_graph_weighted)$name == "Balck", "black",
                                                     ifelse(V(Race_graph_weighted)$name == "Chinese", "gray",
                                                            ifelse(V(Race_graph_weighted)$name =="Eskimos", "yellow",
                                                                   ifelse(V(Race_graph_weighted)$name == "Filipino", "blue",
                                                                          ifelse(V(Race_graph_weighted)$name =="Guamanian","orange",
                                                                                 ifelse(V(Race_graph_weighted)$name =="Hawaiian","pink",
                                                                                        ifelse(V(Race_graph_weighted)$name =="Japanese","seagreen3",
                                                                                               ifelse(V(Race_graph_weighted)$name =="Korean","thistle2",
                                                                                                      ifelse(V(Race_graph_weighted)$name =="Samoan","yellow3",
                                                                                                             ifelse(V(Race_graph_weighted)$name =="Vietnamese","purple3","seashell1"))))))))))))

# Plot the weighted graph
plot(Race_graph_weighted,layout=layout.circle,  edge.arrow.size=.7,
     edge.width=E(Race_graph_weighted)$weight,vertex.size=Race_Size$Size/100,
     edge.color="darkslateblue", vertex.color=V(Race_graph_weighted)$color, vertex.label.family="Arial Black",
     frame = 'TRUE',main = "Race Network by Manner of Death",
     vertex.label.font=1, vertex.label.cex=.8,vertex.label.color = 'palevioletred2')
#plot(simplify(state_graph_weighted))

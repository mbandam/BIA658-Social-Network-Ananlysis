# Install and load the required packages
install.packages('tidytext')
install.packages('readr')
install.packages('dplyr')
install.packages('igraph')
library(readr)
library(dplyr)
library(tidytext)
library(igraph)

# Load the data in the data frame
Death_records = read_csv("Deaths_in_US_2014.csv")

# Calculating the size of the node
State_Size = Death_records %>% group_by(State) %>% summarise(Number_of_Deaths = n(), Population = max(Population)) 
State_Size$Size = State_Size$Population / State_Size$Number_of_Deaths

# creating the adjacency list
State_unigram = Death_records %>% group_by(State) %>% summarise(Causes=paste(Cause,collapse=" "))
State_unigram = State_unigram %>% unnest_tokens(tokens, Causes, to_lower = TRUE)
State_unigram = unique(State_unigram)

create_adj_list = function(df){
  # Input: a dataframe with a column "tokens"
  # Output: all possible 2-combinations (sorted) of the unique tokens
  unique_tokens = unique(df$State)
  adj_list = data.frame()
  if(length(unique_tokens) >= 2) {
    all_combins = t(combn(unique_tokens, 2))
    all_combins = t(apply(all_combins, 1, sort))
    adj_list = data.frame(all_combins, stringsAsFactors = FALSE)
  }
  return(adj_list)
}
adj_list = State_unigram %>% group_by(tokens) %>% do(create_adj_list(.))

# creatig weighted adjacency list
adj_list_weighted = data.frame(adj_list) %>% group_by(X1, X2) %>% summarise(weight = n())

# plot the weighted graph
state_graph_weighted = graph.data.frame(adj_list_weighted[, c("X1", "X2")], directed = FALSE)
E(state_graph_weighted)$weight = adj_list_weighted$weight
plot(state_graph_weighted,layout=layout.fruchterman.reingold,  edge.arrow.size=.7,
     edge.width=E(state_graph_weighted)$weight,vertex.size=State_Size$Size/10000,
     edge.color="darkslateblue", vertex.color="palevioletred2", vertex.frame.color = "black",
     frame = 'TRUE',main = "State Network by causes of death",
      vertex.label.font=1, vertex.label.cex=.8,vertex.label.color = 'black')
#plot(simplify(state_graph_weighted))

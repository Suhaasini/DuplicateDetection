library(tidyverse)
library(knitr)
library(igraph)
library(networkD3)
library(visNetwork)
library(tidytext) 
library(wordcloud)
library(ggtext)
library(ggalt)
library(ggthemes)
library(ggpubr)

IEC_data <- read.csv('Testing.csv')
knitr::kable(IEC_data)
#Change in long format
IEC_data %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(!IE_code, names_to = 'Attribute',values_to = 'Attrib_value') %>% 
  relocate(Attribute, .after = 3) -> long_data
# graph object
g <- graph_from_data_frame(long_data)
# Long format all columns
long_data2 <- IEC_data %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(everything(), names_to = 'Attribute', values_to = 'Attrib_value') %>% 
  distinct()
# Re-order using joins
V(g)$att_type <- V(g)$name %>% 
  as.data.frame() %>% 
  set_names('name') %>% 
  inner_join(long_data2, by = c("name" = "Attrib_value")) %>% 
  pull(Attribute)
V(g)$color <- c('steel blue', 'orange')[1 + (V(g)$att_type == 'IE_code')]
V(g)$shape <- c("square", "circle")[(V(g)$att_type == 'IE_code')+1]

#Plot the object
echo=FALSE
fig.align='center'
fig.width=7
fig.cap="Network Diagram of Duplicate Importers Exporters detected through network analysis"
visIgraph(g)
#Network output-
#{r}
output <- g %>% 
  components() %>% 
  pluck(membership) %>% 
  stack() %>% 
  set_names(c('Group_id', 'IE_code')) %>% 
  right_join(IEC_data %>% 
               mutate(IE_code = as.factor(IE_code)),
             by = "IE_code")
knitr::kable(output)

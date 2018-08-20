library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)

# Association rule mining

groceries = scan("../data/groceries.txt", what="", sep="\n")
str(groceries)
summary(groceries)
head(groceries)

y = strsplit(groceries, ",")
#View(y)
## Cast this variable as a special arules "transactions" class.
gro_trans = as(y, "transactions")
summary(gro_trans)

# Now run the 'apriori' algorithm
# Look at rules with support > .005 & confidence >.1 & length (# cart) <= 10
groceryrules = apriori(gro_trans, 
                     parameter=list(support=0.005, confidence=.2))

# Look at the output... so many rules!
inspect(groceryrules)

## Choose a subset
inspect(subset(groceryrules, subset=lift > 3)) #no whole milk found
inspect(subset(groceryrules, subset=confidence > 0.6)) #only whole milk and other vegetables
inspect(subset(groceryrules, subset=lift > 3 & confidence > 0.4))

# plot all the rules in (support, confidence) space
# notice that high lift rules tend to have low support
plot(groceryrules)

# can swap the axes and color scales
plot(groceryrules, measure = c("support", "lift"), shading = "confidence")

# "two key" plot: coloring is by size (order) of item set
plot(groceryrules, method='two-key plot')

# can now look at subsets driven by the plot
inspect(subset(groceryrules, support > 0.05))
inspect(subset(groceryrules, confidence > 0.65))


# graph-based visualization
sub1 = subset(groceryrules, subset=confidence > 0.65 & support > 0.00001)
summary(sub1)
plot(sub1, method='graph')
?plot.rules

plot(head(sub1, 1, by='lift'), method='graph')

# export
saveAsGraph(head(groceryrules, n = 1000, by = "lift"), file = "groceryrules.graphml")

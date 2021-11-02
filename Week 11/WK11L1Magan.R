# Hank Magan
# 11.02.2021
# WK11L1Magan.R 

# clean up old stuff and set working directory
rm(list=ls())
setwd("~/GitHub/IE3620/Week 11")

# import data & ARM packages
laptops <- read.csv("laptops.csv", header=F)
library(arules)
library(arulesViz)

# import data as "transactions" instead of data.frame for ARM
laptopTrans <- read.transactions(file="laptops.csv", format="basket", sep=",", rm.duplicates=T)
class(laptopTrans)

# explore transactions data
summary(laptopTrans)
inspect(head(laptopTrans))

# generate cross tables;
# count = raw number of occurrences
# support = %
# lift = relative association; chance to be bought together 
ct_count <- crossTable(laptopTrans, measure="count", sort=T)
ct_count[1:5, 1:5]

ct_supp <- crossTable(laptopTrans, measure="support", sort=T)
ct_supp[1:5, 1:5]

ct_lift <- crossTable(laptopTrans, measure="lift", sort=T)
ct_lift[1:5, 1:5]

# view the most purchased products
sort(itemFrequency(laptopTrans, type="absolute"), decreasing=T) # actual count
sort(itemFrequency(laptopTrans, type="relative"), decreasing=T) # in %
itemFrequencyPlot(laptopTrans, topN=10, type="relative")

# perform association rule mining (ARM)

# set parameters to default/standard values
metric.params <- list(supp=0.01, conf=0.5)

# generate rules using the apriori algorithm & sort by lift
rules <- apriori(laptopTrans, parameter=metric.params)
rules <- sort(rules, by="lift", decreasing=T)

# explore generated rules
inspect(rules)
inspectDT(rules)

# most significant association between laptop & tablet to buy headset (1.667 lift)
# laptop, tablet, monitor/printer similarly associated
 
# some items seemingly unlikely to be bought (lift < 1), however it's hard to sift 
# through all the rules; let's prune the data

# prune data (remove redundant rules)
rules.sorted <- sort(rules, by="support")
inspect(rules.sorted)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
redundant
which(redundant)

# remove redundant rules
rules.pruned <- rules[!redundant]
inspect(rules.pruned)
# leaves all but 5 rules

# sort pruned rules by lift
rules <- sort(rules.pruned, by="lift", decreasing=T)
inspect(rules)
inspectDT(rules)

# those buying laptops, tablets (and monitors) are likely to buy a headset (highest lift)
# those buying headsets are likely to buy laptops
# those buying headsets and a monitor/laptop are somewhat likely to buy a tablet
# lift values < 1 seem to have gotten removed after pruning

# find item sets which lead to buying a specific item
# set params
metric.params <= list(supp=0.001, conf=0.5, minlen=2)
rules <- apriori(data=laptopTrans, parameter=metric.params, # search for tablets
                 appearance=list(default="lhs", rhs="Tablet"), 
                 control=list(verbose=T))
rules <- sort(rules, decreasing=T, by="lift")
inspect(rules[1:5])

# find items customers are likely to buy from a given item set
rules <- apriori(data=laptopTrans, parameter=metric.params, # search for laptop + headset
                 appearance=list(default="rhs", lhs=c("Laptop", "Headset")), 
                 control=list(verbose=T))
rules <- sort(rules, decreasing=T, by="lift")
inspect(rules[1:5]) # not entirely helpful with small data set

# visualizing rules
plot(rules, method="graph", engine="htmlwidget")
plot(rules, method="graph", engine="interactive")

ling <- read.table('ling-data-clean.data')
binary <- read.table('binary-ling-data.data')

## Finding out highest number question on average per state

a <- ling
highest.avg.response.per.state <- sapply(as.matrix((levels(a$STATE))),function(x) which.max(colSums(a[a$STATE==x,][,5:71],na.rm = T)/nrow(a)))
states.not.q59 <- which(highest.avg.response.per.state !=10)

## It seems that it was Q59 for almost all of the states aside from 'C)', 'IS', 'NB', 'TA', and 'WN'.  All of which were Q82. 

lowest.avg.response.per.state <- sapply(as.matrix((levels(a$STATE))),function(x) which.min(colSums(a[a$STATE==x,][,5:71],na.rm = T)/nrow(a))) 
states.not.q63 <- which(highest.avg.response.per.state != 14)

## The most frequent lowest average response was Q63.  In this case, it wasn't as overhwelming as in the positives.  There were 39 states that did not have Q63 as it's highest 

pc.analysis <- prcomp(a[,5:71])
sdevs <- pc.analysis$sdev
plot(sdevs)
max.sdevs <- max(sdevs)
min.sdevs <- min(sdevs)
## A look at the standard deviations shows that it has a great ammount of variance among them as it ranges from  8.0874007 to 0.2576736

## Let's take a look at the state frequencies
plot(pc.analysis$x[,floor(quantile(order(var.by.question),c(.25,.75)))],col = mycolors[k.means$cluster])ns <- kmeans(a[,5:71],2,nstart = 10)
plot(pc.analysis$x[,floor(quantile(order(var.by.question),c(.25,.75)))],col = mycolors[k.means$cluster])state.freq <- sapply(levels(a$STATE), function(x) sum(a$STATE == x,na.rm = T))
ordered.state.freq <- state.freq[order(state.freq)]


###From this we can see that the most frequent state was California with 3602, followed by New York closely with 3491
#par(mar=c(35,35,0,0))
#plot(a[a$SATE == 'CA',])

#plot(head(a[a$STATE == 'CA' | a$STATE == 'NY',][,5:71]),col = mycolors[c('CA','NY')])

## Take a look at the variance of each question
var.by.question <- sapply(colnames(a)[5:71],function(x) var(a[,x]))
barplot(var.by.question)

## The option Q059 with an astonishing 64.10198 variance.  Among the variance there is quite a large ammount of variance.

## Let's go back to the pca and plot some of it.

## Here's a plot of the pca for the 25% percentile and 75% percentile questions with regards to variance.  The red is 25%, the blue is 75%
mycolors = c('red','blue')
plot(pc.analysis$x[,floor(quantile(order(var.by.question),c(.25,.75)))],col = mycolors)

## Here's a plot for the lowest and highest varaince.  Red will be low.
plot(pc.analysis$x[,order(var.by.question)[c(1,length(var.by.question))]],col = mycolors)

## Here's some k-means with k = 2 and a nstart = 10.
k.means <- kmeans(a[,5:71],2,nstart = 10)
plot(pc.analysis$x[,floor(quantile(order(var.by.question),c(.25,.75)))],col = mycolors[k.means$cluster])
## An hclust
###h.clust <- hclust(dist(a[,5:71]))

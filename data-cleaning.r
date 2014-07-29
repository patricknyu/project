# create a subset of the data in lingData.txt where all observations that
# omitted every question have been removed. Store the **number** of
# observations that you omitted as the variable <n.no.response>
t <- read.table('lingData.txt',sep = ' ')
omit.indicies <- which(rowSums(t[,5:71] == 0) == 67)
n.no.response <- length(omit.indicies)
t <- t[,-omit.indicies] 

# plot a histogram of the number of omitted responses for each observation
# after removing observations that omitted all questions
hist(rowSums(t[,5:71] == 0,na.rm = TRUE),xlab = 'Question Number',ylab = '# of Ommitted Responses')

# using your subset (with observations that responded to no questions
# removed) find the 99th percentile cutoff for number of questions
# omitted. Remove all observations with more than this number of omitted
# questions.
non.response.cutoff <- quantile(rowSums(t[,5:71] == 0,na.rm = TRUE),.99)
write.table(as.data.frame(t[-which(rowSums(t[,5:71] == 0,na.rm = TRUE) >=non.response.cutoff),]),file = 'ling-data-clean.data')
# save the subset of remaining observations in a file named
# "ling-data-clean.data" 

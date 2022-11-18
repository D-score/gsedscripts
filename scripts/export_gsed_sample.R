# first run 293_0.R up to line 96

# make sample of 10 rows
set.seed(4)
idx <- sort(sample(1:nrow(data), size = 10))
gsed_sample <- data[idx, c(3, 5, 7:299)]
gsed_sample$subjid <- c(1:4, 8:11, 13:14)
write.table(gsed_sample,
            file = "gsed_sample.txt",
            sep = "\t", row.names = FALSE,
            na = "", quote = FALSE)

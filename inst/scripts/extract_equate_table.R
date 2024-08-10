# Extract items and tau of equate groups from keys "gsed1912" and "gsed2406"
# For phase1 pubication, supplementary material

library(dmetric)

model <- "818_17_joint_fixed"
path <- path.expand("~/Project/gsed/phase1-backup/joint")
model <- readRDS(file.path(path, model, "model.Rds"))

eq <- dmetric::get_equates(model)
len <- sapply(eq, length)
eqtab <- data.frame(equate = rep(names(len), len),
                    item = unname(unlist(eq)))
eqtab$item2 <- gsedread::rename_vector(eqtab$item, "gsed", "gsed2")
eqtab$label <- get_labels(eqtab$item, trim = 40)
eqtab$delta_1912 <- round(get_tau(items = eqtab$item, key = "gsed1912"), 1)
eqtab$delta_2406 <- round(get_tau(items = eqtab$item2, key = "gsed2406"), 1)

write.csv(eqtab, file = "equate_table.csv", row.names = FALSE)

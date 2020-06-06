# Short run converting rownames to the first variable
d <- cbind(rownames(d), data.frame(d, row.names=NULL))

#long run conversion of row name to first variable
d <- df
names <- rownames(d)
rownames(d) <- NULL
data <- cbind(names,d)


df$names <- rownames(df)



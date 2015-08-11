qw.data <- NULL
reports <- NULL
errors <- ""
###Plot global settings
medium.colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00")
names(medium.colors) <- c("WS ","WG ","WSQ","WGQ","OAQ")
## Sets color to medium code name, not factor level, so its consistant between all plots regardles of number of medium codes in data
qual.shapes <- c(19,0,2,5)
names(qual.shapes) <- c("Sample","<",">","E")
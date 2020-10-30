
library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
library(viridis)

extend <- function(alphabet) function(i) {
  base10toA <- function(n, A) {
    stopifnot(n >= 0L)
    N <- length(A)
    j <- n %/% N 
    if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
  }   
  vapply(i-1L, base10toA, character(1L), alphabet)
}
MORELETTERS <- extend(LETTERS)

#set working direct
setwd("")

#convert filename to table
file.names = dir(pattern = "*.txt")

# #df = separate(as.data.frame(file.names),
# #df = separate(as.data.frame(file.names),
# #              col = file.names,
#               sep = "#", convert = T,
#               remove = T,
#               into = c("single_pop", "cost0", "cost", "release0", "release"),
#               extra = "warn", fill = "warn")

combination.vector <- paste(MORELETTERS(1:length(file.names)))
slim.master <- NULL
for(k in 1:length(file.names)){
  slim = read.table(file.names[k], sep = " ", header = F)
  colnames(slim) = c("Generation",
                     "Fitness",
                     "Release",
                     "run.no")

  slim$combination <- combination.vector[k]
  
  slim.master <- rbind(slim.master, slim)
}


slim.master %>%
  group_by(combination) %>% 

  summarise_all(funs(mean)) %>%  #-> dat.sum
  ggplot(., aes(x= Release, y= Fitness)) +
  geom_tile(aes(fill= Generation)) +
  scale_fill_viridis(discrete = F, option = "inferno", limits=c(floor(25), ceiling(60)))+
  ggtitle("")


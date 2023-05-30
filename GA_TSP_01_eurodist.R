
# ----- GA para resolver o PCV (prob. do Caixeiro Viajante) -----
#              (TSP - travelling Salesman Problem)

#  https://rpubs.com/karthy1988/TSP_GA#:~:text=4.,using%20Genetic%20Algorithm%20in%20R&text=Given%20a%20matrix%20with%20distances,in%20a%20simple%20directed%20graph.
# Optimized Delivery Route using Genetic Algorithm: Cost cutting for e-commerce
# Karthik  30 November 2015


# -------- LIMPA MEMoRIA ------------------
rm(list =ls())
# -----------------------------------------


# install.packages("GA")
library(GA)

# DATASE "eurodist"
# Distances Between European Cities

data("eurodist", package = "datasets")
D <- as.matrix(eurodist)
str(eurodist)

D
str(D)
head(D)

#Function to calculate tour length 
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[,2:1]
  sum(distMatrix[route])
}

#Firness function to be maximized
tspFitness <- function(tour, ...) 1/tourLength(tour, ...)

GA <- ga(type = "permutation", fitness = tspFitness, distMatrix = D,
         min = 1, max = attr(eurodist, "Size"), popSize = 50, 
         maxiter = 5000, run = 500, pmutation = 0.2)
summary(GA)

GA2 <- ga(type = "permutation", fitness = tspFitness, distMatrix = D,
         min = 1, max = 40, popSize = 50, maxiter = 5000,
         run = 500, pmutation = 0.2)
summary(GA2)

#Visualization 
mds <- cmdscale(eurodist)
x <- mds[, 1]
y <- -mds[, 2]
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
       col = "light gray")
tour <- GA@solution[1, ]
tour <- c(tour, tour[1])
n <- length(tour)
arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]],
       length = 0.15, angle = 25, col = "steelblue", lwd = 2)
text(x, y, labels(eurodist), cex=0.8)

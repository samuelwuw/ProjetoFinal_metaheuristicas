

# --------------------------------------------------------------------
#    ------------  Package "metaheuristicOpt" ---------------
# --------------------------------------------------------------------

# Este pacote inclui várias Metaheurísticas (MH)
# Algumas são bem exóticas (Nova Geracao de MHs)

# Dentre as MHs mais conhecidas, o pacote inclui:
#    GA  - Genetic Algorithm (Classica)
#   PSO - Particle Swarm Optimization (Bem conhecida)
#   ABC - Artificial Bee Colony Algorithm (é considerada Nova Geracao)

install.packages("metaheuristicOpt")
library(metaheuristicOpt)


# ----------------------------------------------------------------------
#            OTIMIZACAO DO PROBLEMA DO TRANSPORTE
#  Usado como exemplo para aplicacao de MHs do Pacote 'metaheuristicOpt'
#                Sao testadas 6 MHs do pacote
# ----------------------------------------------------------------------


# Inicalmente, define-se uma Funcao do R, com o MODELO de TRANSPORTE
# MODELO inclui FUNCAO OBJETIVO, RESTRICOES e "PENALIDADES"
# Esta Funcao sera chamada pelas Funcoes das Metaheurisiticas


Model_TRANSP <- function(x){
  Z <- 0
  PENALTY <- 400
  
# FUNCAO OBJETIVO
  Z<- 50*x[1] + 20*x[2] + 70*x[3] + 30*x[4] + 60*x[5] + 90*x[6]

# RESTRICOES
  Oferta1 <- x[1] + x[2] + x[3]
  Oferta2 <- x[4] + x[5] + x[6]
  Demanda1 <- x[1] + x[4]
  Demanda2 <- x[2] + x[5]
  Demanda3 <- x[3] + x[6]
 
  TOTAL <- x[1] + x[2] + x[3]  + x[4] + x[5] + x[6]
#          ------------ PENALIDADES FIXAS ------------ 
# 
# if(Oferta1 > 15000 ){
#   Z <- Z + 9999999
# }
# if(Oferta2 > 25000 ){
#   Z <- Z + 9999999
# }
# if(Demanda1 < 20000 ){
#   Z <- Z + 9999999
# }
# if(Demanda2 < 10000 ){
#   Z <- Z + 9999999
# }
# if(Demanda3 < 10000 ){
#   Z <- Z + 9999999
# }
# ---------------------------------------------------------------

  
#     ------------ PENALIDADES com Crescimento Linear ------------ 

     # if( TOTAL != 40000 ){
     # Z <- Z + 999999999}

  #   if(Oferta1 > 15000 ){
  #   Z <- Z + (Oferta1 - 15000)*PENALTY
  # }
  # if(Oferta2 > 25000 ){
  #   Z <- Z + (Oferta2 - 25000)*PENALTY
  # }
  # if(Demanda1 < 20000 ){
  #   Z <- Z + (20000 - Demanda1)*PENALTY
  # }
  # if(Demanda2 < 10000 ){
  #   Z <- Z + ( 10000 - Demanda2)*PENALTY
  # }
  # if(Demanda3 < 10000 ){
  #   Z <- Z + (10000 - Demanda3)*PENALTY
  # }
# ---------------------------------------------------------------
  
  
#   ---------- PENALIDADES com Crescimento em Potencia ----------
  
  
  POT <- 2
  DIV <- 1000
  # POT <- 3
  # DIV <- 100000
  # POT <- 4
  # DIV <- 100000000

  if(Oferta1 > 15000 ){
    Z <- Z + (Oferta1 - 15000)^POT/DIV*PENALTY
  }
  if(Oferta2 > 25000 ){
    Z <- Z + (Oferta2 - 25000)^POT/DIV*PENALTY
  }
  if(Demanda1 < 20000 ){
    Z <- Z + (20000 - Demanda1)^POT/DIV*PENALTY
  }
  if(Demanda2 < 10000 ){
    Z <- Z + ( 10000 - Demanda2)^POT/DIV*PENALTY
  }
  if(Demanda3 < 10000 ){
    Z <- Z + (10000 - Demanda3)^POT/DIV*PENALTY
  }
# ---------------------------------------------------------------
  
  
#   ---------- PENALIDADES com Crescimento Exponencial ----------
  
  # if(Oferta1 > 15000 ){
  #   e <- exp((Oferta1 - 15000)/1000)
  #   Z <- Z + e*PENALTY
  # }
  # if(Oferta2 > 25000 ){
  #   e <- exp((Oferta2 - 25000)/1000)
  #   Z <- Z + e*PENALTY
  # }
  # if(Demanda1 < 20000 ){
  #   e <- exp((15000 - Demanda1)/1000)
  #   Z <- Z + e*PENALTY
  # }
  # if(Demanda2 < 10000 ){
  #   e <- exp((10000 - Demanda2)/1000)
  #   Z <- Z + e*PENALTY
  # }
  # if(Demanda3 < 10000 ){
  #   e <- exp((10000 - Demanda3)/1000)
  #   Z <- Z + e*PENALTY
  # }
# ---------------------------------------------------------------
  
    return(Z)
}

# -----------------------------------------------------------------
# ---------------- APLICACAO DAS METAHEURISTICAS -----------------
# -----------------------------------------------------------------

library(metaheuristicOpt)

# --- Define-se No. de Variaveis do Problema
numVar <- 6

# --- Define-se Faixa de Variacao de cada Variavel (pode ser igual p todas)
rangeVar<- matrix(nrow= 2, ncol=6)
for (i in 1:2){
  for (j in 1:numVar){
    rangeVar[1,j] <- as.integer(0)
    rangeVar[2,j] <- as.integer(25000)
  }}
rangeVar


#      ==>>> DETERMINACAO da SOLUÇÃO ÓTIMA pela MH 01 <<<==

# *** SOLUCAO OTIMA por ABC - Artificial Bee Colony Algorithm ***

Solucao_ABC <- ABC(Model_TRANSP, optimType="MIN", numVar, numPopulation=200,
                 maxIter=1000, rangeVar)

## RESULTADOS da SOLUÇÃO ÓTIMA usando o Modelo
Solucao.Otima_ABC <- Model_TRANSP(Solucao_ABC)
Solucao.Otima_ABC
round(Solucao.Otima_ABC, 2)

print(round(Solucao_ABC, 0))
print(Solucao_ABC)



#      ==>>> DETERMINACAO da SOLUÇÃO ÓTIMA pela MH 02 <<<==

# ***** SOLUCAO OTIMA por GA - Genetic Algorithm ******

Solucao_GA <- GA(Model_TRANSP, optimType="MIN", numVar, numPopulation=100,
                 maxIter=3000, rangeVar)

## RESULTADOS da SOLUÇÃO ÓTIMA usando o Modelo
Solucao.Otima_GA <- Model_TRANSP(Solucao_GA)
Solucao.Otima_GA
round(Solucao.Otima_GA, 2)

print(round(Solucao_GA, 0))
print(Solucao_GA)


#      ==>>> DETERMINACAO da SOLUÇÃO ÓTIMA pela MH 03 <<<==

# ***** SOLUCAO OTIMA por PSO - Particle Swarm Optimization ******

Solucao_PSO <- PSO(Model_TRANSP, optimType="MIN", numVar, numPopulation=100,
                   maxIter=10000, rangeVar)

## RESULTADOS da SOLUÇÃO ÓTIMA usando O MODELO
Solucao.Otima_PSO <- Model_TRANSP(Solucao_PSO)
Solucao.Otima_PSO 
round(Solucao.Otima_PSO, digits =2)

print(round(Solucao_PSO, 0))
print(Solucao_PSO)


#      ==>>> DETERMINACAO da SOLUÇÃO ÓTIMA pela MH 04 <<<==

# ***** SOLUCAO OTIMA por KH - Krill-Herd Algorithm ******

Solucao_KHerd <- KH(Model_TRANSP, optimType="MIN", numVar, numPopulation=50,
                   maxIter=200, rangeVar)


## RESULTADOS da SOLUÇÃO ÓTIMA usando o Modelo
Solucao.Otima_K_Herd <- Model_TRANSP(Solucao_KHerd)
Solucao.Otima_K_Herd 
round(Solucao.Otima_K_Herd, digits =2)

print(round(Solucao_KHerd, 0))
print(Solucao_KHerd)


#      ==>>> DETERMINACAO da SOLUÇÃO ÓTIMA pela MH 05 <<<==

#  ** SOLUCAO OTIMA por DE - Differential Evolution Algorithm **

Solucao_DE <- DE(Model_TRANSP, optimType="MIN", numVar, numPopulation=50,
                 maxIter=1000, rangeVar)

## RESULTADOS da SOLUÇÃO ÓTIMA usando o Modelo
Solucao.Otima_DE <- Model_TRANSP(Solucao_DE)
Solucao.Otima_DE 
round(Solucao.Otima_DE, digits =2)

print(round(Solucao_DE, 0))
print(Solucao_DE)


#      ==>>> DETERMINACAO da SOLUÇÃO ÓTIMA pela MH 06 <<<==

#  ** SOLUCAO OTIMA por FA - Firefly (Vagalume) Algorithm **

Solucao_FA <- FFA(Model_TRANSP, optimType="MIN", numVar, numPopulation=50,
                 maxIter=1000, rangeVar)

## RESULTADOS da SOLUÇÃO ÓTIMA usando o Modelo
Solucao.Otima_FA <- Model_TRANSP(Solucao_FA)
Solucao.Otima_FA 
round(Solucao.Otima_FA, digits =2)

print(round(Solucao_FA, 0))
print(Solucao_FA)













##########         EXEMPLO           ##########
## Optimizing the sphere function

# define sphere function as objective function
sphere <- function(x){
  return(sum(x^2))
}

## Define parameter
numVar <- 5
rangeVar <- matrix(c(-10,10), nrow=2)
rangeVar

## ------------ Calculate the Optimum Solution

## Optimum solution using KH - Krill-Herd Algorithm
resultKH <- KH(sphere, optimType="MIN", numVar, numPopulation=20,
               maxIter=100, rangeVar)


## Optimum solution using ABC - Artificial Bee Colony Algorithm
resultABC <- ABC(sphere, optimType="MIN", numVar, numPopulation=20,
                 maxIter=100, rangeVar)


## calculate the Optimum Value using sphere function

optimum.value <- sphere(resultKH)
optimum.value

optimum.value <- sphere(resultABC)
optimum.value

print(resultABC)



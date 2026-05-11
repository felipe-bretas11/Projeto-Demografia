source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

lxf2010 <- c("Feminino", "Feminino","Feminino", "Feminino", "Feminino", 
             "Feminino", "Feminino", "Feminino", "Feminino", "Feminino", 
             "Feminino", "Feminino", "Feminino", "Feminino", "Feminino", 
             "Feminino", "Feminino", "Feminino", 0, 1, 5, 10, 15, 20, 25,
             30, 35, 40, 45, 50, 55, 60, 65, 70, 75, "80+",
             100000, 97353, 96786, 96597, 96597, 96219, 94707, 93195,
             89981, 87902, 84499, 80718, 73157, 66163, 59358, 50284,
             39887, 30057)

lxf2010 <- as.data.frame(matrix(lxf2010, nrow = 18, ncol = 3))
colnames(lxf2010) <- c("Sexo", "Idade", "lx")

lxm2010 <- c("Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
             "Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
             "Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
             "Masculino", "Masculino", "Masculino", 0, 1, 5, 10, 15, 20, 25,
             30, 35, 40, 45, 50, 55, 60, 65, 70, 75, "80+", 100000, 96313, 95871,
             95429, 94839, 91152, 87317, 82597, 76697, 73452, 69175, 63718, 56491, 48084,
             41299, 32744, 24484, 16224)

lxm2010 <- as.data.frame(matrix(lxm2010, nrow = 18, ncol = 3))
colnames(lxm2010) <- c("Sexo", "Idade", 'lx')

lx2010 <- bind_rows(lxf2010, lxm2010)




lx2010$lx <- as.numeric(lx2010$lx)


niveis_idade <- c("0", "1", "5", "10", "15", "20", "25", "30", "35", "40", 
                  "45", "50", "55", "60", "65", "70", "75", "80+")
lx2010$Idade <- factor(lx2010$Idade, levels = niveis_idade)


graf_lx2010 <- ggplot(data = lx2010, aes(x = Idade, y = lx, color = Sexo, group = Sexo)) +
  geom_line(linewidth = 1) + 
  scale_y_continuous(
    breaks = seq(0, 100000, 10000), 
    limits = c(0, 100000)
  )


graf_lx2010


lxf2024 <- c("Feminino", "Feminino","Feminino", "Feminino", "Feminino", 
             "Feminino", "Feminino", "Feminino", "Feminino", "Feminino", 
             "Feminino", "Feminino", "Feminino", "Feminino", "Feminino", 
             "Feminino", "Feminino", "Feminino", 0, 1, 5, 10, 15, 20, 25,
             30, 35, 40, 45, 50, 55, 60, 65, 70, 75, "80+",
             100000, 98454, 98067, 97680, 97422, 97035, 96777, 96262,
             95360, 93685, 91237, 88015, 85180, 81314, 72036, 60696,
             50258, 40335)

lxf2024 <- as.data.frame(matrix(lxf2024, nrow = 18, ncol = 3))
colnames(lxf2024) <- c("Sexo", "Idade", "lx")

lxm2024 <- c("Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
             "Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
             "Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
             "Masculino", "Masculino", "Masculino", 0, 1, 5, 10, 15, 20, 25,
             30, 35, 40, 45, 50, 55, 60, 65, 70, 75, "80+", 100000, 98726, 98408,
             98408, 98090, 97241, 94906, 92995, 89810, 88430, 84927, 80575, 76010,
             69534, 60723, 48090, 36625, 26539)

lxm2024 <- as.data.frame(matrix(lxm2024, nrow = 18, ncol = 3))
colnames(lxm2024) <- c("Sexo", "Idade", 'lx')

lx2024 <- bind_rows(lxf2024, lxm2024)




lx2024$lx <- as.numeric(lx2024$lx)


niveis_idade <- c("0", "1", "5", "10", "15", "20", "25", "30", "35", "40", 
                  "45", "50", "55", "60", "65", "70", "75", "80+")
lx2024$Idade <- factor(lx2024$Idade, levels = niveis_idade)


graf_lx2024 <- ggplot(data = lx2024, aes(x = Idade, y = lx, color = Sexo, group = Sexo)) +
  geom_line(linewidth = 1) + 
  scale_y_continuous(
    breaks = seq(0, 100000, 10000), 
    limits = c(0, 100000)
  )


graf_lx2024


nqxf2010 <- c("Feminino", "Feminino","Feminino", "Feminino", "Feminino", 
              "Feminino", "Feminino", "Feminino", "Feminino", "Feminino", 
              "Feminino", "Feminino", "Feminino", "Feminino", "Feminino", 
              "Feminino", "Feminino", "Feminino", 0, 1, 5, 10, 15, 20, 25,
              30, 35, 40, 45, 50, 55, 60, 65, 70, 75, "80+",
              0.0265, 0.00582, 0.00195, 0, 0.00391, 0.0157, 0.0160, 0.0345,
              0.0231, 0.0387, 0.0447, 0.0937, 0.0956, 0.103, 0.153, 0.207,
              0.246, 1)

nqxf2010 <- as.data.frame(matrix(nqxf2010, nrow = 18, ncol = 3))
colnames(nqxf2010) <- c("Sexo", "Idade", "nqx")

nqxm2010 <- c("Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
              "Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
              "Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
              "Masculino", "Masculino", "Masculino", 0, 1, 5, 10, 15, 20, 25,
              30, 35, 40, 45, 50, 55, 60, 65, 70, 75, "80+", 0.0369, 0.00459, 0.00461,
              0.00618, 0.0389, 0.0421, 0.0541,
              0.0714, 0.0423, 0.0582, 0.0789, 0.113, 0.149,
              0.141, 0.207, 0.252, 0.337, 1)

nqxm2010 <- as.data.frame(matrix(nqxm2010, nrow = 18, ncol = 3))
colnames(nqxm2010) <- c("Sexo", "Idade", 'nqx')

nqx2010 <- bind_rows(nqxf2010, nqxm2010)




nqx2010$nqx <- as.numeric(nqx2010$nqx)


niveis_idade <- c("0", "1", "5", "10", "15", "20", "25", "30", "35", "40", 
                  "45", "50", "55", "60", "65", "70", "75", "80+")
nqx2010$Idade <- factor(nqx2010$Idade, levels = niveis_idade)


graf_nqx2010 <- ggplot(data = nqx2010, aes(x = Idade, y = nqx, color = Sexo, group = Sexo)) +
  geom_line(linewidth = 1) + 
  scale_y_continuous(
    breaks = seq(0, 1, 0.05), 
    limits = c(0, 1)
  )


graf_nqx2010


nqxf2024 <- c("Feminino", "Feminino","Feminino", "Feminino", "Feminino", 
              "Feminino", "Feminino", "Feminino", "Feminino", "Feminino", 
              "Feminino", "Feminino", "Feminino", "Feminino", "Feminino", 
              "Feminino", "Feminino", "Feminino", 0, 1, 5, 10, 15, 20, 25,
              30, 35, 40, 45, 50, 55, 60, 65, 70, 75, "80+",
              0.0155, 0.00393, 0.00395, 0.00264, 0.00397, 0.00266, 0.00532, 0.00937,
              0.0176, 0.0261, 0.0353, 0.0322, 0.0454, 0.114, 0.157, 0.172,
              0.197, 1)

nqxf2024 <- as.data.frame(matrix(nqxf2024, nrow = 18, ncol = 3))
colnames(nqxf2024) <- c("Sexo", "Idade", "nqx")

nqxm2024 <- c("Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
              "Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
              "Masculino", "Masculino", "Masculino", "Masculino", "Masculino",
              "Masculino", "Masculino", "Masculino", 0, 1, 5, 10, 15, 20, 25,
              30, 35, 40, 45, 50, 55, 60, 65, 70, 75, "80+", 0.0127, 0.00322, 0,
              0.00323, 0.00866, 0.0240, 0.0201,
              0.0342, 0.0154, 0.0396, 0.0512, 0.0567, 0.0852,
              0.127, 0.208, 0.238, 0.275, 1)

nqxm2024 <- as.data.frame(matrix(nqxm2024, nrow = 18, ncol = 3))
colnames(nqxm2024) <- c("Sexo", "Idade", "nqx")

nqx2024 <- bind_rows(nqxf2024, nqxm2024)




nqx2024$nqx <- as.numeric(nqx2024$nqx)


niveis_idade <- c("0", "1", "5", "10", "15", "20", "25", "30", "35", "40", 
                  "45", "50", "55", "60", "65", "70", "75", "80+")
nqx2024$Idade <- factor(nqx2024$Idade, levels = niveis_idade)


graf_nqx2024 <- ggplot(data = nqx2024, aes(x = Idade, y = nqx, color = Sexo, group = Sexo)) +
  geom_line(linewidth = 1) + 
  scale_y_continuous(
    breaks = seq(0, 1, 0.05), 
    limits = c(0, 1)
  )


graf_nqx2024


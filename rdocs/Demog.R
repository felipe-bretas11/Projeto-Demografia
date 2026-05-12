source("rdocs/source/packages.R")

Banco <- read.csv("Trabalho Demografia (questao 1).csv", nrows = 26,header = FALSE,skip = 1,sep = ";") 
Banco<-Banco[,1:2]
names(Banco)<-c("Ano","quantidade")
Banco2 <- read.csv("Trabalho Demografia (questao 1).csv", nrows = 25,header = FALSE,skip = 30,sep = ";") 
Banco2<-Banco2[,1:3]
names(Banco2)<-c("Ano_Obito","<1 ano","1-5")
Banco2$Ano_Obito<-as.Date(paste0(as.character(Banco2$Ano_Obito),"-07-01",sep = ""))
Banco$Ano<-as.Date(paste0(as.character(Banco$Ano),"-07-01",sep = ""))
Banco2L <- pivot_longer(Banco2, 
                           cols = c("<1 ano", "1-5"),  
                           names_to = "Idade_Óbito",       
                           values_to = "Quantidade") 
library(LexisPlotR)
#library(tidyverse)
#library(remotes)
#library(microdatasus)
library(lubridate)
#df<-fetch_datasus(information_system = "SIM-DO",year_start = 2000,year_end = 2024,uf="MG")
#df <- process_sim(df)
#df2<-df[df[,11]==313130,]
#write.csv(df2,"dadosdatasus.csv", row.names = F)
df2<-read.csv("dadosdatasus.csv")
df2<-df2[df2[,5]<405,]
df3<-df2[df2[,5]<415 & df2[,5]>409,]
df2$ANO_O<-substr(as.character(df2$DTOBITO),1, 4)
df2$ANO_N<-substr(as.character(df2$DTNASC),1, 4)
df2 <- df2 %>%
  mutate(Idade2 = ifelse(substr(as.character(IDADE), 1, 1) == "4", 
                              substr(as.character(IDADE), nchar(as.character(IDADE)), nchar(as.character(IDADE))), 
                              0))
df2 <- df2 %>%
  mutate(
    # Garantir que ANO_O e ANO_N são números
    ANO_O = as.numeric(ANO_O),
    ANO_N = as.numeric(ANO_N),
    Idade2 = as.numeric(Idade2),
    
    # Aplicar a lógica
    ANO_L = ifelse(ANO_O - ANO_N == Idade2,
                   as.Date(paste0(as.character(ANO_O) , "-10-01",sep = "")),  # 01/10/ANO_O
                   as.Date(paste0(as.character(ANO_O), "-04-01",sep = ""))),
    
    IdadeL = ifelse(ANO_O - ANO_N == Idade2,
                    Idade2 + 0.4,
                    Idade2 + 0.75)
  )
df2 <- df2 %>%
  mutate(ANO_L = as.Date(ANO_L, origin = "1970-01-01"))

anos <- 2000:2024

# Criar todas as combinações possíveis respeitando a regra
todas_combinacoes <- crossing(
  ano = anos,
  mes = c("04", "10")
) %>%
  mutate(
    ANO_L = dmy(paste0("01/", mes, "/", ano)),
    # Definir IdadeL baseado no mês
    IdadeL_base = case_when(
      mes == "04" ~ 0.75,  # abril termina com .75
      mes == "10" ~ 0.4  # outubro termina com .25
    )
  )

# Agora precisamos dos valores completos de IdadeL
# Assumindo que Idade2 pode variar (ex: 0 a 24)
# Para cada combinação de ano/mês, IdadeL = Idade2 + base

# Se você sabe todos os Idade2 possíveis:
idades2_possiveis <- 0:4  # Ajuste conforme seus dados

todas_combinacoes_completas <- todas_combinacoes %>%
  crossing(Idade2 = idades2_possiveis) %>%
  mutate(IdadeL = Idade2 + IdadeL_base) %>%
  select(ANO_L, IdadeL)

LEXIS<- df2 %>%
  group_by(ANO_L, IdadeL) %>%
  summarise(contagem = n(), .groups = 'drop')
LEXIS2<- todas_combinacoes_completas %>%
  left_join(LEXIS, by = c("ANO_L", "IdadeL")) %>%
  mutate(contagem = ifelse(is.na(contagem), 0, contagem))
LEXIS<-LEXIS[-155,]
Diagrama<-lexis_grid(year_start = 2000,year_end = 2025,age_start = 0,age_end = 5)
Diagrama<-Diagrama+annotate("text",x=LEXIS2$ANO_L,y=LEXIS2$IdadeL,label=LEXIS2$contagem,color="black",size=2.5)+annotate("text",x=Banco$Ano,y=0.15,label=Banco$quantidade,color="red",size=2)+
  theme(axis.text.x = element_text(size = 8))
Diagrama
Prob <- df2 %>%
  filter(ANO_N %in% 2000:2019) %>%  # substitua 'ano' pelo nome da coluna 102
  count() %>%
  pull(n) / sum(Banco$quantidade[1:20])
1-Prob #0.01341683
Prob2 <- df2 %>%
  filter(ANO_N %in% 2000:2023) %>% 
  filter(Idade2 ==0) %>% # substitua 'ano' pelo nome da coluna 102
  count() %>%
  pull(n) / sum(Banco$quantidade[1:24])
1-Prob2 # 0.01140385
ProbBrancos<-df2 %>%
  filter(ANO_N %in% 2022:2023) %>%
  filter(Idade2 ==0) %>%
  filter(RACACOR=="Branca") %>%# substitua 'ano' pelo nome da coluna 102
  count() %>%
  pull(n) / 2025
ProbBrancos
ProbPardos<-df2 %>%
  filter(ANO_N %in% 2022:2023) %>%
  filter(Idade2 ==0) %>%
  filter(RACACOR=="Parda") %>%# substitua 'ano' pelo nome da coluna 102
  count() %>%
  pull(n) / 3110
ProbPardos
ProbPretos<-df2 %>%
  filter(ANO_N %in% 2022:2023) %>%
  filter(Idade2 ==0) %>%
  filter(RACACOR=="Preta") %>%# substitua 'ano' pelo nome da coluna 102
  count() %>%
  pull(n) / 3110
ProbPretos
ProbAmarela<-df2 %>%
  filter(ANO_N %in% 2022:2023) %>%
  filter(Idade2 ==0) %>%
  filter(RACACOR=="Amarela") %>%# substitua 'ano' pelo nome da coluna 102
  count() %>%
  pull(n) / 3110
ProbAmarela
ProbIndígena<-df2 %>%
  filter(ANO_N %in% 2022:2023) %>%
  filter(Idade2 ==0) %>%
  filter(RACACOR=="Indígena") %>%# substitua 'ano' pelo nome da coluna 102
  count() %>%
  pull(n) / 3110
ProbIndígena
ProbNA<-df2 %>%
  filter(ANO_N %in% 2022:2023) %>%
  filter(Idade2 ==0) %>%
  filter(is.na(RACACOR)) %>%# substitua 'ano' pelo nome da coluna 102
  count() %>%
  pull(n) / 13
ProbNA

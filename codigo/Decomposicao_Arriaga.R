### Aplicacao do metodo Arriaga para decomposicao da esperanca de vida por idade e causa de morte ######

#Pacotes necessarias:
library(dplyr)
library(readxl)
library(reshape)
library(reshape2) 
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(ggplot2)
library(lattice)
library(writexl)


win.graph()
options(scipen = 999)


#------------------------# 
# DECOMPOSICAO POR IDADE #
#------------------------#

setwd("C:/.../data") # inserir o caminho para os dados de entrada

# Tabuas de vida do Brasil:
life.table.BR <- read_excel("Tabua_de_vida_BR_UFs.xlsx", col_names = TRUE)

life.table.BR <- life.table.BR %>%
  select(Age, Sex, Region, Year, lx, nLx, Tx, ex) %>%
  filter(Region=="BR")

# Tabuas de vida para o semiarido:
life.table.SE <- read_excel("Tabua_de_vida_Semiarido.xlsx", col_names = TRUE)

life.table.SE <- life.table.SE %>% 
  select(Age, Sex, Region, Year, lx, nLx, Tx, ex)

# Empilhando as tabuas de vida do Brasil e Semiarido:
data <- rbind(life.table.BR, life.table.SE)
data <- data %>%
  mutate(Sex = case_when(
    Sex == "Female" ~ "females",
    Sex == "Male" ~ "males", 
    TRUE ~ Sex))

# Anos inicial e final de analise (devem ser alterados para cada periodo de analise):
year1 <- 2010
year2 <- 2022 # necessario mudar o ano final em cada periodo

# Parametros para analises:
sex <- c('females', 'males') # sexo
region <- unique(data$Region) # regiao
age.g <- unique(data$Age) # grupo etario

# Estimando os parametros do metodo Arriaga:
result = as.data.frame(matrix(NA, 0, 8))
for(this.region in region) {
    for(this.sex in sex) {
    
    tmp1 <- data %>% 
      filter(Sex==this.sex, Year==year1, Region==this.region) %>% 
      select(Region, Year, Sex, Age, lx, nLx, Tx)

    tmp2 <- data %>% 
      filter(Sex==this.sex, Year==year2, Region==this.region) %>% 
      select(Region, Year, Sex, Age, lx, nLx, Tx)

    # Efeito Direto:
    DE <- NA
    for(i in 1:length(age.g)) {
      
      DE[i] <- round(tmp1$lx[i]/tmp1$lx[1]*(tmp2$nLx[i]/tmp2$lx[i]-tmp1$nLx[i]/tmp1$lx[i]),4)
    }
    DE[18] <- round(tmp1$lx[18]/tmp1$lx[1]*(tmp2$nLx[18]/tmp2$lx[18]-tmp1$nLx[18]/tmp1$lx[18]),4)
    
    
    # IEfeitos Indireto e de Interacao:
    IIE <- NA
    for(i in 1:length(age.g)) {
      
      IIE[i] <- round(tmp2$Tx[i+1]/tmp2$lx[1]*(tmp1$lx[i]/tmp2$lx[i]-tmp1$lx[i+1]/tmp2$lx[i+1]),4)
    }
    
    # Efeito Total:
    TE <- DE+IIE
    TE[18] <- DE[18]
    
    # base com todos os resultados:
    tmp3 <- as.data.frame(cbind(DE, IIE, TE))
    tmp3$Sex <- this.sex
    tmp3$Region <- this.region
    tmp3$Age_group <- age.g
    tmp3$Period <- paste0(year1, '/', year2)
    tmp3 <- tmp3 %>% 
      mutate(Percent_Contrib=TE/abs(sum(TE))*100) %>%
      select(Period, Region, Sex, Age_group, DE, IIE, TE, Percent_Contrib)
    result <- rbind(result, tmp3)
    result <- result %>%
      select (Region, Period, Sex, Age_group, DE, IIE, TE, Percent_Contrib)
    
   }
   print(this.region)
}
# Retornar para a linha 50 e alterar o ano final

# Empilhando todos os resultados em um unico dataframe:
Decomp_2010_2019 <- result
Decomp_2010_2020 <- result
Decomp_2010_2021 <- result
Decomp_2010_2022 <- result

Decomp_idade <- rbind(Decomp_2010_2019, Decomp_2010_2020, 
                      Decomp_2010_2021, Decomp_2010_2022)


rm(list=setdiff(ls(), c('life.table.BR', 'life.table.SE', 'data', "Decomp_idade")))

setwd("C:/.../resultados") # inserir o caminho para os resultados
write_xlsx(Decomp_idade, 'Decomp_idade.xlsx')  


#--------------------------------# 
# DECOMPOSICAO POR IDADE E CAUSA #
#--------------------------------#

setwd("C:/.../data") # inserir o caminho para os dados de entrada
data_mort_cause.BR <- read_excel("Obitos_Brasil.xlsx", col_names = TRUE)

data_mort_cause.BR$Region <- 'BR'
tmp.BR <- data_mort_cause.BR %>%
  mutate(mx1adj=ObitCorrig_1/pop1, mx2adj=ObitCorrig_2/pop2) %>%
  select(Region, Period, Cause_death, Sex, Age.Group, mx1adj, mx2adj, ObitCorrig_1, ObitCorrig_2) %>%
  filter(Period!="2019/2020" & Period!="2019/2021" & Period!="2019/2022")

data_mort_cause.SE <- read_excel("Obitos_Semiarido.xlsx", col_names = TRUE)
data_mort_cause.SE$Region <- 'SE'
tmp.SE <- data_mort_cause.SE %>%
  mutate(mx1adj=ObitCorrig_1/pop1, mx2adj=ObitCorrig_2/pop2) %>%
  select(Region, Period, Cause_death, Sex, Age.Group, mx1adj, mx2adj, ObitCorrig_1, ObitCorrig_2) %>%
  filter(Period!="2019/2020" & Period!="2019/2021" & Period!="2019/2022")

# Empilhando os dados para Brazil e Semiarido:
tmp_cause <- rbind(tmp.BR, tmp.SE)
tmp_cause$Age.Group <- as.numeric(tmp_cause$Age.Group)

# Padronizando alguns labels:
tmp_cause <- tmp_cause %>%
  mutate(Cause_death = case_when(
    Cause_death == "All_causes" ~ "Todas as causas",
    Cause_death == "All_other_causes" ~ "Todas as outras causas",
    Cause_death == "Circulatory" ~ "Circulat?rias",
    Cause_death == "Covid" ~ "Covid-19",
    Cause_death == "External_causes" ~ "Causas externas",
    Cause_death == "Homicides" ~ "Homic?dios", 
    Cause_death == "Infec_parasit" ~ "Infecciosas e parasit?rias",
    Cause_death == "Neoplasms" ~ "Neoplasias",
    Cause_death == "Pregnancy" ~ "Gravidez, parto e puerp?rio",
    Cause_death == "Suicides" ~ "Suic?dios",
    TRUE ~ Cause_death))

tmp_cause <- tmp_cause %>%
  mutate(Sex = case_when(
    Sex == "Female" ~ "females",
    Sex == "Male" ~ "males", 
    TRUE ~ Sex))

# Juntando a base de decomposicao por idade com os dados de obitos:
tmp_age <- Decomp_idade %>%
  select(Region, Period, Sex, Age_group, TE) %>%
  arrange(Region, Period, Sex, Age_group)
colnames(tmp_age) <- c('Region', 'Period', 'Sex', 'Age.Group', 'TE')

tmp_age <- tmp_age %>%
  mutate(Region = case_when(
    Region == "SEMIARIDO" ~ "SE",
    TRUE ~ Region))

tmp_cause_age <- left_join(tmp_cause, tmp_age, by=c('Region','Period', 'Sex', 'Age.Group'))

all_causes <- tmp_cause_age %>% 
  filter(Cause_death=='Todas as causas') %>%
  select(Region, Period, Cause_death, Sex, Age.Group, ObitCorrig_1, ObitCorrig_2)
  
nCxi <- tmp_cause_age %>% 
  group_by(Region, Period, Sex, Age.Group) %>%
  mutate(all_cause1=sum(ObitCorrig_1)/2, all_cause2=sum(ObitCorrig_2)/2)


# Calcula a proporção de obitos devido a causa i:
nCxi <- nCxi %>% 
  group_by(Region, Period, Sex, Age.Group) %>%
  mutate(nRx1=ObitCorrig_1/all_cause1, nRx2=ObitCorrig_2/all_cause2)
  
nmx_all <- nCxi %>% 
  filter(Cause_death=='Todas as causas') %>%
  select(Region, Period, Cause_death, Sex, Age.Group, mx1adj, mx2adj)
colnames(nmx_all) <- c('Region', 'Period', 'Cause_death', 'Sex', 'Age.Group', 'nmx1', 'nmx2')

nCxi <- left_join(nCxi, nmx_all, by=c('Region','Period', 'Sex', 'Age.Group'))

nCxi_final <- nCxi %>% 
  mutate(ncxi=TE*(((nRx2*nmx2)-(nRx1*nmx1))/(nmx2-nmx1))) %>%
  arrange(Region, Period, Sex, Age.Group, Cause_death.x) %>%
  select(Region, Period, Sex, Age.Group, Cause_death.x, mx1adj, mx2adj, TE, nRx1, nRx2, nmx1,  nmx2, ncxi)
colnames(nCxi_final) <- c('Region', 'Period', 'Sex', 'Age.Group', 'Cause_death', 
                          'mx1adj', 'mx2adj', 'TE', 'nRx1', 'nRx2', 'nmx1',  'nmx2', 'ncxi')


setwd("C:/.../resultados") # inserir o caminho para os resultados

write_xlsx(nCxi_final, 'Decomp_Cause.xlsx')



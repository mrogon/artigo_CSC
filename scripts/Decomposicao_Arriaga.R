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

setwd("C:.../Dados de entrada") # inserir o caminho para os dados de entrada

#------------------------# 
# DECOMPOSICAO POR IDADE #
#------------------------#

# Tabuas de vida do Brasil:
life.table.BR <- read_excel("Tabua_De_Vida__BR_UFs_Corrig.xlsx",
                           col_names = TRUE)

life.table.BR <- life.table.BR %>%
  select(Age, Sex, Region, Year, lx, nLx, Tx, ex) %>%
  filter(Region=="BR")

# Tabuas de vida para o semiarido:
life.table.SE <- read_excel("Tabua_De_Vida_Semiarido.xlsx",
                            col_names = TRUE)

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
year2 <- 2022

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


# stacking result to a unique dataframe 
# must return to the beginning and change de the final year in each period):
Decomp_2010_2019 <- result
Decomp_2010_2020 <- result
Decomp_2010_2021 <- result
Decomp_2010_2022 <- result

Decomp_idade <- rbind(Decomp_2010_2019, Decomp_2010_2020, 
                      Decomp_2010_2021, Decomp_2010_2022)


rm(list=setdiff(ls(), c('life.table.BR', 'life.table.SE', 'data', "Decomp_idade")))


setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Resultados")
save.image("Arriaga_Brazil_Semiarido.RData")
write_xlsx(Decomp_idade, 'New.Decomp_idade.xlsx')  


#--------------------------------# 
# DECOMPOSITION BY CAUSE AND AGE #
#--------------------------------#

### Cause of death contribution   #########

# Preparing Brazil and Semiarido Input Data:
setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Dados de entrada")

data_mort_cause.BR <- read_excel("obitos_Brasil_Input_Quinquenal.xlsx",
                         col_names = TRUE)
data_mort_cause.BR$Region <- 'BR'

mort_cause.BR <- data_mort_cause.BR %>%
  mutate(mx1adj=ObitCorrig_1/pop1, mx2adj=ObitCorrig_2/pop2) %>%
  select(Region, Period, Cause_death, Sex, Age.Group, mx1adj, mx2adj) %>%
  filter(Period!="2019/2020" & Period!="2019/2021" & Period!="2019/2022")

data_mort_cause.SE <- read_excel("obitos_MunSemarido_Input_Quinquenal.xlsx",
                            col_names = TRUE)
data_mort_cause.SE$Region <- 'SE'

mort_cause.SE <- data_mort_cause.SE %>%
  mutate(mx1adj=ObitCorrig_1/pop1, mx2adj=ObitCorrig_2/pop2) %>%
  select(Region, Period, Cause_death, Sex, Age.Group, mx1adj, mx2adj) %>%
  filter(Period!="2019/2020" & Period!="2019/2021" & Period!="2019/2022")


# stacking Brazil and Semiarido data:
mort_cause <- rbind(mort_cause.BR, mort_cause.SE)
mort_cause$Age.Group <- as.numeric(mort_cause$Age.Group)

# Standardizing some variables labels:
mort_cause <- mort_cause %>%
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

mort_cause <- mort_cause %>%
  mutate(Sex = case_when(
    Sex == "Female" ~ "females",
    Sex == "Male" ~ "males", 
    TRUE ~ Sex))

# Difference In Rates:
mort_cause <- mort_cause %>%
  mutate(diff_rate_i=mx1adj-mx2adj) %>%
  arrange(Region, Period, Sex, Age.Group, Cause_death)

# proportional contribution of the difference in cause-specific mortality rates 
# to the difference in the all-cause mortality rate:
tmp <- Decomp_idade %>%
  select(Region, Period, Sex, Age_group, TE) %>%
  arrange(Region, Period, Sex, Age_group)
colnames(tmp) <- c('Region', 'Period', 'Sex', 'Age.Group', 'TE')

tmp <- tmp %>%
  mutate(Region = case_when(
    Region == "SEMIARIDO" ~ "SE",
    TRUE ~ Region))

#Cause_contr <- mort_cause %>%
#  select(Region, Period, Cause_death, Sex, Age_Group, mx1adj, mx2adj, diff_rate_i)

Cause_contr <- left_join(mort_cause, tmp, by=c('Region','Period', 'Sex', 'Age.Group'))

Diff_all_causes = Cause_contr %>%
  filter(Cause_death=='Todas as causas') %>%
  select(Region, Period, Sex, Age.Group, diff_rate_i)

colnames(Diff_all_causes) <- c('Region','Period', 'Sex', 'Age.Group', 'diff_all')

Cause_contr <- left_join(Cause_contr, Diff_all_causes, by=c('Region','Period', 'Sex', 'Age.Group'))

Cause_contr <- Cause_contr %>%
  mutate(Prop_contr=diff_rate_i/diff_all)

# Contribution of each cause to the estimated Total Effect in each age group:
Decomp_Cause <- Cause_contr %>%
  mutate(Cause_contr=Prop_contr*TE) %>%
  mutate(percent_contrib=Cause_contr/TE*100)


# Checking the result for a period, sex, and age group
teste1 = Decomp_Cause %>% 
  filter(Region=='SE', Period=='2010/2020', Sex=='females', diff_rate_i!='NA')
teste2 = teste1 %>% filter(Cause_death!='Todas as causas')
sum(teste2$Cause_contr)

write_xlsx(Decomp_Cause, 'New_Decomp_idade.xlsx')


#--------------------------------# 
# DECOMPOSITION BY CAUSE AND AGE # Following Auger et al (2014)
#--------------------------------#

setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Dados de entrada")

data_mort_cause.BR <- read_excel("obitos_Brasil_Input_Quinquenal.xlsx",
                                 col_names = TRUE)
data_mort_cause.BR$Region <- 'BR'
tmp.BR <- data_mort_cause.BR %>%
  mutate(mx1adj=ObitCorrig_1/pop1, mx2adj=ObitCorrig_2/pop2) %>%
  select(Region, Period, Cause_death, Sex, Age.Group, mx1adj, mx2adj, ObitCorrig_1, ObitCorrig_2) %>%
  filter(Period!="2019/2020" & Period!="2019/2021" & Period!="2019/2022")

data_mort_cause.SE <- read_excel("obitos_MunSemarido_Input_Quinquenal.xlsx",
                                 col_names = TRUE)
data_mort_cause.SE$Region <- 'SE'
tmp.SE <- data_mort_cause.SE %>%
  mutate(mx1adj=ObitCorrig_1/pop1, mx2adj=ObitCorrig_2/pop2) %>%
  select(Region, Period, Cause_death, Sex, Age.Group, mx1adj, mx2adj, ObitCorrig_1, ObitCorrig_2) %>%
  filter(Period!="2019/2020" & Period!="2019/2021" & Period!="2019/2022")

# stacking Brazil and Semiarido data:
tmp_cause <- rbind(tmp.BR, tmp.SE)
tmp_cause$Age.Group <- as.numeric(tmp_cause$Age.Group)

# Standardizing some variables labels:
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

# Merging Decomp_idade with death cause data:
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

# Testing if the sum of deaths by cause is equal to all deaths:
teste <- left_join(nCxi, all_causes, by=c('Region','Period', 'Sex', 'Age.Group')) %>%
  mutate(teste1=all_cause1-ObitCorrig_1.y, teste2=all_cause2-ObitCorrig_2.y)
sum(teste$teste1)
sum(teste$teste2)

# Proportion of deaths due to cause i:
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

# Checking the result for a region, period, and sex
teste1 = nCxi_final %>% 
  filter(Region=='SE', Period=='2010/2020', Sex=='females')
sum(teste1$ncxi)/2


setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Resultados")
save.image("New_Arriaga_Brazil_Semiarido.RData")

write_xlsx(nCxi_final, 'New_Decomp_Cause.xlsx')


#-------------------# 
# FIGURAS DO ARTIGO #
#-------------------#

# Figures 1 e 2 - Decomposition by age:

Decomp_idade$cor <- ifelse(Decomp_idade$TE < 0, "red", "steelblue")

# MALES
# Brazil 2010/2019:
df <- filter(Decomp_idade, Region=='BR', Sex=='males', Period=='2010/2019')
G1 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Grupo de Idade", title = "Brasil - 2010/2019 (ET = 2,3)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Brazil 2010/2020:
df <- filter(Decomp_idade, Region=='BR', Sex=='males', Period=='2010/2020')
G2 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Grupo de Idade", title = "Brasil - 2010/2020 (ET = 0,5)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Brazil 2010/2021:
df <- filter(Decomp_idade, Region=='BR', Sex=='males', Period=='2010/2021')
G3 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Grupo de Idade", title = "Brasil - 2010/2021 (ET = -1,3)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Brazil 2010/2022:
df <- filter(Decomp_idade, Region=='BR', Sex=='males', Period=='2010/2022')
G4 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "Contribui??o (em anos)", y = "Grupo de Idade", title = "Brasil - 2010/2022 (ET = 1,8)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))

# Semi?rido 2010/2019:
df <- filter(Decomp_idade, Region=='SEMIARIDO', Sex=='males', Period=='2010/2019')
G5 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2019 (ET = 1,6)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Semi?rido 2010/2020:
df <- filter(Decomp_idade, Region=='SEMIARIDO', Sex=='males', Period=='2010/2020')
G6 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2020 (ET = -0,2)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Semi?rido 2010/2021:
df <- filter(Decomp_idade, Region=='SEMIARIDO', Sex=='males', Period=='2010/2021')
G7 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2021 (ET = -0,9)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal()+ 
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Semi?rido 2010/2022:
df <- filter(Decomp_idade, Region=='SEMIARIDO', Sex=='males', Period=='2010/2022')
G8 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "Contribui??o (em anos)", y = "", title = "Semi?rido - 2010/2022 (ET = 0,6)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))

Figura1 <- ggarrange(G1, G5, G2, G6, G3, G7, G4, G8,
                     ncol=2, nrow=4)

setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Figuras/Novas")
ggsave("Figura_1_males.jpeg", 
       plot = Figura1,
       dpi = 200, width = 15, height = 18, units = "cm")


# FEMALES
# Brazil 2010/2019:
df <- filter(Decomp_idade, Region=='BR', Sex=='females', Period=='2010/2019')
G1 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Grupo de Idade", title = "Brasil - 2010/2019 (ET = 1,6)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Brazil 2010/2020:
df <- filter(Decomp_idade, Region=='BR', Sex=='females', Period=='2010/2020')
G2 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Grupo de Idade", title = "Brasil - 2010/2020 (ET = 0,5)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Brazil 2010/2021:
df <- filter(Decomp_idade, Region=='BR', Sex=='females', Period=='2010/2021')
G3 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Grupo de Idade", title = "Brasil - 2010/2021 (ET = -1,7)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Brazil 2010/2022:
df <- filter(Decomp_idade, Region=='BR', Sex=='females', Period=='2010/2022')
G4 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "Contribui??o (em anos)", y = "Grupo de Idade", title = "Brasil - 2010/2022 (ET = 1,0)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Semi?rido 2010/2019:
df <- filter(Decomp_idade, Region=='SEMIARIDO', Sex=='females', Period=='2010/2019')
G5 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2019 (ET = 1,2)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Semi?rido 2010/2020:
df <- filter(Decomp_idade, Region=='SEMIARIDO', Sex=='females', Period=='2010/2020')
G6 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2020 (ET = 0,3)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Semi?rido 2010/2021:
df <- filter(Decomp_idade, Region=='SEMIARIDO', Sex=='females', Period=='2010/2021')
G7 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2021 (ET = -0,9)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal()+ 
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))
# Semi?rido 2010/2022:
df <- filter(Decomp_idade, Region=='SEMIARIDO', Sex=='females', Period=='2010/2022')
G8 <- ggplot(df, aes(x = TE, y = factor(Age_group), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "Contribui??o (em anos)", y = "", title = "Semi?rido - 2010/2022 (ET = 0,4)") +
  scale_x_continuous(limits=c(-0.7, 0.6)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 7))

Figura2 <- ggarrange(G1, G5, G2, G6, G3, G7, G4, G8,
                     ncol=2, nrow=4)

setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Figuras/Novas")
ggsave("Figura_2_females.jpeg", 
       plot = Figura2,
       dpi = 200, width = 15, height = 18, units = "cm")



# Figuras 3 e 4 - Decomposi??o por grupo etario e causa:

# Agregating nCx,i by age groups of interest:
age.cat = function(age){
  result = NA
  first = (age %in% 0:9)
  result [first] = '0-9'
  second = (age %in% 10:19)
  result [second] = '10-19'
  third = (age %in% 20:39)
  result [third] = '20-39'
  fourth = (age %in% 40:64)
  result [fourth] = '40-64'
  fifth = (age %in% 65:79)
  result [fifth] = '65-79'
  sixth = (age %in% 80)
  result [sixth] = '80+'
  return(result)
}

data.tmp <- nCxi_final %>%
  mutate(Age.g=age.cat(Age.Group)) %>%
  group_by(Region, Period, Sex, Cause_death, Age.g) %>%
  summarise(ncxi=sum(ncxi))

# MALE
#Brazil(2010/2019):
df <- filter(data.tmp, Sex=='males', Region=='BR', Period=='2010/2019', 
             Cause_death!='Todas as causas'& Cause_death!='Gravidez, parto e puerp?rio')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G1 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "Causa de morte", title = "Brasil - 2010/2019 (ET = 2,3)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Brazil(2010/2020):
df <- filter(data.tmp, Sex=='males', Region=='BR', Period=='2010/2020', 
             Cause_death!='Todas as causas'& Cause_death!='Gravidez, parto e puerp?rio')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G2 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "Causa de morte", title = "Brasil - 2010/2020 (ET = 0,5)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Brazil(2010/2021):
df <- filter(data.tmp, Sex=='males', Region=='BR', Period=='2010/2021', 
             Cause_death!='Todas as causas'& Cause_death!='Gravidez, parto e puerp?rio')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G3 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "Causa de morte", title = "Brasil - 2010/2021 (ET = -1,3)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Brazil(2010/2022):
df <- filter(data.tmp, Sex=='males', Region=='BR', Period=='2010/2022', 
             Cause_death!='Todas as causas'& Cause_death!='Gravidez, parto e puerp?rio')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G4 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "Contribui??o por causa (em anos)", y = "Causa de morte", title = "Brasil - 2010/2022 (1,8)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2019):
df <- filter(data.tmp, Sex=='males', Region=='SE', Period=='2010/2019', 
             Cause_death!='Todas as causas'& Cause_death!='Gravidez, parto e puerp?rio')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G5 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "", title = "Semi?rido - 2010/2019 (ET = 1,6)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2020):
df <- filter(data.tmp, Sex=='males', Region=='SE', Period=='2010/2020', 
             Cause_death!='Todas as causas'& Cause_death!='Gravidez, parto e puerp?rio')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G6 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "", title = "Semi?rido - 2010/2020 (ET = -0,2)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2021):
df <- filter(data.tmp, Sex=='males', Region=='SE', Period=='2010/2021', 
             Cause_death!='Todas as causas'& Cause_death!='Gravidez, parto e puerp?rio')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G7 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "", title = "Semi?rido - 2010/2021 (ET= -0,9)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2022):
df <- filter(data.tmp, Sex=='males', Region=='SE', Period=='2010/2022', 
             Cause_death!='Todas as causas'& Cause_death!='Gravidez, parto e puerp?rio')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G8 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "Contribui??o por causa (em anos)", y = "", title = "Semi?rido - 2010/2022 (ET = 0,6)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))


Figura3 <- ggarrange(G1, G5, G2, G6, G3, G7, G4, G8,
                     ncol=2, nrow=4, common.legend = TRUE, legend='bottom')

setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Figuras/Novas")
ggsave("Figura_3_males.jpeg", 
       plot = Figura3,
       dpi = 200, width = 15, height = 18, units = "cm")


# FEMALE
#Brazil(2010/2019):
df <- filter(data.tmp, Sex=='females', Region=='BR', Period=='2010/2019', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G1 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "Causa de morte", title = "Brasil - 2010/2019 (ET = 1,6)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Brazil(2010/2020):
df <- filter(data.tmp, Sex=='females', Region=='BR', Period=='2010/2020', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G2 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "Causa de morte", title = "Brasil - 2010/2020 (ET = 0,5)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Brazil(2010/2021):
df <- filter(data.tmp, Sex=='females', Region=='BR', Period=='2010/2021', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G3 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "Causa de morte", title = "Brasil - 2010/2021 (ET = -1,3)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Brazil(2010/2022):
df <- filter(data.tmp, Sex=='females', Region=='BR', Period=='2010/2022', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G4 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "Contribui??o por causa (em anos)", y = "Causa de morte", title = "Brasil - 2010/2022 (ET = 1,0)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2019):
df <- filter(data.tmp, Sex=='females', Region=='SE', Period=='2010/2019', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G5 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "", title = "Semi?rido - 2010/2019 (ET = 1,2)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2020):
df <- filter(data.tmp, Sex=='females', Region=='SE', Period=='2010/2020', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G6 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "", title = "Semi?rido - 2010/2020 (ET = 0,3)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2021):
df <- filter(data.tmp, Sex=='females', Region=='SE', Period=='2010/2021', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G7 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "", title = "Semi?rido - 2010/2021 (ET = -0,9)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2022):
df <- filter(data.tmp, Sex=='females', Region=='SE', Period=='2010/2022', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regi?o', 'Per?odo', 'Sexo', 'Causa_morte', 'Grupo_Et?rio','ncxi')
G8 <- ggplot(df, aes(fill=Grupo_Et?rio, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "Contribui??o por causa (em anos)", y = "", title = "Semi?rido - 2010/2022 (ET = 0,4)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))


Figura4 <- ggarrange(G1, G5, G2, G6, G3, G7, G4, G8,
                     ncol=2, nrow=4, common.legend = TRUE, legend='bottom')

setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Figuras/Novas")
ggsave("Figura_4_females.jpeg", 
       plot = Figura4,
       dpi = 200, width = 15, height = 18, units = "cm")


rm(list=setdiff(ls(), c('life.table.BR', 'life.table.SE', 'data', "Decomp_idade",
                        'Decomp_Cause', 'mort_cause.BR', 'mort_cause.BR', 'nCxi_final',)))



#----------------------# 
# MATERIAL SUPLEMENTAR #
#----------------------#

# Material suplementar 1: log das taxas de mortalidade por sexo (Brasil x Semiárido)

setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Dados de entrada")

lt.semiarido <- read_excel("Tabua_De_Vida_Semiarido.xlsx",
                           col_names = TRUE)

lt.brasil <- read_excel("Tabua_De_Vida__BR_UFs_Corrig.xlsx",
                        col_names = TRUE)

tmp <- lt.semiarido %>%
  select(Age, Year, Sex, mx)
df <- filter(tmp, Sex=='Female')
G1 <- ggplot(df, aes(x=Age, y=log(mx), group=as.factor(Year))) +
  geom_line(aes(linetype=as.factor(Year)))+
  ylab("Log(mx)") +
  xlab("Idade") +
  ggtitle("Mulheres (Semi?rido)") +
  theme_minimal()+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        title = element_text(size = 7))

tmp <- lt.brasil %>%
  select(Age, Region, Year, Sex, mx) %>%
  filter(Region=='BR')
df <- filter(tmp, Sex=='Female') 
G2 <- ggplot(df, aes(x=Age, y=log(mx), group=as.factor(Year))) +
  geom_line(aes(linetype=as.factor(Year)))+
  ylab("Log(mx)") +
  xlab("Idade") +
  ggtitle("Mulheres (Brasil)") +
  theme_minimal()+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        title = element_text(size = 7))

tmp <- lt.semiarido %>%
  select(Age, Year, Sex, mx)
df <- filter(tmp, Sex=='Male') 
G3 <- ggplot(df, aes(x=Age, y=log(mx), group=as.factor(Year))) +
  geom_line(aes(linetype=as.factor(Year)))+
  ylab("Log(mx)") +
  xlab("Idade") +
  ggtitle("Homens (Semi?rido)") +
  theme_minimal()+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        title = element_text(size = 7)) 

tmp <- lt.brasil %>%
  select(Age, Region, Year, Sex, mx) %>%
  filter(Region=='BR')
df <- filter(tmp, Sex=='Male')
G4 <- ggplot(df, aes(x=Age, y=log(mx), group=as.factor(Year))) +
  geom_line(aes(linetype=as.factor(Year)))+
  ylab("Log(mx)") +
  xlab("Idade") +
  ggtitle("Homens (Brasil)") +
  theme_minimal()+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        title = element_text(size = 7))

Material_suplementar_1 <- ggarrange(G1, G2, G3, G4, ncol=2, nrow=2, 
                     common.legend = TRUE, legend='right')

setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Figuras/Novas")
ggsave("Material_suplementar_1.jpeg", 
       plot = Material_suplementar_1,
       dpi = 200, width = 15, height = 13, units = "cm")


# Material Suplementar 3 and 4 - Contribuicao por causa:

Decomp_Cause$cor <- ifelse(Decomp_Cause$Cause_contr < 0, "red", "steelblue")

# MALES
# Brazil 2010/2019:
df <- Decomp_Cause %>% 
  filter(Region=='BR', Sex=='males', Period=='2010/2019', 
         Cause_death!='Todas as causas' & Cause_death!='Gravidez, parto e puerp?rio') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G1 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Causa de Morte", title = "Brasil - 2010/2019 (ET = 2,3)") +
  scale_x_continuous(limits=c(-3.6, 1)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Brazil 2010/2020:
df <- Decomp_Cause %>% 
  filter(Region=='BR', Sex=='males', Period=='2010/2020', 
         Cause_death!='Todas as causas' & Cause_death!='Gravidez, parto e puerp?rio') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G2 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Causa de Morte", title = "Brasil - 2010/2020 (ET = 0,5)") +
  scale_x_continuous(limits=c(-3.6, 1)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Brazil 2010/2021:
df <- Decomp_Cause %>% 
  filter(Region=='BR', Sex=='males', Period=='2010/2021', 
         Cause_death!='Todas as causas' & Cause_death!='Gravidez, parto e puerp?rio') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G3 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Causa de Morte", title = "Brasil - 2010/2021 (ET = -1,3)") +
  scale_x_continuous(limits=c(-3.6, 1)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Brazil 2010/2022:
df <- Decomp_Cause %>% 
  filter(Region=='BR', Sex=='males', Period=='2010/2022', 
         Cause_death!='Todas as causas' & Cause_death!='Gravidez, parto e puerp?rio') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G4 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "Contribui??o (em anos)", y = "Causa de Morte", title = "Brasil - 2010/2022 (ET = 1,8)") +
  scale_x_continuous(limits=c(-3.6, 1)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Semiarido 2010/2019:
df <- Decomp_Cause %>% 
  filter(Region=='SE', Sex=='males', Period=='2010/2019', 
         Cause_death!='Todas as causas' & Cause_death!='Gravidez, parto e puerp?rio') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G5 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2019 (ET = 1,6)") +
  scale_x_continuous(limits=c(-3.6, 1)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Semiarido 2010/2020:
df <- Decomp_Cause %>% 
  filter(Region=='SE', Sex=='males', Period=='2010/2020', 
         Cause_death!='Todas as causas' & Cause_death!='Gravidez, parto e puerp?rio') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G6 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2020 (ET = -0,2)") +
  scale_x_continuous(limits=c(-3.6, 1)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Semiarido 2010/2021:
df <- Decomp_Cause %>% 
  filter(Region=='SE', Sex=='males', Period=='2010/2021', 
         Cause_death!='Todas as causas' & Cause_death!='Gravidez, parto e puerp?rio') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G7 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2021 (ET = -0,9)") +
  scale_x_continuous(limits=c(-3.6, 1)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Semiarido 2010/2022:
df <- Decomp_Cause %>% 
  filter(Region=='SE', Sex=='males', Period=='2010/2022', 
         Cause_death!='Todas as causas' & Cause_death!='Gravidez, parto e puerp?rio') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G8 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "Contribui??o (em anos)", y = "", title = "Semi?rido - 2010/2022 (ET = 0,6)") +
  scale_x_continuous(limits=c(-3.6, 1)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))

Figura3 <- ggarrange(G1, G5, G2, G6, G3, G7, G4, G8,
                     ncol=2, nrow=4)

setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Figuras/Novas")
ggsave("Figura_3_males.jpeg", 
       plot = Figura3,
       dpi = 300, width = 30, height = 40, units = "cm")


# FEMALES
# Brazil 2010/2019:
df <- Decomp_Cause %>% 
  filter(Region=='BR', Sex=='females', Period=='2010/2019', Cause_death!='Todas as causas') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G1 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Causa de Morte", title = "Brasil - 2010/2019 (ET = 1,6)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Brazil 2010/2020:
df <- Decomp_Cause %>% 
  filter(Region=='BR', Sex=='females', Period=='2010/2020', Cause_death!='Todas as causas') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G2 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Causa de Morte", title = "Brasil - 2010/2020 (ET = 0,5)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Brazil 2010/2021:
df <- Decomp_Cause %>% 
  filter(Region=='BR', Sex=='females', Period=='2010/2021', Cause_death!='Todas as causas') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G3 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "Causa de Morte", title = "Brasil - 2010/2021 (ET = -1,7)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Brazil 2010/2022:
df <- Decomp_Cause %>% 
  filter(Region=='BR', Sex=='females', Period=='2010/2022', Cause_death!='Todas as causas') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G4 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "Contribui??o (em anos)", y = "Causa de Morte", title = "Brasil - 2010/2022 (ET = 1,0)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Semiarido 2010/2019:
df <- Decomp_Cause %>% 
  filter(Region=='SE', Sex=='females', Period=='2010/2019', Cause_death!='Todas as causas') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G5 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2019 (ET = 1,2)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Semiarido 2010/2020:
df <- Decomp_Cause %>% 
  filter(Region=='SE', Sex=='females', Period=='2010/2020', Cause_death!='Todas as causas') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G6 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2020 (ET = 0,3)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Semiarido 2010/2021:
df <- Decomp_Cause %>% 
  filter(Region=='SE', Sex=='females', Period=='2010/2021', Cause_death!='Todas as causas') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G7 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "", y = "", title = "Semi?rido - 2010/2021 (ET = -0,9)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))
# Semiarido 2010/2022:
df <- Decomp_Cause %>% 
  filter(Region=='SE', Sex=='females', Period=='2010/2022', Cause_death!='Todas as causas') %>%
  select(Cause_death, Cause_contr) %>%
  group_by(Cause_death) %>%
  summarise(Cause_contr=sum(Cause_contr))
df$cor <- ifelse(df$Cause_contr < 0, "red", "steelblue")
sum(df$Cause_contr)
G8 <- ggplot(df, aes(x = Cause_contr, y = factor(Cause_death), fill = cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Usa as cores diretamente da coluna "cor"
  labs(x = "Contribui??o (em anos)", y = "", title = "Semi?rido - 2010/2022 (ET = 0,4)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11),
        title = element_text(size = 13))

Figura4 <- ggarrange(G1, G5, G2, G6, G3, G7, G4, G8,
                     ncol=2, nrow=4)

setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Figuras/Novas")
ggsave("Figura_4_females.jpeg", 
       plot = Figura4,
       dpi = 300, width = 30, height = 40, units = "cm")


# Material suplementar 5: Log das taxas de mortalidade (Brasil x Semiarido) por sexo (2010)

tmp1 <- lt.semiarido %>%
  select(Age, Region, Year, Sex, mx) %>%
  filter(Year==2010)

tmp2 <- lt.brasil %>%
  select(Age, Region, Year, Sex, mx) %>%
  filter(Year==2010, Region=='BR')

df1 <- filter(tmp1, Sex=='Female')  
df2 <- filter(tmp2, Sex=='Female') 
G1 <- ggplot() +
  geom_line(data=df1, aes(x=Age, y=log(mx), colour='Semi?rido'), size=1.5)+
  geom_line(data=df2, aes(x=Age, y=log(mx), colour='Brasil'), size=1.5)+
  ylab("Log(mx)") +
  xlab("Idade") +
  ggtitle("Mulheres") +
  scale_color_manual(name = "Regi?o", values = c("Semi?rido" = "darkblue", "Brasil" = "darkred")) +
  theme_minimal()+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        title = element_text(size = 7))

df1 <- filter(tmp1, Sex=='Male')  
df2 <- filter(tmp2, Sex=='Male') 
G2 <- ggplot() +
  geom_line(data=df1, aes(x=Age, y=log(mx), colour='Semi?rido'), size=1.5)+
  geom_line(data=df2, aes(x=Age, y=log(mx), colour='Brasil'), size=1.5)+
  ylab("Log(mx)") +
  xlab("Idade") +
  ggtitle("Homens") +
  scale_color_manual(name = "Regi?o", values = c("Semi?rido" = "darkblue", "Brasil" = "darkred")) +
  theme_minimal()+
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        title = element_text(size = 7))


Material_suplementar_5 <- ggarrange(G1, G2, ncol=2, nrow=1, 
                                    common.legend = TRUE, legend='right')

setwd("C:/Users/Marcos/OneDrive/PROJETOS/Covid_CNPq_Universal_2021/Projeto_CNPq_Brasil_Mexico/PRODU??ES/Artigo_seminarido/Figuras/Novas")
ggsave("Material_suplementar_5.jpeg", 
       plot = Material_suplementar_5,
       dpi = 200, width = 15, height = 10, units = "cm")





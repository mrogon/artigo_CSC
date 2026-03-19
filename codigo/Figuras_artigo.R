#-------------------# 
# FIGURAS DO ARTIGO #
#-------------------#

# FiguRAS 1 e 2 - Decomposicao por idade:

setwd("C:/.../data") # inserir o caminho para os dados de entrada

Decomp_idade <- read_excel("Decomp_idade.xlsx", col_names = TRUE)

Decomp_idade$cor <- ifelse(Decomp_idade$TE < 0, "red", "steelblue")

# HOMENS:
# Brasil 2010/2019:
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
# Brasil 2010/2020:
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
# Brasil 2010/2021:
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
# Brasil 2010/2022:
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

# Semiarido 2010/2019:
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
# Semiarido 2010/2020:
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
# Semiarido 2010/2021:
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
# Semiarido 2010/2022:
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

setwd("C:/.../Figuras Artigo")
ggsave("Figura_1_homens.jpeg", 
       plot = Figura1,
       dpi = 200, width = 15, height = 18, units = "cm")


# MULHERES
# Brasil 2010/2019:
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

setwd("C:/.../Figuras Artigo")
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
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G1 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
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
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G2 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
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
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G3 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
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
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G4 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
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
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G5 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
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
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G6 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
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
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G7 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
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
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G8 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
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

setwd("C:/.../Figuras Artigo")
ggsave("Figura_3_males.jpeg", 
       plot = Figura3,
       dpi = 200, width = 15, height = 18, units = "cm")


# MULHERES
#Brasil(2010/2019):
df <- filter(data.tmp, Sex=='females', Region=='BR', Period=='2010/2019', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G1 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "Causa de morte", title = "Brasil - 2010/2019 (ET = 1,6)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Brasil(2010/2020):
df <- filter(data.tmp, Sex=='females', Region=='BR', Period=='2010/2020', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G2 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "Causa de morte", title = "Brasil - 2010/2020 (ET = 0,5)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Brasil(2010/2021):
df <- filter(data.tmp, Sex=='females', Region=='BR', Period=='2010/2021', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G3 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "Causa de morte", title = "Brasil - 2010/2021 (ET = -1,3)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Brasil(2010/2022):
df <- filter(data.tmp, Sex=='females', Region=='BR', Period=='2010/2022', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G4 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "Contribuicao por causa (em anos)", y = "Causa de morte", title = "Brasil - 2010/2022 (ET = 1,0)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2019):
df <- filter(data.tmp, Sex=='females', Region=='SE', Period=='2010/2019', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G5 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "", title = "Semiarido - 2010/2019 (ET = 1,2)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2020):
df <- filter(data.tmp, Sex=='females', Region=='SE', Period=='2010/2020', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G6 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "", title = "Semiarido - 2010/2020 (ET = 0,3)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2021):
df <- filter(data.tmp, Sex=='females', Region=='SE', Period=='2010/2021', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G7 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "", y = "", title = "Semiarido - 2010/2021 (ET = -0,9)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))
#Semiarido(2010/2022):
df <- filter(data.tmp, Sex=='females', Region=='SE', Period=='2010/2022', 
             Cause_death!='Todas as causas')
colnames(df) <- c('Regiao', 'Periodo', 'Sexo', 'Causa_morte', 'Grupo_Etario','ncxi')
G8 <- ggplot(df, aes(fill=Grupo_Etario, y=Causa_morte, x=ncxi)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs(x = "Contribuicao por causa (em anos)", y = "", title = "Semiarido - 2010/2022 (ET = 0,4)") +
  scale_x_continuous(limits=c(-3.6, 1.2)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 6))


Figura4 <- ggarrange(G1, G5, G2, G6, G3, G7, G4, G8,
                     ncol=2, nrow=4, common.legend = TRUE, legend='bottom')

setwd("C:/.../Figuras Artigo")
ggsave("Figura_4_females.jpeg", 
       plot = Figura4,
       dpi = 200, width = 15, height = 18, units = "cm")




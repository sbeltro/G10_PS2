################################################################################
# Problem Set 2: Predicting Poverty
# Autores: Natalia Capacho, Yurani Gonzalez, Sebastian Beltran
# Big Data - MECA 
################################################################################

# Limpiar el espacio de trabajo ----
rm(list=ls())

# Instalar Paquetes y cargar librerias ----
if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(rio,
       tidyverse,
       skimr,
       dplyr,
       leaps,
       ROCR,
       glmnet,
       tableone,
       writexl,
       devtools,
       ggpubr,
       caret)

# Datos train ----
#  ** Personas ---- 

# Cargar la base de datos
train_personas <- readRDS("stores/dataPS2RDS/train_personas.rds")

#    *** Limpieza la base ----
train_personas <- train_personas %>%
  mutate(Impa = as.numeric(Impa),
         Impa = ifelse(test = is.na(Impa)==T | Impa==0,
                       yes  = Impaes,
                       no   = Impa))

recode_vars = c("Impa", "Oc", "Oficio", "P7040")

train_personas <- as.data.frame(train_personas)
for (i in recode_vars){
  train_personas[,i] = ifelse(test = is.na(train_personas[,i]) == T , yes = 0 , train_personas[,i])
}

train_personas <- as_tibble(train_personas)

train_personas <- train_personas %>% 
  mutate(mujer = case_when(P6020 == 1 ~ 0, 
                           P6020 == 2 ~ 1))

# Recodificar variables
variables_categoricas <- c("mujer", "Oc", "P7040")

train_personas <- as.data.frame(train_personas)
for (i in variables_categoricas){
  train_personas[,i] = as.numeric(train_personas[,i])
  train_personas[,i] = as.logical(train_personas[,i])
}

train_personas <- train_personas %>%
  mutate(P6050   = factor(P6050, levels = c(1, 2, 3, 4, 5, 6, 9)),
         P6090   = factor(P6090, levels = c(1, 2, 9)),
         P6100   = factor(P6100, levels = c(1, 2, 3, 9)),
         P6210   = factor(P6210, levels = c(1, 2, 3, 4, 5, 6, 9)),
         Oficio  = factor(Oficio, levels = c(1:99)),
         P6585s3 = factor(P6585s3, levels = c(1, 2, 9)),
         P6585s4 = factor(P6585s4, levels = c(1, 2, 9)),
         P7510s1 = factor(P7510s1, levels = c(1, 2, 9)),
         P7510s2 = factor(P7510s2, levels = c(1, 2, 9)),
         P7510s3 = factor(P7510s3, levels = c(1, 2, 9)),
         P6870   = factor(P6870, levels = c(1:9)))

train_personas <- train_personas %>%
  mutate(P6210 = ifelse(test = is.na(P6210)==T,
                        yes  = 9,
                        no   = P6210))

train_personas <- train_personas %>% 
  mutate(educ = case_when(P6210 == 1 | P6210 == 9 ~ 0, 
                          P6210 == 2 ~ 3, 
                          P6210 == 3 ~ 8, 
                          P6210 == 4 ~ 12,
                          P6210 == 5 ~ 16,
                          P6210 == 6 ~ 20))

train_personas <- train_personas %>%
  mutate(jefeHogar = case_when(P6050 == 1 ~ 1, 
                               P6050 == 2 | P6050 == 3 | P6050 == 4 | P6050 == 5 | 
                                 P6050 == 6 | P6050 == 9 | is.na(P6050) == T ~ 0))

train_personas <- train_personas %>%
  mutate(subsidiado = case_when(P6100 == 3 ~ 1, 
                                P6100 == 1 | P6100 == 2 | P6100 == 9 | is.na(P6100) == T ~ 0))

train_personas <- train_personas %>% 
  mutate(formal = case_when(P6090 == 1 ~ 1, 
                            P6090 == 2 | P6090 == 9 | is.na(P6090) == T ~ 0))

train_personas <- train_personas %>% 
  mutate(microEmpresa = case_when(P6870 == 1 | P6870 == 2 | P6870 == 3 | P6870 == 4 ~ 1,
                                  P6870 == 5 | P6870 == 6 | P6870 == 7 | P6870 == 8 | 
                                    P6870 == 9 | is.na(P6870) == T ~ 0))

train_personas <- train_personas %>% 
  mutate(ayudaHogares = ifelse(P7510s1 == 1 | P7510s2 == 1,
                               yes = 1,
                               no  = 0))

train_personas <- train_personas %>% 
  mutate(ayudaInstituciones = ifelse(P7510s3 == 1,
                                     yes = 1,
                                     no  = 0))

# Construir edad al cuadrado 
train_personas <- train_personas %>% 
  mutate(edad2 = P6040^2)

train_personas <- train_personas %>%
  mutate(educ = ifelse(test = is.na(educ) == T,
                       yes = 0,
                       no = educ))

# Construir variable experiencia laboral (experiencia potencial)
train_personas <- train_personas %>% 
  mutate(experiencia  = case_when(P6040 <= 18 ~ 0,
                                  educ == 0 & P6040 > 18 ~ P6040 - 18,
                                  educ > 0 & P6040 >= 18 & P6040 <= 22 ~ P6040 - 18, 
                                  educ > 0 & P6040 > 22 ~ P6040 - educ - 6),
         experiencia2 = experiencia^2)

train_personas <- train_personas %>%
  mutate(experiencia  = ifelse(test = experiencia < 0,
                               yes  = 0,
                               no   = experiencia),
         experiencia2 = ifelse(test = experiencia2 < 0,
                               yes  = 0,
                               no   = experiencia2))

# Renombrar variables 
colnames(train_personas)[which(colnames(train_personas)=="Impa")]    = "y_laboral"
colnames(train_personas)[which(colnames(train_personas)=="P6040")]   = "edad"
colnames(train_personas)[which(colnames(train_personas)=="P6585s3")] = "subFamiliar"
colnames(train_personas)[which(colnames(train_personas)=="P6585s4")] = "subEducativo"
colnames(train_personas)[which(colnames(train_personas)=="P7040")]   = "segundoTrabajo"
colnames(train_personas)[which(colnames(train_personas)=="Oc")]      = "Ocupado"

train_personas <- train_personas %>%
  mutate(mujer2           = ifelse(test = mujer == T, yes = 1, no  = 0),
         formal2          = ifelse(test = formal == T, yes = 1, no  = 0),
         jefeHogar2       = ifelse(test = jefeHogar == T, yes = 1, no  = 0),
         microEmpresa2    = ifelse(test = microEmpresa == T, yes = 1, no  = 0),
         Ocupado2         = ifelse(test = Ocupado == T, yes = 1, no  = 0),
         segundoTrabajo2  = ifelse(test = segundoTrabajo == T, yes = 1, no  = 0),
         jefeHogar_mujer2 = jefeHogar2 * mujer2)

recode_vars2 = c("educ", "ayudaHogares", "ayudaInstituciones", "segundoTrabajo2", "Oficio")

train_personas <- as.data.frame(train_personas)
for (i in recode_vars2){
  train_personas[,i] = ifelse(test = is.na(train_personas[,i]) == T,
                              yes = 0, 
                              train_personas[,i])
}
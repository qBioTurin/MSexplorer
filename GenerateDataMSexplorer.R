library(readr)
library(dplyr)
setwd("~/Desktop/GIT/MSmicro/MSexplorerLOCAL")
diversity_score_perKMEvolution2 <- read.table("data/diversity_score_perKMEvolution.txt", sep="\t", header = TRUE)
diversity_score_perKMEvolution <- read.table("data/diversity_score_perKM.txt", sep="\t", header = TRUE)

colnames(diversity_score_perKMEvolution)

diversity_score_perKMEvolution$DAY_T0 <- as.Date(diversity_score_perKMEvolution$DAY_T0,format = "%d/%m/%y")
diversity_score_perKMEvolution$EDSS_WORSENING_DAY <- as.Date(diversity_score_perKMEvolution$EDSS_WORSENING_DAY,format = "%d/%m/%y")
diversity_score_perKMEvolution$NEW_RELAPSE_OR_NEW_LESIONS_DAY <- as.Date(diversity_score_perKMEvolution$NEW_RELAPSE_OR_NEW_LESIONS_DAY,format = "%d/%m/%y")
diversity_score_perKMEvolution$WORSENING.DAY <- as.Date(diversity_score_perKMEvolution$WORSENING.DAY,format = "%d/%m/%y")

diversity_score_perKMEvolution <- diversity_score_perKMEvolution %>%
  mutate(WORSENING.DAY = if_else(is.na(WORSENING.DAY) & WORSENING == 0 , EDSS_WORSENING_DAY, WORSENING.DAY))

df = diversity_score_perKMEvolution2 %>% 
  select(id,EDSS_DIAGNOSI,EDSS_PROGRESSIONE)

merged_df=  diversity_score_perKMEvolution %>% 
  select(id,WORSENING,DAY_T0,WORSENING.DAY)%>%
  mutate(Event = WORSENING, EventTime = as.numeric(difftime(WORSENING.DAY, DAY_T0, units = "weeks")))

merged_df = merge(merged_df,df)
metadata <- readr::read_delim("./data/Bacteria_alpha_metadata.csv", 
                              delim = ",", escape_double = FALSE, trim_ws = TRUE)


metadataUpdae = merge(metadata , merged_df %>% select(-WORSENING.DAY,-DAY_T0),all = T )

colnames(metadataUpdae)

write_csv(x = metadataUpdae,
          file = "./www/Bacteria_alpha_metadataUpdate.csv")

#### Updating data to uplaod

# Load data

# metadata <- readr::read_delim("./www/Bacteria_alpha_metadata.csv", 
#                               delim = ",", escape_double = FALSE, trim_ws = TRUE)
metadata <- readr::read_csv("www/Bacteria_alpha_metadataUpdate.csv")
# Convert categorical variables to factors
metadata$gc_treatment <- as.factor(metadata$gc_treatment)
metadata$category <- as.factor(metadata$category)
metadata$lesion_burden <- as.factor(metadata$lesion_burden)
levels(metadata$lesion_burden) <- c("Low", "High")

metadata$bone_marrow_lesions <- as.factor(metadata$bone_marrow_lesions)
levels(metadata$bone_marrow_lesions) <- c("Low", "High")

metadata$gadolinium_contrast <- as.factor(metadata$gadolinium_contrast)
levels(metadata$gadolinium_contrast) <- c("GAD-", "GAD+")

metadata$subtentorial_lesions <- as.factor(metadata$subtentorial_lesions)
levels(metadata$subtentorial_lesions) <- c("No", "Yes")

metadata = metadata %>% mutate(EDSS_PROGRESSIONE = ifelse(WORSENING == 0, EDSS_DIAGNOSI, EDSS_PROGRESSIONE) )

write_csv(metadata %>%
            select(id, sex, age, bmi, category, clinical_presentation, gc_treatment, subtentorial_lesions, 
                   bone_marrow_lesions, gadolinium_contrast, lesion_burden, 
                   WORSENING,EDSS_DIAGNOSI,EDSS_PROGRESSIONE,Event, EventTime ) %>%
            rename(spinal_cord = bone_marrow_lesions),file = "../MSexplorerWorkflow/InputData/metadata.csv")



# Select relevant columns
metadata <- metadata %>%
  select(id, sex,clinical_presentation, gc_treatment, subtentorial_lesions, 
         bone_marrow_lesions, gadolinium_contrast, lesion_burden, 
         WORSENING,EDSS_DIAGNOSI,EDSS_PROGRESSIONE,Event, EventTime ) %>%
  rename(spinal_cord = bone_marrow_lesions)



create_data <- function(file, type) {
  data_new <- read.csv(file)
  rownames(data_new) <- data_new$X
  data_new <- data_new[, -1]
  data_new <- t(data_new)
  data_new <- as.data.frame(data_new)
  data_new <- data_new %>% mutate(across(everything(), as.numeric))
  
  colnames(data_new) <- paste0(type, "_", colnames(data_new))
  data_new <- data_new %>% mutate(id = rownames(.))
  
  return(data_new)
}

create_data_new <- function(file, type) {
  data_new <- read.csv(file)
  data_new <- data_new[, -1]
  data_new <- data_new %>% tidyr::gather(-X,-Group,value = "Value", key = "Patient") %>%
    rename(Type = X)%>%
    mutate(Value = as.numeric(Value))
  
  data_new$MetricType = paste0(type, "_", data_new$Type)
  
  return(data_new)
}

# Load all CSV files
files <- list.files(path = "./www/csv/OLD", pattern = "\\.csv$", full.names = TRUE)

# Apply function to load and process data

DataList <- lapply(files, function(f) {
  type <- gsub(x = basename(f), pattern = ".csv", replacement = "")
  df = create_data(f, type)
  #df %>% arrange(id) %>% select(-id)
  df
})
names(DataList) = gsub(x = files, pattern = "(./www/csv/)|(_alpha.csv)|(ALE.csv)",replacement = "") #c("simpson","EH","shannon")
AllData <- merge(DataList[[3]], merge(DataList[[1]], DataList[[2]]))
rownames(AllData) <- AllData$id
OLDData <- AllData  %>% tidyr::gather(-id,value = "Value", key = "Alpha") %>%
  mutate(Value = as.numeric(Value), Subset = "OLD", Method = "" ) %>%
  rename(Patient = id) %>%
  mutate(Discriminant = gsub(x=Alpha, pattern = "[a-z]+_alpha_", replacement = "" ),
         Alpha = gsub(x=Alpha, pattern = "(_alpha_Spinal_Cord)|(_alpha_Lesion)|(_alpha_Subtentorial)|(_alpha_Gadolinium)$", replacement = "" ),
         Alpha = if_else(Alpha =="simpson", "Simpson",if_else(Alpha =="shannon", "Shannon","EH")) )

# f = "www/csv/AlphaFusedTable.csv"
# 
# data_new <- read.csv(f)
# data_new <- data_new %>% tidyr::gather(-Alpha,-Method,-Subset,-Discriminant,value = "Value", key = "Patient") %>%
#   mutate(Value = as.numeric(Value), Subset = paste(Subset), Patient = gsub(x=Patient, pattern = "_[1-9]+$", replacement = ""))
# 
# data_new=rbind(data_new,OLDData[,colnames(data_new)])

library(dplyr)

#fusedDASalpha <- readxl::read_excel("www/csv/NewfusedDASalpha.xlsx")
#NewfusedDASalpha <- readr::read_table("~/Desktop/GIT/MSmicro/MSexplorerWorkflow/Output/DAS_ALPHA/merged_alpha.tsv")
fusedDASalpha <-  readr::read_table("data/merged_alpha.tsv")
data_new <- fusedDASalpha %>% as.data.frame() %>%
  tidyr::gather(-Alpha,-Method,-Subset,-Discriminant,value = "Value", key = "Patient") %>%
  mutate(Value = as.numeric(Value), Subset = paste(Subset))

data_new = data_new %>% mutate(Alpha= if_else(Alpha =="Shannon", "Entropy",Alpha) ) 
saveRDS(data_new %>% filter(Method == "Both") %>% select(-Method),
        file = "../MSexplorer/www/Data.Rds" )
saveRDS(metadata, file = "../MSexplorer/www/metadata.Rds" )




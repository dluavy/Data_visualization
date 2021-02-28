#################################################################################################################################
#### multi-version_data_comparison.R
#### Descriptions: This script includes functions to generate plots for different comparison scenario and functions to prepare data to
#### call those plotting functions 
#### Author: Dan Lu, 05/2020
#### Note: there are multiple output formats available: 
#### 1. "across_datasets": comparison of the two versions of the age trend of estimates for each cancer and sex across different cancer registries and datasets within a country
#### 2. "within_dataset_country": diagnostic plots for different pipeline estimate versions for each dataset within a country
#### 3. "within_dataset": comparison of the two versions of the age trend of estimates of each cancer and sex across different countries for one dataset
#### args: "across_datasets"
####       "within_dataset_country"
####       "within_dataset"
###############################################################################################################################
## Setup ##
message("Setting up environment...")
rm(list=ls())

#loading packages
library(haven)
library(data.table)
library(dplyr)
library(ggplot2)

#set up file path
prep_step_A <- ""
prep_step_B <- ""

version_A <- ""
version_B <- ""
formats <- c("within_dataset") # could be "across_datasets", "within_dataset_country", "within_dataset"
plot_save_path <- ""
refresh_folder_B <- "" #specify data B folder if available 
#refresh_folder_B <- ""
datatype <- c("") #parameters: "inc", "mor"
main_path <- ""
locations <- fread("")
registries <- fread("") %>% select(registry_index, country_id)
loc <- merge(registries, locations, by.x = "country_id", by.y = "parent_id", all.x = T)
age_groups <- fread("") %>% select(cancer_age_group_id, age_group_name) %>%  na.omit() %>% as.data.table()
age_levels <- c()
#setup color template
colors <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
            "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#FFD92F",
            "#66C2A5", "#1B9E77", "#F781BF", "#F0027F", "#666666", "#B3B3B3", "#E6AB02", "#B15928")
plot_datasets <- c() #dataset identifiers
subnational_dataset_ids <- c() #subnaitonal dataset identifier

##load functions 
#Comparison of the two versions of the age trend of estimates for each cancer and sex across different cancer registries and datasets within a country
across_datasets <- function(data_table){
    message("Aggregate across datasets and countries")
    plot_df <- data_table[, -c("age_group_name")]
    plot_df <- plot_df[, age := as.character(age)]
    plot_df <- melt(plot_df, id.vars = c("dataset_id","country_id", "parent_name", "sex", "age", "acause"))
    setnames(plot_df, c("variable", "value"), c("version", "counts")) 
    #generate scatter plots for each country
    countries <- unique(plot_df$parent_name)
    for (c in countries){
        dt <- plot_df[parent_name == c,]
        tmp <- unique(dt[,.(parent_name, dataset_id, acause)])
        page <- ceiling(nrow(tmp)/36)
        temp <- list()
        #function to plot 36 subplots one page
        toplot <- function(df){
            for (p in seq_len(page)){
                if(p*36 < nrow(df[,.(dataset_id, acause)])){
                    acause <- list(dataset_id = df[((p-1)*36 +1):(p*36), ]$dataset_id,
                    acause = df[((p-1)*36 +1):(p*36), ]$acause)
                }else{
                    acause <- list(dataset_id = df[((p-1)*36 +1):nrow(df[,.(dataset_id, acause)]), ]$dataset_id,
                    acause = df[((p-1)*36 +1):nrow(df[,.(dataset_id, acause)]), ]$acause)}
            temp[[p]] <- acause
      }
      return (temp)
    }
    toplot <- toplot(tmp)
    for (p in seq_len(page)){
      per_page <- toplot[[p]]
      ts_df <- c()
      for (i in seq_len(length(per_page[[1]]))){
        ts_df_tmp <- dt[(dataset_id == per_page$dataset_id[i] & acause == per_page$acause[i]), ]
        ts_df <- rbind(ts_df, ts_df_tmp)
      }
      
      print(ggplot(data = ts_df) +
            geom_point(aes(x = age, y = counts, color = version, shape = sex)) +
            facet_wrap(dataset_id ~ acause) +
            scale_shape(solid = FALSE)+
            labs(y = "Counts", x = "age_group_id", color = "Prep Version", title = paste0("prep ", version_A," vs.", version_B, ", ", type), subtitle = c, 
                   caption = paste(prep_step_A, version_A, "vs.", prep_step_B, version_B, sep = " "))+
            theme(panel.background = element_blank()))
    }
  }
} 

#Diagnostic plots for different pipeline estimate versions for each dataset within a country
within_dataset_country <- function(data_table){
    message("Aggregate within dataset and country")
    for (dataset in unique(data_table$dataset_id)){
        df <- data_table[dataset_id == dataset, ]
        for (c in unique(df$parent_name)){
            dt <- df[parent_name == c,]
            tmp <- unique(dt[,.(parent_name, acause)])
            page <- ceiling(nrow(tmp)/36)
            temp <- c()
            toplot <- function(df){
            for (p in seq_len(page)){
                if(p*36 < length(df$acause)){
                    acause <- as.list(unique(df[((p-1)*36 +1):(p*36),.(acause)]))
                }else{
                    acause <- as.list(unique(df[((p-1)*36 +1):length(df$acause),.(acause)]))}
                    temp <- append(temp, acause)
        }
        return (temp)
      }
      toplot <- toplot(tmp)
      for (p in seq_len(page)){
        cause_names <- toplot[[p]]
        ts_df <- filter(dt, acause %in% cause_names) %>% as.data.table()
        print(ggplot(data = ts_df) +
                geom_point(aes(x = get(version_A), y = get(version_B), color = age_group_name, shape = sex)) +
                facet_wrap( ~ acause) +
                scale_shape(solid = FALSE)+
                geom_abline(color = "grey") + 
                scale_color_manual(values = colors) +
                labs(x = version_A, y = version_B, color = "age_group_name", title = paste0(" prep ", version_A," vs.", version_B, ", ", type), subtitle = paste0("dataset_id: ", dataset, ",", c), 
                     caption = paste(prep_step_A, version_A, "vs.", prep_step_B, version_B, sep = " "))+
                theme(panel.background = element_blank()))
      }
    } 
  }
}  

#Comparison of the two versions of the age trend of estimates of each cancer and sex across different countries for one dataset
within_dataset <- function(data_table){
    message("Aggregate within dataset")
    for (dataset in unique(data_table$dataset_id)){
        if (!dataset %in% subnational_dataset_ids){
            dt <- data_table[dataset_id == dataset, ]
            print(ggplot(data = dt) +
                geom_point(aes(x = get(version_A), y = get(version_B), color = age_group_name, shape = sex)) +
                facet_wrap( ~ acause) +
                scale_shape(solid = FALSE)+
                geom_abline(color = "grey") + 
                scale_color_manual(values = colors) +
                abs(x = version_A, y = version_B, color = "age_group_name", title = paste0(" prep ", version_A," vs.", version_B, ", ", type), subtitle = paste0("dataset_id: ", dataset), 
                   caption = paste(prep_step_A, version_A, "vs.", prep_step_B, version_B, sep = " "))+
                theme(panel.background = element_blank()))
        }else{
            for (dataset in subnational_dataset_ids){
            df <- data_table[dataset_id == dataset, ]
            for (c in unique(df$parent_name)){
                dt <- df[parent_name == c,]
                tmp <- unique(dt[,.(parent_name, acause)])
                page <- ceiling(nrow(tmp)/36)
                temp <- c()
            toplot <- function(df){
                for (p in seq_len(page)){
                    if(p*36 < length(df$acause)){
                        acause <- as.list(unique(df[((p-1)*36 +1):(p*36),.(acause)]))
                    }else{
                        acause <- as.list(unique(df[((p-1)*36 +1):length(df$acause),.(acause)]))}
                        temp <- append(temp, acause)
            }
            return (temp)
          }
          toplot <- toplot(tmp)
          for (p in seq_len(page)){
            cause_names <- toplot[[p]]
            ts_df <- filter(dt, acause %in% cause_names) %>% as.data.table()
            print(ggplot(data = ts_df) +
                    geom_point(aes(x = get(version_A), y = get(version_B), color = age_group_name, shape = sex)) +
                    facet_wrap( ~ acause) +
                    scale_shape(solid = FALSE)+
                    geom_abline(color = "grey") + 
                    scale_color_manual(values = colors) +
                    labs(x = version_A, y = version_B, color = "age_group_name", title = paste0(" prep ", version_A," vs.", version_B, ", ", type), subtitle = paste("dataset_id: ", dataset, ",", c), 
                         caption = paste(prep_step_A, version_A, "vs.", prep_step_B, version_B, sep = " ")) +
                    theme(panel.background = element_blank()))
          }
          
        }
        
      }
    } 
  }
}

##data preperation and plots generation
for (type in datatype){
  if (type == "inc"){
    if (version_A != ""){
      message("Dataset A: ",paste0(main_path, "appended_", prep_step_A, "_", type, "_", version_A, ".dta"))
      dt_A <- read_dta(paste0(main_path, "appended_", prep_step_A, "_", type, "_", version_A, ".dta")) %>%  select(registry_index, sex_id, age, acause, cases, dataset_id, year_start, year_end) %>% data.table() 
    } else {
      message("Dataset A: ",paste0(main_path, "appended_", prep_step_A, "_", type, ".csv"))
      dt_A <- fread(paste0(main_path, "appended_", prep_step_A, "_", type, ".csv")) %>%  select(registry_index, sex_id, age, acause, cases, dataset_id, year_start, year_end) %>% data.table() 
    }
    dt_A <- merge(dt_A, loc, by = "registry_index", all.x = T)
    if (version_B != ""){
      message("Dataset B: ", paste0(main_path, refresh_folder_B, "appended_", prep_step_B, "_", type, "_", version_B, ".csv"))
      dt_B <- fread(paste0(main_path, refresh_folder_B, "appended_", prep_step_B, "_", type, "_", version_B, ".csv")) %>%  select(registry_index, sex_id, age, acause, cases, dataset_id, year_start, year_end)  
    } else {
      message("Dataset B: ", paste0(main_path, refresh_folder_B, "appended_", prep_step_B, "_", type, ".csv"))
      dt_B <- fread(paste0(main_path, refresh_folder_B, "appended_", prep_step_B, "_", type, ".csv")) %>%  select(registry_index, sex_id, age, acause, cases, dataset_id, year_start, year_end)   
    }
    setnames(dt_A, "cases", "counts")
    setnames(dt_B, "cases", "counts")
  } else{
    if (version_A != ""){
      message("Dataset A: ", paste0(main_path, "appended_", prep_step_A, "_", type, "_", version_A, ".dta"))
      dt_A <- read_dta(paste0(main_path, "appended_", prep_step_A, "_", type, "_", version_A, ".dta")) %>%  select(registry_index, sex_id, age, acause, deaths, dataset_id, year_start, year_end) %>% data.table() 
    } else {
      message("Dataset A: ", paste0(main_path, "appended_", prep_step_A, "_", type, ".csv"))
      dt_A <- fread(paste0(main_path, "appended_", prep_step_A, "_", type, ".csv")) %>%  select(registry_index, sex_id, age, acause, deaths, dataset_id, year_start, year_end) %>% data.table()
    }
    dt_A <- merge(dt_A, loc, by = "registry_index", all.x = T)
    if (version_B != ""){
      message("Dataset B: ", paste0(main_path, refresh_folder_B, "appended_", prep_step_B, "_", type, "_", version_B, ".csv"))
      dt_B <- fread(paste0(main_path, refresh_folder_B, "appended_", prep_step_B, "_", type, "_", version_B, ".csv")) %>%  select(registry_index, sex_id, age, acause, deaths, dataset_id, year_start, year_end)  
    } else {
      message("Dataset B: ", paste0(main_path, refresh_folder_B, "appended_", prep_step_B, "_", type, ".csv"))
      dt_B <- fread(paste0(main_path, refresh_folder_B, "appended_", prep_step_B, "_", type, ".csv")) %>%  select(registry_index, sex_id, age, acause, deaths, dataset_id, year_start, year_end)   
    }
    setnames(dt_A, "deaths", "counts")
    setnames(dt_B, "deaths", "counts")
  }
  
  #subset older version data to only have the dataset_ids from new_version
  dt_B <- dt_B[dataset_id %in% unique(dt_A$dataset_id), ]
  dt_B <- merge(dt_B, loc, by = "registry_index", all.x = T)
  plot_dt <- merge(dt_A, dt_B, by = c("dataset_id", "country_id", "parent_name", "year_start", "year_end", "registry_index", "sex_id", "age", "acause"), all = F)
  plot_dt <- merge(plot_dt, age_groups, by.x = "age", by.y = "cancer_age_group_id", all.x = T)
  plot_dt <- plot_dt[dataset_id %in% plot_datasets, ]
  plot_dt <- plot_dt[acause %in% grep("neo*", acause, value = T),]
  if (type %in% c("across_datasets", "within_dataset_country")){
    plot_dt <- plot_dt[, counts.x := sum(counts.x), by = c("dataset_id","country_id", "parent_name", "sex_id", "age", "acause")]
    plot_dt <- plot_dt[, counts.y := sum(counts.y), by = c("dataset_id", "country_id", "parent_name", "sex_id", "age", "acause")]
    plot_dt <- plot_dt[, change := (counts.x - counts.y)/counts.y, by = c("dataset_id", "country_id", "parent_name", "sex_id", "age", "acause")]
    #subset observations with larger than 20% difference between versions
    plot_dt <- plot_dt[abs(change) > 0.2, ] 
    plot_dt <- plot_dt[, sex := ifelse(sex_id == 1, "Males","Females")]
    plot_dt <- unique(plot_dt[, c("dataset_id", "country_id", "parent_name","sex", "age","age_group_name", "acause", "counts.x", "counts.y")])
    plot_dt <- plot_dt[, age_group_name := factor(age_group_name, levels = age_levels)]
    setnames(plot_dt, c("counts.x", "counts.y"), c(version_A, version_B)) 
    plot_dt <- plot_dt[order(dataset_id),]
  } else {
    plot_dt <- plot_dt[dataset_id %in% subnational_dataset_ids, counts.x := sum(counts.x), by = c("dataset_id","country_id", "parent_name", "sex_id", "age", "acause")]
    plot_dt <- plot_dt[dataset_id %in% subnational_dataset_ids, counts.y := sum(counts.y), by = c("dataset_id", "country_id", "parent_name", "sex_id", "age", "acause")]
    plot_dt <- plot_dt[dataset_id %in% subnational_dataset_ids, change := (counts.x - counts.y)/counts.y, by = c("dataset_id", "country_id", "parent_name", "sex_id", "age", "acause")]
    plot_dt <- plot_dt[!dataset_id %in% subnational_dataset_ids, counts.x := sum(counts.x), by = c("dataset_id","parent_name", "sex_id", "age", "acause")]
    plot_dt <- plot_dt[!dataset_id %in% subnational_dataset_ids, counts.y := sum(counts.y), by = c("dataset_id", "parent_name", "sex_id", "age", "acause")]
    plot_dt <- plot_dt[!dataset_id %in% subnational_dataset_ids, change := (counts.x - counts.y)/counts.y, by = c("dataset_id", "parent_name", "sex_id", "age", "acause")]
    plot_dt <- plot_dt[abs(change) > 0.2, ]
    plot_dt <- plot_dt[, sex := ifelse(sex_id == 1, "Males","Females")]
    plot_dt <- unique(plot_dt[, c("dataset_id", "country_id", "parent_name","sex", "age","age_group_name", "acause", "counts.x", "counts.y")])
    plot_dt <- plot_dt[, age_group_name := factor(age_group_name, levels = age_levels)]
    version_A <- "3.2_p6" #specify x-axis label
    version_B <- "3.1_p6" #speficy y-axis label
    setnames(plot_dt, c("counts.x", "counts.y"), c(version_A, version_B)) 
    plot_dt <- plot_dt[order(dataset_id),]
  }
  
  for (format in formats){
    pdf(paste0(plot_save_path, prep_step_A, "_", version_A, "_vs._", prep_step_B, "_", version_B, "_", type, "_", format, ".pdf"), width = 12) 
    if (format == "across_datasets"){
      across_datasets(plot_dt)
    } else if (format == "within_datasets_country"){
      within_dataset_country(plot_dt)
    } else {
      within_dataset(plot_dt)
    }
  }
  dev.off()
}






































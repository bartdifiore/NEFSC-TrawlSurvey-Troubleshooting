library(gmRi)
library(tidyverse)
library(rfishbase)

# Get data on length_weight_relationships

get_weight <- function(scientific_name, season, sex = NULL, length, make_list = F){
  
  # Set up
  res_box_path<- cs_path(box_group = "Res Data")
  
  lw_key_path <- paste0(res_box_path, "NMFS_trawl/length_weight_keys/wigley_06_lwreg.csv")
  
  wigley <- readr::read_csv(lw_key_path, col_types = readr::cols()) %>% 
    janitor::clean_names() %>% 
    mutate(scientific_name = str_to_sentence(scientific_name), 
           catchsex = as.character(catchsex), 
           season = str_to_lower(season))
  
  
  fb_lw_table <- rfishbase::length_weight()
  
  scientific_names <- rfishbase::species_names(fb_lw_table$SpecCode)
  
  fb_lw_table <- fb_lw_table %>%
    left_join(scientific_names) 
  
  faos <- faoareas() %>% select(AreaCode, StockCode, FAO, Species) %>% 
    filter(AreaCode %in% c(21, 31))
  
  scientific_name = as.vector(str_to_sentence(scientific_name))
  season = as.vector(str_to_lower(season))
  sex = as.vector(as.character(sex))
  length = as.vector(length)
  
  out <- list()
  weights <- vector()
  # parameters <- matrix(ncol = 2, nrow = length(scientific_name))
  # source <- vector()
  
  for(i in 1:length(scientific_name)){
  
  errors <- tryCatch({
    
    error_check = dim(wigley[wigley$scientific_name == scientific_name[i] & wigley$season == season[i] & wigley$catchsex == sex[i], ])[1]
    
  if(scientific_name[i] %in% unique(wigley$scientific_name) == T & error_check > 0){
      wigley_sub = wigley[wigley$scientific_name == scientific_name[i] & wigley$season == season[i] & wigley$catchsex == sex[i], ]
      
      weight_kg = with(wigley_sub, exp(lna + (b*log(length[i]))))
      parameters = c(wigley_sub$lna, wigley_sub$b)
      names(parameters) = c("ln_a", "b")
      source = "wigley"
      out[[i]] <- list(weight_kg = weight_kg, 
                  parameters = parameters, 
                  source = source)
      weights[i] <- weight_kg
  }
    
  if(scientific_name[i] %in% unique(wigley$scientific_name) == T & error_check == 0){
    wigley_sub = wigley[wigley$scientific_name == scientific_name[i] & wigley$season == season[i] & wigley$catchsex == "0", ]
    
    weight_kg = with(wigley_sub, exp(lna + (b*log(length[i]))))
    parameters = c(wigley_sub$lna, wigley_sub$b)
    names(parameters) = c("ln_a", "b")
    source = "wigley"
    out[[i]] <- list(weight_kg = weight_kg, 
                     parameters = parameters, 
                     source = source)
    weights[i] <- weight_kg
  }
  
  if(scientific_name[i] %in% unique(wigley$scientific_name) == F & scientific_name[i] %in% unique(fb_lw_table$Species) == T){
    stocks_of_interest = unique(faos$StockCode[faos$Species == scientific_name[i]])
    fao_area = unique(faos$FAO[faos$Species == scientific_name[i]])
    fishbase_sub = fb_lw_table[fb_lw_table$Species == scientific_name[i], ] %>% filter(StockCode %in% stocks_of_interest)
    a = mean(fishbase_sub$a, na.rm = T)
    b = mean(fishbase_sub$b, na.rm = T)
    weight_g = a*length[i]^b
    parameters = c(a, b)
    names(parameters) = c("a", "b")
    source = paste("fishbase", stocks_of_interest, fao_area)
    out[[i]] <- list(weight_kg = weight_g/1000, 
                parameters = parameters, 
                source = source)
    weights[i] <- weight_g/1000
  }
  
  if(scientific_name[i] %in% unique(wigley$scientific_name) == F & scientific_name[i] %in% unique(fb_lw_table$Species) == F){
    out[[i]] <- "Species not found."
    weights[i] <- "Species not found."
  }
  
  }, error= function(e){
    out[[i]] <- list(weight_kg = cat("ERROR:", conditionMessage(e),"\n"),
                parameters = cat("ERROR:", conditionMessage(e),"\n"),
                source = cat("ERROR:", conditionMessage(e),"\n"))
    weights[i] <- "Sex known, but no sex-specific LW estimates available."
  }

  )

  }
  if(make_list == T){
    return(out)
  }
  if(make_list == F){
    return(weights)
  }
}

# temp_df <- expand.grid(sp = "Gadus morhua", season = c("spring", "fall"), sex = c("0", "1"), length = c(100))
# 
# get_weight(scientific_name = temp_df$sp, season = temp_df$season, sex = temp_df$sex, length = temp_df$length, make_list = F)

# get_weight("Leucoraja garmanidf", "spring", "0", 43)
# get_weight("Gadus morhua", "fall", "0", 43)
# get_weight("Sebastes fasciatus", "fall", "0", length = 20)
# get_weight("Aspidophoroides monopterygius", NA, NULL, length = 30)
# get_weight("Gadus morhua", "spring", "0", length = 100)
# get_weight("Thunnus alalunga", NA, NA, length = 100)
# get_weight("Trachurus lathami", NA, NA, length = 10, make_list = T)
# 
# spec_vec <- c("Thunnus alalunga", "Sebastes Fasciatus", "gadus morhua")
# get_weight(scientific_name = spec_vec[2], season = "fall", sex = NULL, length = 10)
# get_weight("Melanogrammus aeglefinus", NA, NULL, length = 20)
# get_weight("Limanda ferruginea", "spring", "1", length = 20)
# get_weight("Glyptocephalus cynoglossus", "spring", NULL, length = 20)

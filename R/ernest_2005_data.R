#' @title Download raw paper data
#'
#' @name DownloadPaperData
#'
#' @description Download datasets used in Ernest 2005.
#' Only downloads if the relevant folders do not exist in the `datapath`. 
#'
#' @param datapath Folder to store the data
#'
#' @return NULL
#'
#' @export

download_raw_paper_data <- function(datapath = here::here('data', 'paper', 'raw')) {
  
  if(!dir.exists(paste0(datapath, '/andrews-lter'))) {
    dir.create(paste0(datapath, '/andrews-lter'))
  }
  download.file('http://andlter.forestry.oregonstate.edu/ltermeta/ltersearch/dataaccess.aspx?docid=WE02601_v1.csv', (paste0(datapath, '/andrews-lter/andrews.csv')))
  download.file('http://andlter.forestry.oregonstate.edu/mdaccess/metadownload.aspx?dbcode=WE026', (paste0(datapath, '/andrews-lter/andrews-metadata.pdf')))
  download.file('http://andlter.forestry.oregonstate.edu/ltermeta/ltersearch/dataaccess.aspx?docid=SA00501_v2.csv', paste0(datapath, '/andrews-lter/andrews-specieslist.csv'))
  
  
  if(!dir.exists(paste0(datapath, '/niwot'))) {
    
    dir.create(paste0(datapath, '/niwot'))
    
  }
  
  download.file('http://niwot.colorado.edu/data_csvs/smammals.jh.data.csv', (paste0(datapath, '/niwot/niwot-raw.csv')))
  download.file('http://niwot.colorado.edu/meta_data/smammals.jh.meta.txt', (paste0(datapath, '/niwot/niwot-metadata.txt')))
  
  
  
  if(!dir.exists(paste0(datapath, '/sev'))) {
    
    dir.create(paste0(datapath, '/sev'))
    
  }
  
  download.file('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-sev.8.297976&entityid=d70c7027949ca1d8ae053eb10300dc0e', (paste0(datapath, '/sev/sev.csv')))
  download.file('https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sev.8.297976', (paste0(datapath, '/sev/sev-metadata.html')))
  
  
}


#' @title Process raw data in to the appropriate format. 
#'
#' @name ProcessAll
#' @description Process all the datasets and make the key of site names
#'
#' @return NULL
#'
#' @export

process_raw_data <- function() {
  process_andrews_data()
  process_niwot_data()
  process_sev_data()
  process_portal_data()
  make_ernest_name_key()
  return(TRUE)
}


#' @title Process Andrews data in to the appropriate format. 
#' @name ProcessAndrews
#' @description Process Andrews smammal data
#'#'
#' @return NULL
#'
#' @export

process_andrews_data <- function(datapath = here::here()) {
  
  andrews_raw <- read.csv(paste0(datapath, '/data/paper/raw/andrews-lter/andrews.csv'), stringsAsFactors = F)
  
  andrews <- andrews_raw %>%
    as.data.frame() %>%
    `names<-`(tolower(names(.))) %>%
    dplyr::filter(month >= 7, year <= 1999, !is.na(weight))
  
  andrews_species <-  read.csv(paste0(datapath, '/data/paper/raw/andrews-lter/andrews-specieslist.csv'), stringsAsFactors = F)
  
  andrews_rod <- andrews_species %>%
    as.data.frame() %>%
    `names<-`(tolower(names(.))) %>%
    dplyr::select(tax_order, genus, species, subspecies) %>%
    dplyr::mutate(spcode = paste0(substr(genus, 0, 2), substr(species,0,2))) %>%
    dplyr::mutate(spcode = toupper(spcode)) %>%
    dplyr::select(tax_order, spcode) %>%
    dplyr::rename(species = spcode)
  
  
  andrews_means <- andrews %>%
    dplyr::select(species, weight) %>%
    dplyr::group_by(species) %>%
    dplyr::summarize(meanweight = mean(weight), nind = dplyr::n()) %>% 
    dplyr::left_join(andrews_rod, by = 'species') %>%
    dplyr::filter(!is.na(tax_order)) %>%
    dplyr::filter(meanweight <= 400)
  
  andrews <- andrews %>%
    dplyr::filter(species %in% andrews_means$species) %>%
    dplyr::select(species, weight)
  
  processedpath = paste0(datapath, '/data/paper/processed')
  if(!dir.exists(processedpath)) {
    dir.create(processedpath)
  }
  
  write.csv(andrews, paste0(processedpath, '/andrews-processed.csv'), row.names = F)
  
  return(TRUE)
  
}

#' @title Process Niwot data into the appropriate format. 
#' @name ProcessNiwot
#' @param datapath path to where data is 
#' @description Process Niwot smammal data
#' Initial cleaning from LTER website code generator
#'
#' @return NULL
#'
#' @export
process_niwot_data <- function(datapath = here::here()){
  # Halfpenny, Jim. 2019. Small mammal disturbance data for Niwot Ridge from 1981-6-30 to 1990-8-23, yearly. http://niwot.colorado.edu 
  # http://niwot.colorado.edu/data/data/small-mammal-species-composition-data-for-niwot-ridge-1981-1990
  
  niwot_raw <- read.csv(paste0(datapath, '/data/paper/raw/niwot/niwot-raw.csv'), stringsAsFactors = F)
  
  niwot <- niwot_raw %>% 
    dplyr::filter(!is.na(weight)) %>%
    dplyr::mutate(species = as.character(species)) %>%
    dplyr::filter(species != "?MOUSE") %>%
    dplyr::filter(species != "?PIKAS") %>%
    dplyr::filter(species != "CHIPSP") %>%
    dplyr::filter(species != "VOLESP") %>%
    dplyr::select(collector, date, species, weight, condition, habitat)
  
  species_means <- niwot %>%
    dplyr::select(species, weight) %>%
    dplyr::group_by(species) %>%
    dplyr::summarize(mean.weight = mean(weight), n.captures = n()) %>%
    dplyr::ungroup()
  
  big_species <- species_means %>%
    dplyr::filter(mean.weight > 400) %>%
    dplyr::select(species)
  
  niwot <- niwot %>%
    dplyr::filter(!(species %in% big_species$species))
  
  # Still 1 too many species, based on paper.
  # Removing pocket gophers because they were only trapped for the first two years? 
  niwot <- niwot %>%
    dplyr::filter(species != 'THOTAL')
  
  niwot <- niwot %>% 
    dplyr::select(species, weight)
  
  processedpath = paste0(datapath, '/data/paper/processed')
  if(!dir.exists(processedpath)) {
    dir.create(processedpath)
  }
  
  write.csv(niwot, paste0(processedpath, '/niwot-processed.csv'), row.names = F)
  
  return(TRUE)
  
}




#' @title Process Sevilleta data in to the appropriate format. 
#'
#' @name ProcessSev
#'
#' @description Process Sevilleta smammal data
#' Initial cleaning from LTER website code generator
#'
#' @return NULL
#'
#' @export
#'
process_sev_data <- function(datapath = here::here()){
  # FROM LTER WEBSITE #
  # Package ID: knb-lter-sev.8.297976 Cataloging System:https://pasta.lternet.edu.
  # Data set title: Small Mammal Mark-Recapture Population Dynamics at Core Research Sites at the Sevilleta National Wildlife Refuge, New Mexico (1989 - present).
  # Data set creator:  Seth Newsome - University of New Mexico 
  # Metadata Provider:  Information Manager Sevilleta LTER -  
  # Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
  # Contact:  Information Manager Sevilleta LTER -    - data-use@sevilleta.unm.edu
  # Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sev.8.297976
  # Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
  #
  infile1  <- paste0(datapath, '/data/paper/raw/sev/sev.csv') 
  # This creates a tibble named: dt1 
  sev_raw <-readr::read_delim(infile1  
                              ,delim=","   
                              ,skip=1 
                              , col_names=c( 
                                "year",   
                                "location",   
                                "season",   
                                "night",   
                                "web",   
                                "trap",   
                                "recap",   
                                "species",   
                                "sex",   
                                "age",   
                                "reprod",   
                                "mass"   ), 
                              col_types=list(
                                readr::col_character(),  
                                readr::col_character(),  
                                readr::col_character(),  
                                readr::col_character(),  
                                readr::col_character(),  
                                readr::col_character(),  
                                readr::col_character(),  
                                readr::col_character(),  
                                readr::col_character(),  
                                readr::col_character(),  
                                readr::col_character(), 
                                readr::col_number() ), 
                              na=c( "na", ".", " ","NA")  ) 
  # Observed issues when reading the data. An empty list is good!
  readr::problems(sev_raw) 
  
  sev <- sev_raw %>%
    dplyr::filter(year <= 1998, !is.na(mass))
  
  sev_means <- sev %>%
    dplyr::select(species, mass) %>%
    dplyr::group_by(species) %>%
    dplyr::summarize(mean_weight = mean(mass), nind = dplyr::n())
  
  # I'm getting 26 species or 24 if you remove the unidentified ones.
  
  sev_unid <- c('onsp', 'pmsp')
  
  sev_means_unid <- sev %>%
    dplyr::filter(!(species %in% sev_unid)) %>%
    dplyr::select(species, mass) %>%
    dplyr::group_by(species) %>%
    dplyr::summarize(mean_weight = mean(mass), nind = dplyr::n())
  
  sev <- sev %>%
    dplyr::filter(!(species %in% sev_unid)) %>%
    dplyr::select(location, species, mass) %>%
    dplyr::rename(weight = mass)
  
  sev_locations <- unique(sev$location)
  
  processedpath = paste0(datapath, '/data/paper/processed')
  if(!dir.exists(processedpath)) {
    dir.create(processedpath)
  }
  
  for(i in 1:length(sev_locations)) {
    this_location <- sev_locations[i]
    
    this_sev <- sev %>%
      dplyr::filter(location == this_location) %>%
      dplyr::select(-location)
    
    write.csv(this_sev, paste0(processedpath, '/sev-', this_location, '-processed.csv'), row.names = F)
    
  }
  
  return(TRUE)
  
}

#' @title Process Portal data in to the appropriate format. 
#'
#' @name ProcessPortal
#'
#' @description Process Portal smammal data
#' Using `portalr`
#'
#' @return NULL
#'
#' @export
#'

process_portal_data <- function(datapath = here::here(), 
                                portaldatapath = '/Users/renatadiaz/Documents/GitHub/weecology/') {
  
  portal <- portalr::summarise_individual_rodents(path = portaldatapath, download_if_missing = F, clean = T)
  
  portal <- portal %>%
    dplyr::filter(treatment == 'control', year %in% c(1977:1999), !is.na(wgt)) %>%
    dplyr::select(species, wgt)
  
  portal_means <- portal %>%
    dplyr::group_by(species) %>%
    dplyr::summarize(meanwgt = mean(wgt), totaln = dplyr::n())
  
  processedpath = paste0(datapath, '/data/paper/processed')
  if(!dir.exists(processedpath)) {
    dir.create(processedpath)
  }
  
  write.csv(portal, paste0(processedpath, '/portal-processed.csv'), row.names = F)
  
  
  
  return(TRUE)
}

#' Load paper community data
#'
#' @param datapath Main working directory
#'
#' @return list of data frames, one for each community.
#' @export
#'
load_paper_data <- function(){
  data_files <- list.files(path = here::here('data', 'paper', 'processed'), full.names = T)
  
  community_names <- vapply(data_files, FUN = replicatebecs:::get_community_name, FUN.VALUE = "name", USE.NAMES =F)
  
  communities <- list()
  
  for(i in 1:length(data_files)) {
    communities[[i]] <- read.csv(data_files[[i]], stringsAsFactors = F)
    colnames(communities[[i]]) <- c('individual_species_ids', 'individual_sizes')
    
  }
  
  names(communities) <- community_names
  
  return(communities)
}


#' Get community name from data path
#'
#' @param data_file data path
#'
#' @return community name
#'
get_community_name <- function(data_file) {
  
  this_name <-strsplit(data_file, split = 'processed/') %>%
    unlist() %>%
    dplyr::nth(2) %>%
    strsplit(split = '-processed.csv') %>%
    unlist()
  
  return(this_name)
  
}


#' Rerrange table in appendix B
#'
#' @return table of site comparisons, and d and p values, for KS tests.
#' @export
#'
tidy_appendix_b <- function(){
  
  appendix_b_d = read.csv(here::here('ernest-2005-files/ernest_appendixB_maxD.csv'), stringsAsFactors = F)
  
  appendix_b_d = appendix_b_d %>%
    tidyr::gather(key = "site_b", value = "max_d", -site_a, na.rm = T) %>%
    dplyr::mutate(site_b = site_b %>%
                    stringr::str_replace(pattern = "v.", replacement = "v ") %>%
                    stringr::str_replace(pattern = ".2", replacement = " 2") %>%
                    stringr::str_replace(pattern = "s.s", replacement = "s s"))
  
  appendix_b_p = read.csv(here::here('ernest-2005-files/ernest_appendixB_pval.csv'), stringsAsFactors = F)
  appendix_b_p = appendix_b_p %>%
    tidyr::gather(key = "site_b", value = "ernest_p_val", -site_a, na.rm = T)%>%
    dplyr::mutate(site_b = site_b %>%
                    stringr::str_replace(pattern = "v.", replacement = "v ") %>%
                    stringr::str_replace(pattern = ".2", replacement = " 2")%>%
                    stringr::str_replace(pattern = "s.s", replacement = "s s"))
  
  appendix_b = dplyr::left_join(appendix_b_d, appendix_b_p, by = c("site_a", "site_b"))
  
  return(appendix_b)
  
}


#' Compare summary stats from Ernest and current data
#'
#' @return data frame of Ernest summary stats and current summary stats
#' @export
#'
compare_summary_stats = function() {
  communities <- load_paper_data()
  
  bsds <- lapply(communities, FUN = make_bsd) 
  
  
  communities_summary_stats = data.frame(community_name = names(bsds), 
                                         new_richness = NA, 
                                         new_min_mass = NA, 
                                         new_max_mass = NA)
  
  for(i in 1:nrow(communities_summary_stats)) {
    communities_summary_stats$new_richness[i] = nrow(bsds[[i]])
    communities_summary_stats$new_min_mass[i] = min(bsds[[i]]$species_mean_mass)
    communities_summary_stats$new_max_mass[i] = max(bsds[[i]]$species_mean_mass)
  }
  
  communities_summary_stats$new_min_mass[4:9] = min(communities_summary_stats$new_min_mass[4:9])
  communities_summary_stats$new_max_mass[4:9] = max(communities_summary_stats$new_max_mass[4:9])
  
  
  ernest_summary_stats = read.csv(here::here('ernest-2005-files', 'ernest_summary_stats.csv'), stringsAsFactors = F)
  
  ernest_key = read.csv(here::here('ernest-2005-files', 'ernest_key.csv'), stringsAsFactors = F)
  
  joined_summary_stats = dplyr::left_join(ernest_summary_stats, ernest_key, by = 'site') %>%
    dplyr::left_join(communities_summary_stats, by = 'community_name') %>%
    dplyr::rename(ernest_name = site, 
                  new_name = community_name) %>%
    dplyr::select(ernest_name, new_name, ernest_richness, new_richness,
                  ernest_min_mass, new_min_mass,
                  ernest_max_mass, new_max_mass)
  
  return(joined_summary_stats)
}

#' Make key to Ernest names - current community names
#' Ernest (2005) refers to the communities with the `site` name in the text and Figure 1 To compare the communities above to the communities in the resurrected data set, we can try to match them based on the BSD and BSED plots and species richness. 
#' @return
#' @export
#'
make_ernest_name_key <- function() {
  
  ernest_summary_stats = read.csv(here::here("ernest-2005-files/ernest_summary_stats.csv"), stringsAsFactors = F)
  
  # Guesses based on body size plots
  ernest_summary_stats$community_name <- c('andrews', 'niwot', 'portal', 'sev-5pgrass', 
                                           'sev-rsgrass', 'sev-two22', 'sev-goatdraw', 
                                           'sev-5plarrea', 'sev-rslarrea')
  
  ernest_key = ernest_summary_stats %>%
    dplyr::select(site, community_name)
  
  write.csv(ernest_key, file = here::here('ernest-2005-files/ernest_key.csv'), row.names = F)
}
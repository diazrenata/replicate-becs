#' @title Download raw paper data
#' @name DownloadPaperData
#' @description Download datasets used in Ernest 2005.
#' @param from_url default FALSE. If TRUE, download from source URLs. If FALSE, load from included data in repo. 
#' @param storage_path a user-specified directory to store the data
#' @return NULL
#' @export
#' @importFrom utils download.file

download_raw_paper_data <- function(from_url = FALSE, storage_path = here::here("working-data", "paper")){
  
  inst_path = file.path(system.file(package= "replicatebecs"), "data", "paper", "raw")
  
  if(!dir.exists(file.path(storage_path))) {
    dir.create(file.path(storage_path), recursive = T)
  }
  
  if(from_url) {
  
  if(!dir.exists(file.path(storage_path, "andrews-lter"))) {
    dir.create(file.path(storage_path, "andrews-lter"))
  }
  
  download.file("http://andlter.forestry.oregonstate.edu/ltermeta/ltersearch/dataaccess.aspx?docid=WE02601_v1.csv", file.path(storage_path, "raw","andrews-lter", "andrews.csv"))
  download.file("http://andlter.forestry.oregonstate.edu/mdaccess/metadownload.aspx?dbcode=WE026", file.path(storage_path,"andrews-lter","andrews-metadata.pdf"))
  download.file("http://andlter.forestry.oregonstate.edu/ltermeta/ltersearch/dataaccess.aspx?docid=SA00501_v2.csv", file.path(storage_path, "andrews-lter", "andrews-specieslist.csv"))
  
  
  if(!dir.exists(file.path(storage_path, "niwot"))) {
    dir.create(file.path(storage_path, "niwot"))
  }
  
  download.file("http://niwot.colorado.edu/data_csvs/smammals.jh.data.csv", file.path(storage_path, "niwot","niwot-raw.csv"))
  download.file("http://niwot.colorado.edu/meta_data/smammals.jh.meta.txt", file.path(storage_path, "niwot","niwot-metadata.txt"))
  
  
  if(!dir.exists(file.path(storage_path,"sev"))) {
    dir.create(file.path(storage_path, "sev"))
  }
  
  download.file("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-sev.8.297976&entityid=d70c7027949ca1d8ae053eb10300dc0e", file.path(storage_path, "sev","sev.csv"))
  download.file("https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sev.8.297976", file.path(storage_path, "sev", "sev-metadata.html"))
  
  } else {
    file.copy(inst_path, storage_path, recursive = T)
  }
  
}

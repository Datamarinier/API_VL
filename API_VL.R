# Setup -------------------------------------------------------------------
#.rs.restartR()
options(stringsAsFactors = FALSE)

library(httr)
library(jsonlite)
library(stringi)
library(data.table)
library(tidyr)
library(purrr)
library(dplyr)
library(reactable)

#Single call functie
call_api_once <- function(URL,path,query,...){
  
  response <- GET( file.path(URL,path,...), query=query)
  
  message("(1/3) Call gestuurd")
  
  if(identical(as.numeric(status_code(response)),200)){
    
    message("(2/3) Response aangekregen ","Call=  ",response$url)
    
  }else{
    
    stop("foutmelding: ", http_status(response)[[1]]) #nakijken of stop werkt
  }
  
  text <- content(response,"text")
  
  if(!identical(headers(response)$`content-type`,"application/json;charset=UTF-8")){
    
    stop("Geen JSON, functie stopt")
  }
  
  robj <- fromJSON(text)
  
  message("(3/3) JSON object succesvol omgezet naar R-object")
  
  return(robj)
  
}

#Multiple call functie
call_api_multiple_times <- function(iterator, URL, path, query, resultVector){
  
  pb <- txtProgressBar(min = 0, max = length(iterator), style = 3)
  
  lijst <- vector(mode="list",length= length(iterator))
  
  for(i in seq_along(iterator)){
    
    path2 <- iterator[[i]]
    
    robj <- list()
    
    tryCatch(
      
      call_api_once (URL,
                     path,
                     query,
                     path2) %>%
        pluck(!!!resultVector) %>% return,
      
      error=function(e){
        return(toString(e))
      }
    ) ->  lijst[[i]]
    
    setTxtProgressBar(pb, i)
    
  }
  close(pb)
  
  names(lijst) <- iterator
  
  return(lijst)
}

# code --------------------------------------------------------------------

# -> Alle 'feiten' ophalen van 01-01-2019 tot en met 15-11-2019

  # Eerst ophalen van de vergaderingen
  robj <- call_api_once(URL = "http://ws.vlpar.be/e/opendata/",
                        path = "/verg/vorige",
                        query = list(type="plen", dagen=10000,limiet=1000,datumvan="01012019",datumtot="15112019"))
  
  iterator <-  pluck(robj,"items","vergadering","id") #equivalent aan robj[["items"]][["vergadering"]][["id"]]: maar via functie
  
  mainlist <- call_api_multiple_times(iterator=iterator[1:3], #[1:3]-> eerste drie in ljst
                                      URL = "http://ws.vlpar.be/e/opendata/",
                                      path = "verg",
                                      query = list(aanpassingen="nee",idPrsHighlight=0),
                                      resultVector = c("vergadering")) # result vector -> 'plukt' de vergaderingen er uit

# agendalijnen -> dit geeft de agendalijnen 
    mainlist %>%
      tibble(verg = .) %>%
      unnest_wider(verg) %>%
      hoist(`agenda-item`,
            agenda_lijn = "agenda-lijn") %>%
      select( id_verg = id, agenda_lijn,`agenda-item`,datumbegin,datumeinde ) %>%
      unnest_longer(agenda_lijn) %>%
      unnest_wider(agenda_lijn) %>%
      select(id_verg, debat,gedachtewisseling,vrageninterpellatie,datumbegin,datumeinde ) %>%
      pivot_longer(cols = c(debat,gedachtewisseling,vrageninterpellatie) , names_to = "type_activiteit", values_to = "value") %>%
      unnest_longer(value) %>% 
      hoist(value, 
            id = "id", 
            onderwerp = "onderwerp",
            naam = c("objecttype","naam"),
            link = list("link",1,"href") ) %>%
      filter(!is.na(id)) -> collection 

# dit geeft de journaallijnen     
    mainlist %>%
      tibble(verg = .) %>%
      unnest_wider(verg) %>%
      unnest_wider(journaallijn,names_repair="unique") %>% 
      select(id_verg = id...9 #deze kolomnaam kan variëren, soms bv id...8
             ,id_jln= id...13
             ,debat
             ,gedachtewisseling
             ,vrageninterpellatie
             ,`parlementair-initiatief`) %>%
      pivot_longer(cols = c(debat,gedachtewisseling,vrageninterpellatie,`parlementair-initiatief`) , names_to = "type_activiteit", values_to = "value") %>%
      unnest(cols = c(id_jln, value)) %>% 
      unnest_wider(value,names_repair = "unique") %>% 
      unnest() -> result #warning is hier normaal

# filter resultaat op feit debat/VI
  result %>%
    filter(type_activiteit == "vrageninterpellatie") -> frame
  
  #ophalen van alle vrageninterpellaties
  mainlist <- call_api_multiple_times(iterator=frame$id_jln,
                                      URL = "http://ws.vlpar.be/e/opendata/",
                                      path = "jln",
                                      query = list(),
                                      resultVector = c("spreker"))

    tibble(mainlist,names(mainlist)) %>%
      unnest %>% 
      rename(jln_id=`names(mainlist)`) %>% 
      select(sprekertitel,sprekertekst,jln_id) -> tbl
  
  # via tabel
    reactable(
      tbl,
      filterable = TRUE,
      searchable = TRUE)
    
  # via csv
    write.csv2(tbl,"tbl.csv",row.names=FALSE)

  # Specifieke zoektermen -> bv Antwrepen of UNIZO
   tbl %>% 
      filter(stringr::str_detect(sprekertekst, "Antwerpen")) %>% View
    

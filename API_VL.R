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

# API requests uitvoeren en omzetten --------------------------------------------------------------------

# -> Alle 'parlementaire activiteiten' ophalen voor de relevante periode

  # Eerst ophalen van de vergaderingen
  robj <- call_api_once(URL = "http://ws.vlpar.be/e/opendata/",
                        path = "/verg/vorige",
                        query = list(type="plen", dagen=999999,limiet=999999,datumvan="01012015",datumtot="31122015"))
  
  iterator <-  pluck(robj,"items","vergadering","id") #equivalent aan robj[["items"]][["vergadering"]][["id"]]: maar via functie
  
  mainlist <- call_api_multiple_times(iterator=iterator, # [1:234] [1:3]-> eerste drie in list, dit is om eens sub-selectie te kunnen maken, iterator zelf = hele lijst
                                      URL = "http://ws.vlpar.be/e/opendata/",
                                      path = "verg",
                                      query = list(aanpassingen="nee",idPrsHighlight=0), #zie parameters in documentatie Vlaams parl
                                      resultVector = c("vergadering")) # result vector -> 'plukt' de vergaderingen er uit

# agendalijnen -> dit geeft de agendalijnen 
    mainlist %>%
      tibble::tibble(verg = .) %>%
      tidyr::unnest_wider(verg) %>%
      hoist(`agenda-item`,
            agenda_lijn = "agenda-lijn") %>%
      select( id_verg = id, agenda_lijn,`agenda-item`,datumbegin,datumeinde ) %>%
      unnest_longer(agenda_lijn) %>%
      unnest_wider(agenda_lijn) %>%
      select(id_verg, debat,gedachtewisseling,vrageninterpellatie,`parlementair-initiatief`,datumbegin,datumeinde ) %>%
      pivot_longer(cols = c(debat,gedachtewisseling,vrageninterpellatie,`parlementair-initiatief`) , names_to = "type_activiteit", values_to = "value") %>%
      unnest_longer(value) %>% 
            hoist(value,                       #elementen die dieper zitten, naar boven halen
            idAct = "id", 
            onderwerp = "onderwerp",
            naamAct = c("objecttype","naam"),
            link = list("link", 1,"href"),
            nummer = "nummer",
            objectstatus = "objectstatus", 
            titel = "titel",
            zittingsjaar = "zittingsjaar", 
            contacttype = list("contacttype", 1, "beschrijving")) %>%  
      unnest_longer(contacttype) %>% 
      filter(!is.na(idAct)) -> collection 
    
    collection2 <- subset(collection, contacttype == "Vraagsteller" & type_activiteit == "vrageninterpellatie")
    
    collection2 %>% 
       hoist(value, 
             naamMP = list("contacttype", 1, "contact", 1, "naam"),
             voornaamMP = list("contacttype", 1, "contact", 1, "voornaam"),
             fractie = list("contacttype", 1, "contact", 1, "fractie"),
             MP_ID = list("contacttype", 1, "contact", 1, "id")) %>% 
             unnest_wider(fractie, names_repair = "unique") -> collection3
    
    collection3 <- subset(collection3, select=-c(value, kleur, logo))
    
    collection3 %>% 
      rename(
        fractie = naam,
        fractie_id = id) -> collection3    
    
    collection4 <- subset(collection, contacttype == "Ondervraagde minister" & type_activiteit == "vrageninterpellatie")
    
    collection4 %>% 
      hoist(value, 
            naamMP = list("contacttype", 1, "contact", 2, "naam"),
            voornaamMP = list("contacttype", 1, "contact", 2, "voornaam"),
            MP_ID = list("contacttype", 1, "contact", 2, "id")) %>% 
         unnest(cols=c(naamMP, voornaamMP, MP_ID)) -> collection5
    
    collection5 <- subset(collection5, select=-c(value))
    
        collection5 <- collection5 %>%
      add_column(fractie = NA, fractie_id = NA, 'zetel-aantal' = NA)
    names(collection5)
    
    collection6 <- subset(collection, contacttype == "Spreker" & type_activiteit == "vrageninterpellatie")
    
    collection6 %>% 
      hoist(value, 
            naamMP = list("contacttype", 1, "contact", 3, "naam"),
            voornaamMP = list("contacttype", 1, "contact", 3, "voornaam"),
            fractie = list("contacttype", 1, "contact", 3, "fractie"),
            MP_ID = list("contacttype", 1, "contact", 3, "id")) %>%  
      unnest_wider(fractie, names_repair = "unique") %>%
      unnest(cols=c(naamMP, voornaamMP, MP_ID, id, naam, 'zetel-aantal')) -> collection7
    
    collection7 <- subset(collection7, select=-c(value, kleur, logo))
    
    collection7 %>% 
      rename(
        fractie = naam,
        fractie_id = id) -> collection7
    
    actuele_vragen <- rbind(collection3, collection5,collection7)
    
              
    # dit geeft de journaallijnen     
    mainlist %>%
      tibble(verg = .) %>%
      unnest_wider(verg) %>%
      unnest_wider(journaallijn,names_repair="unique") %>% 
      select(id_verg = id...9 #deze kolomnaam kan variëren, soms bv id...8 #dat is hier zo gemaakt omdat er meerdere kolommen zijn met 'id'?
             ,id_jln= id...13
             ,debat
             ,gedachtewisseling
             ,vrageninterpellatie
             ,`parlementair-initiatief`) %>%
      pivot_longer(cols = c(debat,gedachtewisseling,vrageninterpellatie,`parlementair-initiatief`), names_to = "type_activiteit", values_to = "value") %>%
      unnest(cols = c(id_jln, value)) %>% 
      unnest_wider(value,names_repair = "unique") %>% 
      unnest() -> result #warning is hier normaal

# filter resultaat op type parlementaire activiteit (één van de agenda-items)
  collection %>%
    filter(type_activiteit == "parlementair-initiatief") -> frame
  
  #ophalen van alle parlementaire initiatieven
  mainlist <- call_api_multiple_times(iterator=frame$id,
                                      URL = "http://ws.vlpar.be/e/opendata/",
                                      path = "pi",
                                      query = list(),
                                      resultVector = c("parlementair-initiatief")) #hier begrijp ik niet goed wat je zou ingeven want is al specifiek voor parlementair initiatief
                                                            #ik begrijp niet hoe het equivalent is aan de toepassing hierboven voor 'vergadering' -> leeg laten

   #volgende error verscheen indien resultVector = 'spreker' :) 
  #'Error in names(lijst) <- iterator : 
  # 'names' attribute [1882] must be the same length as the vector [976]'

    tibble(mainlist,names(mainlist)) %>%
      unnest %>% 
      rename(id=`names(mainlist)`) %>% 
      select('') -> tbl   #sprekertitel,sprekertekst,jln_id
  
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
    

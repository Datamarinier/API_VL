# Setup -------------------------------------------------------------------
#.rs.restartR()

library(httr)
library(jsonlite)
library(stringi)
library(data.table)
library(tidyr)
library(purrr)
library(dplyr)

options(stringsAsFactors = FALSE)

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

#https://ws.vlpar.be/e/opendata/api/

url <- "http://ws.vlpar.be/e/opendata/"
path <- "/verg/vorige"
query <- list(type="plen", dagen=100,limiet=25,datumvan="01012020",datumtot="21032020")
response <- GET( file.path(url,path),query=query)
text <- content(response,"text")

prettify(text)

robj <- fromJSON(text)

View(robj)

robj[["items"]][["vergadering"]]$id


#in actie
robj <- call_api_once(URL="http://ws.vlpar.be/e/opendata/",
                      path="/verg/vorige",
                      query=list(type="plen", dagen=15,limiet=25,datumvan="01012021",datumtot="15112021"))

#met ... voorbeeld
robj<- call_api_once(URL="http://ws.vlpar.be/e/opendata",
                     path="verg",
                     query=list(aanpassingen="nee",idPrsHighlight=0),
                     1360139
)


# get list van alle vergaderingen in de afgelopen 1000 dagen

#,datumvan="01012021",datumtot="15112021"
robj <- call_api_once(URL = "http://ws.vlpar.be/e/opendata",
                      path = "/verg/vorige",
                      query = list(type="plen", dagen=365,limiet=25))

# get IDs van deze vergaderingen

tibble(verg = robj) %>%
  unnest_wider(verg) %>%
  unnest_wider(vergadering) %>%  
  unnest(cols = c(`agenda-gewijzigd`, `agenda-versie`, commissie, datumagendering, 
                  datumbegin, datumeinde, id, `laatste-wijziging`, link, omschrijving, 
                  `omschrijving-kort`, `opname-start`, plenairehandelingen, 
                  status, type, `uses-beknopt-verslag`, `uses-webhandelingen`, 
                  vergaderingnummer, vergaderzaal, `video-youtube-id`, `voorlopig-verslag`, 
                  voorzitter),names_repair="unique") 

iterator <-  pluck(robj,"items","vergadering","id") 

# iterate over deze vergardering IDs, om alle debatten op te halen

robj <- call_api_once(URL = "http://ws.vlpar.be/e/opendata",
                      path = "/verg",
                      query = list(aanpassingen="nee",idPrsHighlight=0),
                      iterator[1])

list(robj$vergadering) %>%
  tibble(verg = .) %>%
  unnest_wider(verg) %>% View


mainlist <- call_api_multiple_times(iterator=iterator,
                                    URL = "http://ws.vlpar.be/e/opendata/",
                                    path = "verg",
                                    query = list(aanpassingen="nee",idPrsHighlight=0),
                                    resultVector = c("vergadering","journaallijn","debat"))

# final DEMO: getting all types of activitites in one list with ID verg & ID of object itself
#,datumvan="01012019",datumtot="15112019"
robj <- call_api_once(URL = "http://ws.vlpar.be/e/opendata/",
                      path = "/verg/vorige",
                      query = list(type="plen", dagen=10000,limiet=1000,datumvan="01012019",datumtot="15112019"))

iterator <-  pluck(robj,"items","vergadering","id") 

mainlist <- call_api_multiple_times(iterator=iterator[1:3],
                                    URL = "http://ws.vlpar.be/e/opendata/",
                                    path = "verg",
                                    query = list(aanpassingen="nee",idPrsHighlight=0),
                                    resultVector = c("vergadering"))

saveRDS(mainlist,"500_vergarderingen.rds")
df <- readRDS("mainlist.rds")

#agendalijnen
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

mainlist %>%
  tibble(verg = .) %>%
  unnest_wider(verg) %>%
  unnest_wider(journaallijn,names_repair="unique") %>% 
  select(id_verg = id...9
         ,id_jln= id...13
         ,debat
         ,gedachtewisseling
         ,vrageninterpellatie
         ,`parlementair-initiatief`) %>%
  pivot_longer(cols = c(debat,gedachtewisseling,vrageninterpellatie,`parlementair-initiatief`) , names_to = "type_activiteit", values_to = "value") %>%
  unnest(cols = c(id_jln, value)) %>% 
  unnest_wider(value,names_repair = "unique") %>% 
  unnest() -> result

result %>%
  filter(type_activiteit == "debat") -> frame

mainlist <- call_api_multiple_times(iterator=frame$id_jln,
                                    URL = "http://ws.vlpar.be/e/opendata/",
                                    path = "jln",
                                    query = list(),
                                    resultVector = c("spreker"))

saveRDS(mainlist,"143_debat.rds")
mainlist <- readRDS("143_debat.rds")

tibble(mainlist,names(mainlist)) %>%
  unnest %>% 
  rename(jln_id=`names(mainlist)`) %>% 
  select(sprekertitel,sprekertekst,jln_id) -> tbl

reactable(
  tbl,
  filterable = TRUE,
  searchable = TRUE)

write.csv2(tbl,"tbl.csv",row.names=FALSE)

# doorzoekbaar maken

robj <- call_api_once(URL = "http://ws.vlpar.be/e/opendata/",
                      path = "jln",
                      query = list(),
                      "1340974")

View(robj[["spreker"]]$sprekertekst[[1]])

tbl %>% 
  filter(stringr::str_detect(sprekertekst, "Antwerpen")) %>% View


library(reactable)
robj[["spreker"]] %>% 
  select(sprekertitel, sprekertekst) -> tbl

reactable(
  tbl,
  filterable = TRUE,
  searchable = TRUE)

df <- 
  read_chunkwise( tibble(robj[["spreker"]]), 
                  chunk_size=100,
                  format = "table",
                  header = TRUE) %>% 
  filter(stringr::str_detect(sprekertekst, "Milieu"))

chunked::read_lines_chunked(tibble(robj[["spreker"]]), str, chunk_size = 5)


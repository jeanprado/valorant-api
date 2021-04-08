`%>%` <- magrittr::`%>%`

# get val_content ---------------------------------------------------------
get_content <- function(headers=default, element) {
  request_url <- "https://br.api.riotgames.com/val/content/v1/contents"
  
  if(missing(headers)) {
    headers <- c(locale = "pt-BR",
                 `X-Riot-Token` = Sys.getenv("VAL_API"))
  }
  
  rq <- httr::GET(request_url, httr::add_headers(headers))
  
  if(rq$status_code == 200) {
    raw_rq <- rq %>% httr::content() %>% `[[`(element)
    return(raw_rq)
    
  } else { return(usethis::ui_oops(httr::http_status(rq$status_code)$message)) }
}

acts <- get_content(element = "acts")

# get player id -----------------------------------------------------------
get_player_id <- function(usertag) {
  request_url <- "https://americas.api.riotgames.com/riot/account/v1/accounts/by-riot-id/"
  
  user <- stringr::str_split(usertag, "#")[[1]][1]
  tag <- user <- stringr::str_split(usertag, "#")[[1]][2]
  
  rq <- httr::GET(paste0(request_url, user, "/", tag),
                  httr::add_headers(`X-Riot-Token` = Sys.getenv("VAL_API")))
  
  if(rq$status_code == 200) {
    puuid <- rq %>% httr::content() %>% `$`(puuid)
    return(puuid)
    
  } else { return(usethis::ui_oops(httr::http_status(rq$status_code)$message)) }
}

get_player_id("poczinha#6473")

# get current act id ------------------------------------------------------
acts <- if(length(acts)) { acts } else { get_content(element = "acts") }

act_no <- purrr::map_lgl(1:length(acts),
                         ~acts[[.]]$isActive == TRUE) %>% which() %>% `[[`(1)

current_act_id <- acts[[act_no]]$id

# get leaderboard data ----------------------------------------------------
get_ranked <- function(act_id=current_act_id, start=0) {
  request_url <- "https://br.api.riotgames.com/val/ranked/v1/leaderboards/by-act/"
  
  rq <- httr::GET(paste0(request_url, act_id), query = list(size = 200,
                                                            startIndex = start),
                  httr::add_headers(`X-Riot-Token` = Sys.getenv("VAL_API")))
  
  if(rq$status_code == 200) {
    rq_raw <- rq %>% httr::content() %>% jsonlite::fromJSON()
    usethis::ui_done(paste0("Returning #", crayon::bold(start+1), " to #",
                            crayon::bold(start+200), " player from a total of ",
                            crayon::bold(format(rq_raw[["totalPlayers"]],
                                   big.mark = ",", decimal.mark = ".")), " players"))
    
    return(tibble::as_tibble(rq_raw[["players"]]))
    
  } else { return(usethis::ui_oops(httr::http_status(rq$status_code)$message)) }
}

ranked <- get_ranked()

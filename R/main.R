#' add_user_db
#' @export

add_user_db <- function(user, password, signed_in, role, debug = F){
  con <- dbConnect(RSQLite::SQLite(), "data/user.db")
  
  existing_users <- con %>% dplyr::tbl("users") %>% dplyr::pull(user)
  if(debug){print(existing_users)}
  if(user %in% existing_users){
    message("User already exists")
  } else {
    new_user <- tibble(user, password, signed_in, role)
    dbWriteTable(con, "users", new_user, append = T)
    message(glue::glue("User <{ user }> with password <{ password }> was created"))
    
  }
  dbDisconnect(con)
}


#' init_db
#' @export
init_db <- function(user = NA, path = NA){
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  
  #if(length(setdiff(DBI::dbListTables(con), c("com", "dk"))) != 0){
  if(!DBI::dbExistsTable(con,  "com")){
    con %>% DBI::dbWriteTable("com", tibble::tibble(user = user, pageid_1 = NA, pageid_2 = NA, more_left = NA, time = NA, party = NA), overwrite = T)
  }
  # con %>% DBI::dbWriteTable("dk", tibble::tibble(user = user, pageid = NA, name = NA, party = NA), overwrite = T)
  
  # existing_users <- con %>% dplyr::tbl("com") %>% dplyr::pull(user) %>% unique
  # if(!user %in% existing_users){
  #   con %>% DBI::dbWriteTable("com", tibble::tibble(user = user, pageid_1 = NA, pageid_2 = NA, more_left = NA, time = NA, party = NA), append = T)
  #   con %>% DBI::dbWriteTable("dk", tibble::tibble(user = user, pageid = NA, name = NA, party = NA), append = T)
  # }
  
  return(con)
}

#' get_pair_matrix
#' @export

get_pair_matrix <- function(party = "leader"){
  
  pair_mps <- expand.grid(pageid_1 = mp$pageid, pageid_2 = mp$pageid) %>%
    dplyr::filter(pageid_1 != pageid_2) %>%
    dplyr::left_join(dplyr::select(pairwiseR::mp, pageid_1 = pageid, name_1 = name, party_1 = party), by = "pageid_1") %>%
    dplyr::left_join(dplyr::select(pairwiseR::mp, pageid_2 = pageid, name_2 = name, party_2 = party), by = "pageid_2")
  
  if(party == "leader"){
    leaders <- pairwiseR::mp %>% dplyr::filter(most_popular) %>% dplyr::pull(pageid)
    out <- pair_mps %>%
      dplyr::filter(pageid_1 %in% {{leaders}}) %>%
      dplyr::filter(pageid_2 %in% {{leaders}})
  } else {
    out <- pair_mps %>%
      dplyr::filter(party_1 == {{party}}) %>%
      dplyr::filter(party_2 == {{party}})
  }
  
  return(out)
}


#' get_new_pair
#' @export

get_new_pair <- function(user = NA, con = NA, pageid_1 = NULL, pageid_2 = NULL, pair_mp = NULL, party = NULL){
  
  if(is.null(pageid_1) & is.null(pageid_2)){
    tmp_mp <- pair_mp
  } else {
    if(is.null(pageid_2)){
      tmp_mp <- pair_mp %>%
        dplyr::filter(pageid_1 == {{pageid_1}})
    }
    if(is.null(pageid_1)){
      tmp_mp <- pair_mp %>%
        dplyr::filter(pageid_2 == {{pageid_2}})
    }
  }
  
  dk <- con %>%
    dplyr::tbl("dk") %>%
    dplyr::collect() %>%
    dplyr::filter(user == {{user}})
  
  already <- con %>%
    dplyr::tbl("com") %>%
    dplyr::collect(.) %>%
    dplyr::filter(user == {{user}}) %>%
    dplyr::filter(party == {{party}})
  
  message(round(nrow(already)/2, 0), " pairs were compared.")
  
  unique_known <- unique(c(already$pageid_1, already$pageid_2))
  
  message(length(unique_known), " unique MPs for party ", party, " are known")
  
  if(length(unique_known) > 50){
    tmp_mp <- tmp_mp %>% 
      filter(pageid_1 %in% unique_known & pageid_2 %in% unique_known)
  }
  
  new_pair <- tmp_mp %>%
    dplyr::anti_join(already, by = c("pageid_1", "pageid_2")) %>%
    # filter(party %in% {{party}}) %>%
    # filter(id_1 != {{id}}) %>%
    dplyr::filter(!pageid_1 %in% dk$pageid) %>%
    dplyr::filter(!pageid_2 %in% dk$pageid) %>%
    dplyr::sample_n(1)
  
  if(nrow(new_pair) == 0){
    message("All pairs have been coded")
    return(NULL)
  }
  
  return(new_pair)
}



#' add_dont_know
#' @export

add_dont_know <- function(user = NA, pageid = NA, name = NA, party = NA, con = NULL){
  message(name, " won't appear anymore")
  con %>% DBI::dbWriteTable("dk", tibble::tibble(user, pageid, name, party), append = T)
}


#' add_comparison
#' @export

add_comparison <- function(user = NA, pageid_1 = NA, pageid_2 = NA, name_1 = NA, name_2 = NA,
                           more_left = NA, time = NA, party = NA, con = NULL){
  
  if(more_left == 1){
    message(name_1, " is more left than ", name_2)
  } else if (more_left == -1 ) {
    message(name_2, " is more left than ", name_1)
  } else {
    message(name_2, " and ", name_1, " have a similar position")
  }
  
  out_1 <- tibble::tibble(user, pageid_1, pageid_2, more_left, time, party) %>% glimpse
  out_2 <- tibble::tibble(user, pageid_2 = pageid_1, pageid_1 = {{pageid_2}}, more_left = -more_left, time, party) %>% glimpse
  
  
  
  con %>% DBI::dbWriteTable("com", out_1, append = T)
  con %>% DBI::dbWriteTable("com", out_2, append = T)
}

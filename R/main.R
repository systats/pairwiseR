#' init_db
#' @export

init_db <- function(user = NA, path = NA, force = F){
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  
  if(force){
    con %>% DBI::dbWriteTable("com", tibble::tibble(user = user, pageid_1 = NA, pageid_2 = NA, more_left = NA, time = NA, party = NA), overwrite = T)
    con %>% DBI::dbWriteTable("dk", tibble::tibble(user = user, pageid = NA, name = NA, party = NA), overwrite = T)
  }
  
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

get_new_pair <- function(user = NA, con = NA, pageid_1 = NULL, pageid_2 = NULL, pair_mp = NULL){

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
    dplyr::filter(user == {{user}}) %>%
    dplyr::collect(.)

  already <- con %>%
    dplyr::tbl("com") %>%
    dplyr::filter(user == {{user}}) %>%
    dplyr::collect(.)

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

add_dont_know <- function(user = NA, pageid = NA, name = NA, party = NA){
  message(name, " won't appear anymore")
  con %>% DBI::dbWriteTable("dk", tibble::tibble(user, pageid, name, party), append = T)
}


#' add_comparison
#' @export

add_comparison <- function(user = NA, pageid_1 = NA, pageid_2 = NA, name_1 = NA, name_2 = NA,
                           more_left = NA, time = NA, party = NA){

  if(more_left == 1){
    message(name_1, " is more left than ", name_2)
  } else if (more_left == -1 ) {
    message(name_2, " is more left than ", name_1s)
  } else {
    message(name_2, " and ", name_1s, " have a similar position")
  }

  con %>% DBI::dbWriteTable("com", tibble::tibble(user, pageid_1, pageid_2, more_left, time, party), append = T)
  con %>% DBI::dbWriteTable("com", tibble::tibble(user, pageid_2, pageid_1, more_left = -more_left, time, party), append = T)
}

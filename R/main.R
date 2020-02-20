#' init_db
#' @export

init_db <- function(user = NA, path = NA){
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  
  if(length(setdiff(DBI::dbListTables(con), c("com", "dk"))) != 0){
    con %>% DBI::dbWriteTable("com", tibble::tibble(user = user, pageid_1 = NA, pageid_2 = NA, more_left = NA, time = NA, party = NA), overwrite = T)
    con %>% DBI::dbWriteTable("dk", tibble::tibble(user = user, pageid = NA, name = NA, party = NA), overwrite = T)
  }
  
  existing_users <- con %>% dplyr::tbl("com") %>% dplyr::pull(user) %>% unique
  if(!user %in% existing_users){
    con %>% DBI::dbWriteTable("com", tibble::tibble(user = user, pageid_1 = NA, pageid_2 = NA, more_left = NA, time = NA, party = NA), append = T)
    con %>% DBI::dbWriteTable("dk", tibble::tibble(user = user, pageid = NA, name = NA, party = NA), append = T)
  }
  
  return(con)
}

#' get_pair_matrix
#' @export

get_pair_matrix <- function(party = "all"){
  
  pair_mps <- expand.grid(pageid_1 = mp$pageid, pageid_2 = mp$pageid) %>%
    dplyr::filter(pageid_1 != pageid_2) %>%
    dplyr::left_join(dplyr::select(pairwiseR::mp, pageid_1 = pageid, name_1 = name, party_1 = party), by = "pageid_1") %>%
    dplyr::left_join(dplyr::select(pairwiseR::mp, pageid_2 = pageid, name_2 = name, party_2 = party), by = "pageid_2")
  
  if(party == "all"){return(pair_mps)}
  
  # if(party == "leader"){
  #   leaders <- pairwiseR::mp %>% dplyr::filter(most_popular) %>% dplyr::pull(pageid)
  #   out <- pair_mps %>%
  #     dplyr::filter(pageid_1 %in% {{leaders}}) %>%
  #     dplyr::filter(pageid_2 %in% {{leaders}})
  # } else {
  #   
  #   out <- pair_mps %>%
  #     dplyr::filter(party_1 == {{party}}) %>%
  #     dplyr::filter(party_2 == {{party}})
  # }
  
  return(out)
}


#' get_dk
#' @export
get_dk <- function(con, user){
  con %>%
    dplyr::tbl("dk") %>%
    dplyr::collect() %>%
    dplyr::filter(user == {{user}})
}

#' get_already
#' @export
get_already <- function(con, user, party = "all"){
  con %>%
    dplyr::tbl("com") %>%
    dplyr::collect(.) %>%
    dplyr::filter(user == {{user}}) %>%
    dplyr::filter(party == {{party}})
}



#' get_new_pair
#' @export

get_new_pair <- function(user = NA, con = NA, pair_mp = NULL, party = NULL){
  
  dk <- con %>% get_dk({{user}})
  already <- con %>% get_already({{user}}, {{party}})
  message(nrow(already) %/% 2, " pairs were compared.")
  
  tmp_mp <- pair_mp %>%
    filter_at(vars(contains("pageid")), ~!.x %in% dk$pageid)
  message((nrow(tmp_mp) - nrow(already)) %/% 2, " pairs yet to compare")
  
  # if(is.null(pageid_1) & is.null(pageid_2)){
  #   tmp_mp <- pair_mp
  # } else {
  #   if(is.null(pageid_2)){
  #     tmp_mp <- pair_mp %>%
  #       dplyr::filter(pageid_1 == {{pageid_1}})
  #   }
  #   if(is.null(pageid_1)){
  #     tmp_mp <- pair_mp %>%
  #       dplyr::filter(pageid_2 == {{pageid_2}})
  #   }
  # }
  
  to_include <- pairwiseR::mp %>%
    filter(!pageid %in% dk$pageid) %>%
    arrange(desc(trafic)) %>%
    slice(1:(nrow(already) %/% 2 + 2 )) %>%
    pull(pageid)
  
  new_pairs <- tmp_mp %>%
    dplyr::anti_join(already, by = c("pageid_1", "pageid_2")) %>%
    dplyr::filter(pageid_1 %in% to_include) %>%
    dplyr::filter(pageid_2 %in% to_include)
  

  if(nrow(new_pairs) == 0){
    message("All pairs have been coded")
    return(NULL)
  }
  
  return(dplyr::sample_n(new_pairs, 1))
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
                           more_left = NA, time = NA, party = NA, con = NULL, par_anal = 2){
  
  if(more_left == 1){
    message(name_1, " is more left than ", name_2)
  } else if (more_left == -1 ) {
    message(name_2, " is more left than ", name_1)
  } else {
    message(name_2, " and ", name_1, " have a similar position")
  }
  
  solved <- con %>% 
    get_already(user, party) %>%
    get_analytically_solved(quiet = F, par = par_anal) %>%
    mutate(user = user, time = {{time}}, party = {{party}})
  
  out_1 <- tibble::tibble(user, pageid_1, pageid_2, more_left, time, party)
  out_2 <- tibble::tibble(user, pageid_2 = pageid_1, pageid_1 = {{pageid_2}}, more_left = -more_left, time, party)
  
  out <- bind_rows(out_1, out_2, solved) 
  
  con %>% DBI::dbWriteTable("com", out, append = T)
}

#' get_analytically_solved
#' @export

get_analytically_solved <- function(already, quiet = T, par = 3){
  
  inp <- already %>%
    filter(more_left != 0) %>%
    mutate(ideo = case_when(
      more_left == -1 ~ "right", 
      more_left == 1 ~ "left")
    ) %>%
    select(contains("pageid"), ideo) %>%
    group_by(pageid_1, ideo) %>%
    filter(n() > par) %>%
    ungroup %>%
    unique %>%
    group_by(pageid_1, ideo) %>%
    summarise(pageid_2 = list(pageid_2)) %>%
    ungroup %>%
    pivot_wider(id_cols = pageid_1, names_from = ideo, values_from = pageid_2)
  
  if(nrow(inp) <= 1 | !"left" %in% colnames(inp) | !"right" %in% colnames(inp) ){return(tibble())}
  out <- expand_grid(select(inp, pageid_1, left), 
                     select(inp, pageid_2 = pageid_1, right)) %>%
    filter(pageid_1 != pageid_2) %>%
    mutate(n_inter = map2_dbl(left, right, ~length(intersect(.x, .y)))) %>%
    filter(n_inter > par) %>%
    select(contains("pageid")) %>%
    mutate(more_left = -1) 
  
  if(!quiet){message(glue::glue("Could discard {nrow(out)} analytically"))}
  sym_out <- tibble(pageid_1 = out$pageid_2, 
                    pageid_2 = out$pageid_1, 
                    more_left = -out$more_left) 
  
  
  return(bind_rows(out, sym_out))
  
}

#' simule_ranking
#' @export
simule_ranking <- function(pageid){
  ranking <- tibble(pageid) %>%
    sample_n(n()) %>%
    mutate(rank = 1:n())
  
  expand_grid(pageid_1 =pageid, pageid_2 = pageid) %>%
    filter(pageid_1 != pageid_2) %>%
    left_join(ranking, by = c("pageid_1" = "pageid")) %>%
    rename(rank_1 = rank) %>%
    left_join(ranking, by = c("pageid_2" = "pageid")) %>%
    rename(rank_2 = rank) %>%
    mutate(more_left = ifelse(rank_1 > rank_2, 1, -1)) %>%
    select(-contains("rank"))
  
}

#' add_user_db
#' @export

# add_user_db <- function(user, password, signed_in, role, debug = F){
#   con <- dbConnect(RSQLite::SQLite(), "data/user.db")
#   
#   existing_users <- con %>% dplyr::tbl("users") %>% dplyr::pull(user)
#   if(debug){print(existing_users)}
#   if(user %in% existing_users){
#     message("User already exists")
#   } else {
#     new_user <- tibble(user, password, signed_in, role)
#     dbWriteTable(con, "users", new_user, append = T)
#     message(glue::glue("User <{ user }> with password <{ password }> was created"))
#     
#   }
#   dbDisconnect(con)
# }
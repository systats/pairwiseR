Readme
================

Package used for a survey including pairwise comparison (left-right in
our case)

## Packages

## Main

``` r
party <- "SPD"

# This initialises the database, use argument force to overwrite existing database
con <- init_db(user = "ben", path = "data/mp.db", force = T)

# Extracting all potential pairs of MP
pair_mp <- get_pair_matrix(party = party)


# Get a new pair, which has not been coded yet and which does not include "not known" mps
d <- get_new_pair(user = "ben", con = con) %>% glimpse
```

    ## Observations: 1
    ## Variables: 6
    ## $ pageid_1 <int> 7848111
    ## $ pageid_2 <int> 7455374
    ## $ name_1   <chr> "Lars Castellucci"
    ## $ party_1  <chr> "SPD"
    ## $ name_2   <chr> "Nina Scheer"
    ## $ party_2  <chr> "SPD"

``` r
# Add one of the name to the don't know table ("dk")
add_dont_know(user = "ben", pageid = d$pageid_1, name = d$name_1, party = d$party_1)
```

    ## Lars Castellucci won't appear anymore

``` r
# Get a new pair, but only update the first name. 
d <- get_new_pair(user = "ben", con = con, pageid_2 = d$pageid_2) %>% glimpse
```

    ## Observations: 1
    ## Variables: 6
    ## $ pageid_1 <int> 938851
    ## $ pageid_2 <int> 7455374
    ## $ name_1   <chr> "Katja Mast"
    ## $ party_1  <chr> "SPD"
    ## $ name_2   <chr> "Nina Scheer"
    ## $ party_2  <chr> "SPD"

``` r
# Same with the second name
d <- get_new_pair(user = "ben", con = con, pageid_1 = d$pageid_1) %>% glimpse
```

    ## Observations: 1
    ## Variables: 6
    ## $ pageid_1 <int> 938851
    ## $ pageid_2 <int> 666158
    ## $ name_1   <chr> "Katja Mast"
    ## $ party_1  <chr> "SPD"
    ## $ name_2   <chr> "Christine Lambrecht"
    ## $ party_2  <chr> "SPD"

``` r
# Add comparison, more_left should be either : -1 (1 > 2) ; 0 (same position) ; 1 (2 > 1)
add_comparison(user = "ben", pageid_1 = d$pageid_1, pageid_2 = d$pageid_2, 
               name_1 = d$name_1, d$name_2,
               more_left = 1, 
               time = lubridate::now(), party = party)
```

    ## Katja Mast is more left than Christine Lambrecht

``` r
# Get the don't knows
dks <- con %>% tbl("dk") %>% glimpse
```

    ## Observations: ??
    ## Variables: 4
    ## Database: sqlite 3.29.0 [/Users/benjaminguinaudeau/Google Drive/Konstanz/SideProjects/package/pairwiseR/data/mp.db]
    ## $ user   <chr> "ben", "ben"
    ## $ pageid <int> NA, 7848111
    ## $ name   <chr> NA, "Lars Castellucci"
    ## $ party  <chr> NA, "SPD"

``` r
# Get the comparisons
comps <- con %>% tbl("com") %>% glimpse
```

    ## Observations: ??
    ## Variables: 6
    ## Database: sqlite 3.29.0 [/Users/benjaminguinaudeau/Google Drive/Konstanz/SideProjects/package/pairwiseR/data/mp.db]
    ## $ user      <chr> "ben", "ben", "ben"
    ## $ pageid_1  <int> NA, 938851, 938851
    ## $ pageid_2  <int> NA, 666158, 666158
    ## $ more_left <int> NA, 1, -1
    ## $ time      <dbl> NA, 1575315297, 1575315297
    ## $ party     <chr> NA, "SPD", "SPD"

## MP Database

``` r
data <- legislatoR::get_core("deu") %>%
  left_join(legislatoR::get_political("deu")) %>%
  left_join(legislatoR::get_office("deu")) %>%
  left_join(legislatoR::get_traffic("deu"))

mp <- data %>%
  filter(session == 18) %>%
  select(pageid, name, party, date, traffic) %>%
  group_by(pageid, name, party) %>%
  summarise(trafic = median(traffic)) %>%
  ungroup %>%
  mutate(party = case_when(
    party %in% c("CDU", "CSU") ~ "CDU/CSU", 
    party == "BÜNDNIS 90/DIE GRÜNEN" ~ "GRUENE", 
    party ==  "DIE LINKE" ~ "PDS/LINKE", 
    party == "none" ~ NA_character_, 
    T ~ party
  )) %>%
  group_by(party) %>%
  arrange(-trafic) %>%
  slice(1:100) %>%
  mutate(most_popular = ifelse(1:n() %in% 1:10, T, F)) %>%
  ungroup %>%
  glimpse

usethis::use_data(mp)
```

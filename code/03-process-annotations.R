library(tidyverse)
library(jsonlite)
library(lubridate)
library(knitr)


# Import and clean labeled data -------------------------------------------

sentiment_lvls <- c("Very Negative", "Negative",  "Slightly Negative",  "Neutral",  "Slightly Positive",  "Positive",  "Very Positive")

d <- "data/doccano" |> 
  list.dirs(recursive = F) %>%
  pluck(which.max(ymd_hm(str_extract(., "\\d[\\d-_]+$")))) |> # get the most recent data
  list.files(full.names = T) %>%
  set_names(., str_extract(., "\\w+(?=\\.jsonl$)")) |> # set names by annotator
  map(\(doc) doc |> # iterate over files
    read_lines() |> 
    map_dfr(\(line) line |> # iterate over lines
          fromJSON() |>
          modify_in("label", \(x) if (is.list(x)) data.frame() else as.data.frame(x)) |> # Items without labeled person will be omittted
          modify_in("Comments", \(x) x |> as.character() |> list()) |>
          as_tibble() 
        ) |> 
    unnest(label)
  ) |> 
  bind_rows(.id = "annotator") |> 
  mutate(
    mention = str_sub(text, V1, V2) |> 
           str_remove_all(r"(Herrn?|Frau|[(")'„"/#.,:]|^\w |^-|Kanzler)") |> # Clean annotated names
           str_squish(),
    sentiment7 = factor(V3, levels = sentiment_lvls)
    ) |> 
  select(-V1:-V3) |> 
  left_join(read_csv("data/doccano/person-disambiguation.csv", col_select = 1:2, col_types = "cc")) |>  # Add disambiguated names 
  filter(!person %in% c("mislabeled", "not relevant")) |> 
  arrange(id)

d <- d |> 
  mutate(sentiment3 = case_when (
    as.integer(sentiment7) %in% 1:3 ~ "negative",
    as.integer(sentiment7)  ==  4   ~ "neutral",
    as.integer(sentiment7) %in% 5:7 ~ "positive",
  ))

# mentions without match in disambiguation file. Should be zero length, else add
d |> 
  filter(is.na(person))

# Distribution of any annotation per statement. 
d |> 
  distinct(annotator, id, text) |> 
  count(id, text) |>
  count(n) |> 
  set_names("Number of Annotators", "Number of Items") |> 
  kable("pipe") |>
  c(": Distribution of the count of annotated items by the number of annotators {#tbl-annotator-count}") |> 
  write_lines("tab/annotator_count.md")

# 63 individual politician mentions were labeled. Only 9 were labeled more than 
# 10 times. 
d |> 
  count(person, sort = T)

# Both because of frequency of occurrence and them being chancellorship
# aspirants, restrict to Baerbock, Laschet, Scholz, Habeck and Söder:
chancellorship_competitors <- c("Annalena Baerbock", "Armin Laschet", "Olaf Scholz", "Robert Habeck", "Markus Söder")

d <- d |> 
  filter(person %in% chancellorship_competitors)

# Annotators were instructed to comment when sentences seem unusual. Of 1889 
# sentences, 96 received such comments. Some of these indicate meta information
# like title, reporting location or author leading or trailing the sentence. 
# Others indicator errors by the tokenizer, where sentences were incorrectly split.

d |> 
  filter(!map_lgl(Comments, is_empty)) |> 
  pull(Comments) |> 
  unique()

d |> 
  filter(map_lgl(Comments, \(x) "Missing period" %in% x))

# Consolidation -------------------------------------------------------------------------
# This commented out piece of code consolidates cases where the same annotator 
# gave different labels to separate mentions of the same person within the same item. 
# Because of the small number of these cases they were just discarded. 
# d |> 
#   group_by(annotator, id, person) |> 
#   count() |>
#   filter(n >1) |> 
#   arrange(id)
#   
# d1 <- d |> 
#   group_by(annotator, id, person) |> 
#   mutate(grp = sentiment3 |> unique() |> length() |> (`!=`)(1)) %>%
#   split(.$grp) |> 
#   modify_in(2, \(x) x |> 
#               mutate(sentiment3cons = case_when(
#                 sum(c("positive", "negative") %in% sentiment3) > 1 ~ NA,
#                 "positive" %in% sentiment3 ~ "positive",
#                 "negative" %in% sentiment3 ~ "negative",
#                 identical(unique(sentiment3), "neutral") ~ "neutral"
#               )) |>
#               ungroup() 
#               )

# -------------------------------------------------------------------------

# Remove any annotation where the same person was mentioned more the once and 
# the annotator assigned different labels to the different mentions
d2 <- d |> 
  group_by(annotator, id, person) |> 
  filter(sentiment3 |> unique() |> length() |> (`==`)(1)) |> 
  ungroup()

# Number of item*politican combinations rated by x annotators. There are
# just 13 instances where one of the 5 selected politicians was labeled only 
# once, indicating that an annotator missed an instance there. 
d2 |> 
  group_by(id, person) |>
  summarize(annotations = annotator |> unique() |> length()) |> 
  ungroup() |> 
  count(annotations) |> 
  set_names("Number of Annotators", "Number of Item-Person Combination") |> 
  kable("pipe") |>
  c(": Distribution of item-politician combinations by number of annotators {#tbl-item-person-annotators}") |> 
  write_lines("tab/item-person-annotators.md")


# Inspect cases with just one annotator
d2 |> 
  group_by(id, person) |>
  summarize(text = unique(text), 
            annotations = annotator |> unique() |> length(),
            annotator = list(c(annotator))) |> 
  filter(annotations == 1) |> 
  unnest(annotator) 

consolidated <- d2 |> 
  filter(mention != "Lasche") |> # Filter out typo with alternative meaning
  group_by(id, person) |>
  filter(annotator |> unique() |> length() > 1) |> # Discard cases annotated by only one annotator 
  group_by(annotator, id, text, person) |> 
  summarize( # Aggregate sentiment to statement*annotator*person
    mention = list(mention), 
    sentiment3 = unique(sentiment3)
    ) |> 
  group_by(id, text, person) |> 
  summarize(
    mention = list(mention), 
    sentiment3 = sentiment3 |> 
      table() |>
      (\(x) x / sum(x))() |> 
      sort(T) |> 
      (\(x) ifelse(x[1] > .5, names(x)[1], "disagreement"))()
    ) |> 
  ungroup() 

# Consolidation discards 469 statement*politician combinations, leaving 1711
consolidated |> 
  count(sentiment3) |> 
  mutate(p = n/sum(n) * 100) |> 
  set_names("Label", "Number of cases", "Share (%)") |> 
  kable("pipe", 2) |>
  c(": Distribution of consolidated labels (single-annotator cases discarded before) {#tbl-consolidated-cases}") |> 
  write_lines("tab/consolidated-cases.md")
  

# Create regex to match candidates
candidate_regex <- 
  read_csv("data/doccano/person-disambiguation.csv", col_types = "ccl") |> 
  filter(
    is.na(name_too_short), 
    person %in% chancellorship_competitors,
    str_detect(mention, "[^s]$")
    ) |> 
  arrange(person, -str_length(mention)) |> 
  group_by(person) |> 
  summarize(regex = ifelse(person == "Olaf Scholz", mention, str_c(mention, "s?")) |>  
              str_c(collapse = "|") %>%
              str_c("(", ., ")")) |> 
  deframe()


consolidated <- consolidated |> 
  filter(sentiment3 != "disagreement") |> # remove cases of disagreement
  mutate(multimention = mention |> map_int(\(x) max(map_int(x, length))) > 1) |>
  group_by(person) |> 
  mutate(text_atepc = str_replace_all(text, unname(candidate_regex[first(person)]), "[B-ASP]\\1[E-ASP]") |> 
           str_c(" $LABEL$ ", sentiment3),
         text_apc = str_replace_all(text, unname(candidate_regex[first(person)]), "$T$") |> 
           str_c("\n", person, "\n", sentiment3)) |> 
  ungroup() 

consolidated |> 
  write_rds("data/custom_data_full.rds")

# Do 80-10-10 split for train/valid/test. Running this code will overwrite the existing sets.
# split_sets <- consolidated |> 
#   group_by(person) |> 
#   mutate(set = c(rep("train", round(n()*.1)), # Stratified sampling
#                  rep("test", n() - 2*round(n()*.1)), 
#                  rep("valid", round(n()*.1))
#                  ) |> sample()
#          ) |> 
#   ungroup() %>%
#   split(.$set)
# 
# write_rds(split_sets, "data/split_sets.rds")
# 
# split_sets |> 
#   iwalk(\(dat, set) dat |> 
#           pull(text_apc) |> 
#           write_lines(str_c("data/apc_datasets/101.news_german/news_german.", set, ".dat.apc")))


# Export unlabeled sentences for prediction -------------------------------
candidate_regex_collapsed <- candidate_regex |> 
  unname() |> 
  str_remove_all("^\\(|\\)$") |> 
  str_c(collapse = "|") %>%
  str_c("(", ., ")")

"data/unannotated_mentions.rds" |> 
  read_rds() |> 
  mutate(text_atepc = str_replace_all(sent, candidate_regex_collapsed, "[B-ASP]\\1[E-ASP]")) |> 
  pull(text_atepc) |> head()
  write_lines("data/unannotated_mentions.txt")

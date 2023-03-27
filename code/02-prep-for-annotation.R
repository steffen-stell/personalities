library(tidyverse)



short <- tibble(publication = c("BILD Bund", "Der Tagesspiegel", "Die Welt", 
                       "Frankfurter Rundschau", "taz, die tageszeitung"), 
  pub = c("bild", "ts", "welt", "fr", "taz"))

tx <- "data/lexis/lexis_frame.rds" |> 
  read_rds() |> 
  left_join(short) |>
  replace_na(list(page_section_prefix = "", subsection = "")) |> 
  mutate(
    sec = case_when(
      subsection != "" ~ str_c(section, "-", subsection),
      page_section_prefix != "" ~ str_c(section, "-", page_section_prefix),
      T ~ section
      )
    ) |> 
  select(pub, sec, text, date, id) |> 
  mutate(sent = tokenizers::tokenize_sentences(text),
         para = tokenizers::tokenize_paragraphs(text, paragraph_break = "\n"))

# Export sections for manual labeling for inclusion
# tx |> 
#   count(pub, sec) |> 
#   arrange(pub, -n) |> 
#   readODS::write_ods("data/sections.ods")

# Subset by section
tx <- readODS::read_ods("data/sections.ods") |> 
  as_tibble() |> 
  inner_join(tx, multiple = "all") |> 
  filter(include == 1) |>
  select(-include)

# Distribution of articles by week and publication
tx |> 
  ggplot() +
  geom_histogram(aes(week(date), fill = pub))

tx |> 
  select(-text) |> 
  unnest(sent = sent) |> 
  pull(sent) |>
  str_count("Baerbock") |> 
  sum()
  
# tx |> 
#   filter(str_detect(text, r"(\(?http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+)")) |> View()

# tx$text |> 
#   str_detect(r"(https?://[^)\]\s]+(?=[)\]\s]))") |>  
#   na.omit()

# All mentions for 
all_mentions <- tx |>
  select(-para) |> 
  unnest(sent) |> 
  filter(str_detect(sent, "(Baerbock|Scholz|Laschet|Habeck|Söder|Lindner)")) |> 
  pull(sent) |> 
  sample()

# export sample for annotation in Doccano
# all_mentions[seq_len(round(.2*length(all_mentions)))] |> 
#   write_lines("data/final-sample-for-classification.txt")


# Reexport mentions with meta data ----------------------------------------

am <- all_mentions |> 
  str_subset(sent, "(Baerbock|Scholz|Laschet|Habeck|Söder)") |> 
  tibble(sent = _)

annotation_sents <- read_lines("data/final-sample-for-classification.txt") |> 
  tibble(sent = _) |> 
  mutate(doccano_id = row_number()) |> 
  inner_join(unnest(tx, sent)) |> 
  select(-para)

annotation_sents |> 
  write_rds("data/annotation_mentions.rds")

unannotated <- annotation_sents |> 
  anti_join(am, y = _) |> 
  inner_join(unnest(tx, sent)) |> 
  select(-para) |> 
  filter(str_detect(sent, "(Baerbock|Scholz|Laschet|Habeck|Söder)"))

unannotated |> 
  write_rds("data/unannotated_mentions.rds")

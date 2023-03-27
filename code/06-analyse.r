library(tidyverse)
library(reticulate)
library(lubridate)



# Classification ----------------------------------------------------------
# Set up 
use_condaenv("thesis")
reticulate::py_run_string('import os; os.environ["TOKENIZERS_PARALLELISM"] = "false"')

pa <- import("pyabsa")


# Caution! The predict function of the classifier silently deduplicates the 
# input, thus returning shorter vectors than expected. In addition, items that 
# are too long can have more than one unnested list element returned, leading to longer 
# than expected return vectors. The text element in the returned list cannot be used for 
# joining, as the bos/eos tokens are replaced with spaces. The batch_predict 
# method exhibits the same issue. Furthermore, batch_predict writes an invalid 
# json file. 

candidate_regex_collapsed <- "(Annalena Charlotte Alma Baerbocks?|Annalena Baerbocks?|Annelana Baerbocks?|Annelena Baerbocks?|Annalenas?|Baerbocks?|Barbocks?|Armin Laschets?|Arnim Laschets?|Laschets?|Markus Söders?|Söders?|Scholzomaten|OIaf Scholz|Olaf Scholz|Scholzomat|Scholz|Robert Habecks?|Habecks?)"

classifier <- pa$AspectPolarityClassification$SentimentClassifier(
  checkpoint = "mv_checkpoints/fast_lsa_t_v2_custom_dataset_acc_57.4_f1_57.81"
  )

unclassified <- "data/unannotated_mentions.rds" |> 
  read_rds() |> 
  rename(full_text = text) |> 
  group_by(pub, date, sent) |> 
  slice(1) |> # When a sentence occurs twice on the same day in the same publication, it is very likely there are two versions of the same article. Reduce to one.
  ungroup() |> 
  mutate(text_atepc = str_replace_all(sent, candidate_regex_collapsed, "[B-ASP]\\1[E-ASP]")) 


# classifier$predict() takes a lot of time to run
classified <- 
  unclassified |> 
  distinct(text_atepc) |> 
  filter(str_count(text_atepc) < 600) |> # discard items that are too long
  mutate(cls = classifier$predict(text_atepc, print_result = F, ignore_error = T)) 

baerbock_names <- "(Annalena Charlotte Alma Baerbocks?|Annalena Baerbocks?|Annelana Baerbocks?|Annelena Baerbocks?|Annalenas?|Baerbocks?|Barbocks?)"

# Unnest classification returns
ab_classified <- classified |> 
  mutate(cls = cls |> 
           map(\(el) el |> 
                 keep_at(1:5) |> 
                 map_if(1:5 != 1, list) |> 
                 as_tibble()
               )
         ) |> 
  unnest(cls) |> 
  inner_join(unclassified, multiple = "all") |> 
  unnest_longer(aspect:probs) |> 
  filter(str_detect(aspect, baerbock_names)) |> 
  select(-text) |> 
  rename(sent_id = id) |> 
  mutate(pred_weighted = as.vector(do.call(rbind, probs) %*% (-1:1))) # Weight numeric sentiment by probability


# Prep annotated cases for binding with classified data
ab_annotated <- read_rds("data/custom_data_full.rds") |> 
  filter(person == "Annalena Baerbock") |> 
  inner_join(read_rds("data/annotation_mentions.rds") |> 
               group_by(pub, date, sent) |> 
               slice(1), 
             by = c("id" = "doccano_id"),
             multiple = "all"
             ) |> 
  select(-text.x,
         -multimention,
         -person, 
         -mention) |> 
  rename(sent_id = id.y, 
         doccano_id = id,
         full_text = text.y,
         text_atepc = text2,
         sentiment = sentiment3
         )

# Combined set of classifed and annotated data
combined <- ab_classified |> 
  bind_rows(ab_annotated) |> 
  mutate(sentiment_num = sentiment |> 
           factor(c("negative", "neutral", "positive")) |> 
           as.integer() - 2,
         week = week(date),
         pred_weighted = ifelse(is.na(pred_weighted), sentiment_num, pred_weighted))

# Select events impacting Baerbocks standing
important_events <- tribble(
  ~date, ~event,
  "2021-04-19", "candidate-designate", 
  "2021-05-20", "late-reported side earnings",
  "2021-06-06", "CV misrepresentations",
  "2021-06-28", "plagiarism allegations",
  "2021-07-16", "Ahrtal floods",
  "2021-09-26", "election"
) |> 
  mutate(week = week(ymd(date)))

write_rds(combined, "data/predicted_popularity.rds")

# Plot --------------------------------------------------------------------
theme_set(theme_bw())
mysave <- \(plot, fig_name, ratio = 9/16, ...){
  ggsave(filename = str_c("fig/", fig_name, ".pdf"), 
         plot = plot,
         width = 152, 
         height = ratio * 152,
         units = "mm",
         ...)
  cat(str_c('![]("fig/', fig_name,'.pdf"){#fig-', fig_name, '})'))
}

pb_agg <- read_rds("data/pb_agg.rds")

combined |> 
  group_by(pub, week) |> 
  summarize(n = n(), pred = mean(pred_weighted)) |> 
  arrange(pub, week) |>
  filter(n >= 10,
         pub != "bild") |> 
  ggplot(aes(week, pred, color = pub)) +
  geom_line()

# Distribution of confidence
ab_classified |> 
  ggplot(aes(confidence)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = .05) 

# Distribution of weighted sentiment
ab_classified |> 
  ggplot(aes(pred_weighted)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = .2) 


{combined |> 
  group_by(week) |> 
  summarize(n = n(), pred = mean(pred_weighted)) |> 
  ggplot(aes(week, n)) +
  geom_bar(stat = "identity") +
  labs(x = "Week", 
       y = "Number of Mentions of Baerbock"
       )} %>% 
  mysave("mentions-over-time")


# Plot with Politbarometer
combined |> 
  group_by(week) |> 
  summarize(n = n(), pred = mean(pred_weighted)) |>
  left_join(pb_agg) %>% 
  {
    ggplot(., aes(week)) +
    geom_hline(yintercept = 0, alpha = .3) +
    geom_vline(data = important_events, aes(xintercept = week), linetype = 3) +
    geom_line(aes(y = pred)) +
    geom_point(aes(y = pred, size = n)) +
    geom_point(aes(y = skalo_baer), shape = 15) +
    geom_line(data = .[!is.na(.$skalo_baer), ], aes(y = skalo_baer), na.rm = T, linetype = 2) +
    geom_text(data = important_events, aes(x = week, label = event), y = -1.05, angle = 90, hjust = "bottom", vjust = -.15, size = 3) +
    theme_bw() +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(
      x = "Week",
      y = "Aggregated Sentiment (solid) / Politbarometer Popularity (dashed)",
      size = "No. of Mentions"
    )
    } |> 
  ggsave(filename = "fig/aggregated-sentiment-politbarometer.pdf", width = 260, height = 130, units = "mm")




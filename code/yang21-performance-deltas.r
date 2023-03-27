"BERT-BASE, RoBERTa-BASE, DeBERTa-BASE
80.36(0.78), 82.76(0.63), 82.76(0.31)
77.04(0.71), 79.73(0.77), 79.45(0.60)
86.34(0.18), 87.77(1.61), 88.66(0.35)
80.01(0.28), 82.10(2.01), 83.06(0.29)
82.52(1.13), 83.83(0.49), 83.06(1.24)
81.87(1.23), 83.29(0.50), 82.52(1.25)
" |> 
  read_table() |> 
  mutate_all(str_extract, "^[\\d.]+") |> 
  mutate_all(as.double) |> 
  mutate(metric = c("Acc", "F1", "Acc", "F1", "Acc", "F1"),
         data = rep(c("Laptop14", "Rest14", "MAMS"), each = 2)
         ) |> 
  select(-2) |> 
  rename(bert = 1, deberta = 2) |> 
  mutate(delta = deberta - bert) |> 
  group_by(metric) |> 
  summarize(mean(delta))





"LSAT_BERT  BERT_BASE
81.35(0.39), 80.36(0.78)
78.43(0.52), 77.04(0.71)
87.32(0.22), 86.34(0.18)
81.86(0.20), 80.01(0.28)
83.51(0.26), 82.52(1.13)
82.90(0.28), 81.87(1.23)" |> 
  read_table() |> 
  mutate_all(str_extract, "^[\\d.]+") |> 
  mutate_all(as.double) |> 
  mutate(metric = c("Acc", "F1", "Acc", "F1", "Acc", "F1"),
         data = rep(c("Laptop14", "Rest14", "MAMS"), each = 2)
  ) |> 
  mutate(delta = LSAT_BERT - BERT_BASE) |> 
  group_by(metric) |> 
  summarize(mean(delta))














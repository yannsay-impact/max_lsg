# max_lsg
alternative to msni2022. The function *make_lsg* aims to do the final/max aggregation between the critical indicators and the final non-critical indicator.

```
source(max_lsg.R)
make_lsg(dummy_dataset,
         crit_to_4plus = c("crit1_4plus", "crit2_4plus"), #vector with the names all of indicators that have the critical indicators that score from 1 to 5 (5 is 4+)
         crit_to_4 = c("crit1_4", "crit2_4"), #vector with the names all of indicators that have the critical indicators that score from 1 to 4
         crit_to_3 = c("crit1_3", "crit2_3"), #vector with the names all of indicators that have the critical indicators that score from 1 to 3
         non_crit = "crit1_nc") #name of the column with the non critical composite that scores from 1 to 3
         
dummy_dataset$lsg_score <- make_lsg(dummy_dataset,
                                    crit_to_4plus = c("crit1_4plus", "crit2_4plus"),
                                    crit_to_4 = c("crit1_4", "crit2_4"), 
                                    crit_to_3 = c("crit1_3", "crit2_3"), 
                                    non_crit = "crit1_nc") 

dummy_dataset |> View()

#without 4+
dummy_no4plus <- dummy_dataset[, c("crit1_4", "crit2_4", "crit1_3", "crit2_3", "crit1_nc")] |>
  unique()
dummy_no4plus$lsg_score <- make_lsg(dummy_no4plus,
                                    crit_to_4 = c("crit1_4", "crit2_4"), 
                                    crit_to_3 = c("crit1_3", "crit2_3"), 
                                    non_crit = "crit1_nc") 

dummy_no4plus |> View()
```

source("code/00_dependencies.R")

# Load data

frs_tbl <- read_xlsx(
  path = "data/2024-03-11_frs-smallcap-perf-data.xlsx",
  skip = 10,
  col_types = c("text", "numeric", "skip", "numeric", "skip"),
  col_names = c("date", "acct_ret", "skip_1", "mdl_ret", "skip_2")
) |> 
  separate(
    col = date,
    into = c("start_date", "end_date"),
    sep = " to "
  ) |> 
  mutate(
    start_date = dmy(start_date),
    end_date = dmy(end_date)
  ) |> 
  filter(!is.na(start_date))

caps_tbl <- read_xlsx(
  path = "data/2024-03-11_caps-smallcap-perf-data.xlsx",
  col_types = c("text", "date", "numeric", "numeric", "numeric", "numeric")
) |> 
  select(
    date, GR
  ) |> 
  rename(
    acct_ret = GR
  ) |> 
  mutate(
    date = as.Date(date)
  )

# examine frs tbl

frs_tbl <- frs_tbl |> 
  mutate(
    start_day = wday(start_date, label = TRUE),
    end_day = wday(end_date, label = TRUE)
  ) |> 
  filter(
    !(
      # start_day %in% c("Sat", "Sun") |
      end_day %in% c("Sat", "Sun")
    )
  ) |> 
  select(end_date, acct_ret, mdl_ret) |> 
  rename(date = end_date)



# compare frs_tbl and caps_tbl

compare_tbl <- caps_tbl |> 
  left_join(
    # frs_tbl |> select(-bm_ret),
    frs_tbl,
    by = join_by(date),
    suffix = c("_caps", "_frs")
  )

# a couple of quick checks
compare_tbl |> 
  summarize(
    across(
      .cols = where(is.numeric),
      \(x) (prod(1+x/100, na.rm = TRUE) - 1) * 10000
    )
  )

# analyze the daily differences


comp_rep_tbl <- compare_tbl |> 
  mutate(
    diff = acct_ret_caps - acct_ret_frs
  ) |> 
  filter(!is.na(diff))

comp_rep_tbl |> 
  summarize(
    pct_lt_10bps = mean(abs(diff)<0.1),
    pct_10_20bps = mean(abs(diff)>=0.1 & abs(diff)<0.2),
    pct_gt_20bps = mean(abs(diff)>=0.2)
  )


comp_rep_plot <- comp_rep_tbl |> 
  ggplot(
    aes(x = date, y = diff)
  ) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-0.3, 0.3) +
  theme_minimal()
  



comp_mdl_tbl <- compare_tbl |> 
  mutate(
    diff = acct_ret_caps - mdl_ret
  ) |> 
  filter(!is.na(diff))

comp_mdl_tbl |> 
  summarize(
    pct_lt_10bps = mean(abs(diff)<0.1),
    pct_10_20bps = mean(abs(diff)>=0.1 & abs(diff)<0.2),
    pct_gt_20bps = mean(abs(diff)>=0.2)
  )

comp_mdl_plot <- comp_mdl_tbl |> 
  ggplot(
    aes(x = date, y = diff)
  ) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-0.3, 0.3) +
  theme_minimal()


rep_mdl_tbl <- compare_tbl |> 
  mutate(
    diff = acct_ret_frs - mdl_ret
  ) |> 
  filter(!is.na(diff))

rep_mdl_tbl |> 
  summarize(
    pct_lt_10bps = mean(abs(diff)<0.1),
    pct_10_20bps = mean(abs(diff)>=0.1 & abs(diff)<0.2),
    pct_gt_20bps = mean(abs(diff)>=0.2)
  )

rep_mdl_plot <- rep_mdl_tbl |> 
  ggplot(
    aes(x = date, y = diff)
  ) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-0.3, 0.3) +
  theme_minimal()



plot_grid(
  comp_rep_plot,
  comp_mdl_plot,
  rep_mdl_plot,
  ncol = 1
)

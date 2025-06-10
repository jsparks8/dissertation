# Assumes anovas_full.R has been run and those datasets are in the environment


sim_str <- data.frame(
  cell = paste0("Cell_", c(1:8)),
  CELL = c(1:8),
  num_cats = as.factor(rep(c(6, 2), each = 2, times = 2)),
  test_length = as.factor(rep(c(10, 30), 4)),
  sample_size = as.factor(rep(c(500, 2000), each = 4))
)

CELL_ALL <- c(1:8)
REP_ALL <- c(1:10)

rsqd <- data.frame()

for (c in 1:length(CELL_ALL)) {
  for (reps in 1:length(REP_ALL)) {
    # for (c in 8) {
    # for (reps in 10) {
    CELL <- CELL_ALL[c]
    REP <- REP_ALL[reps]

    # Call appropriate directories
    starting.directory <- data.frame(folders = c(list.dirs(path = "Simulation"))) %>%
      filter(str_detect(folders, paste0("CELL", !!CELL)) &
        str_detect(folders, paste0("REP", !!REP, "$"))) %>%
      dplyr::pull()

    file_prefix <- paste0(starting.directory, "/", str_sub(starting.directory, 12), "_")

    print(file_prefix)

    # Import Estimates
    rt_true0 <- readxl::read_excel(
      path = paste0(file_prefix, "DATASET.xlsx"),
      sheet = "rt true", col_names = FALSE
    ) %>%
      dplyr::mutate(id = row_number()) %>%
      pivot_longer(cols = -id, names_to = "qid", values_to = "rt") %>%
      dplyr::mutate(qid = str_replace(qid, "...", "X"))


    start_theta <- compare_df %>%
      dplyr::filter(cell == paste0("Cell_", CELL) &
        rep == paste0("Rep_", REP) &
        family == "theta") %>%
      dplyr::select(ggumrt_estimate)

    start_delta <- compare_df %>%
      dplyr::filter(cell == paste0("Cell_", CELL) &
        rep == paste0("Rep_", REP) &
        family == "b") %>%
      dplyr::select(ggumrt_estimate)

    rt0 <- compare_df %>%
      dplyr::filter(cell == paste0("Cell_", CELL) &
        rep == paste0("Rep_", REP) &
        family == "RT_0") %>%
      dplyr::pull(ggumrt_estimate)

    rt1 <- compare_df %>%
      dplyr::filter(cell == paste0("Cell_", CELL) &
        rep == paste0("Rep_", REP) &
        family == "RT_1") %>%
      dplyr::pull(ggumrt_estimate)

    rt2 <- compare_df %>%
      dplyr::filter(cell == paste0("Cell_", CELL) &
        rep == paste0("Rep_", REP) &
        family == "RT_2") %>%
      dplyr::pull(ggumrt_estimate)

    theta_delta_sqd <- data.frame()
    for (N_use in 1:nrow(start_theta)) {
      for (J_use in 1:nrow(start_delta)) {
        theta <- as.numeric(start_theta[N_use, ])
        delta <- as.numeric(start_delta[J_use, ])
        temp <- data.frame(
          id = N_use, qid = paste0("X", J_use),
          distance_sqd = (theta - delta)^2
        )
        theta_delta_sqd <- bind_rows(theta_delta_sqd, temp)
      }
    }

    df_rsqd <- theta_delta_sqd %>%
      dplyr::full_join(rt_true0, by = c("id", "qid")) %>%
      dplyr::mutate(
        rt0 = !!rt0,
        rt1 = !!rt1,
        rt2 = !!rt2,
        e = rt - rt0 - rt1 * distance_sqd - rt2 * (distance_sqd^2)
      )

    # calculate rsqd
    temp <- df_rsqd %>%
      dplyr::mutate(pred = rt0 + rt1 * distance_sqd + rt2 * I(distance_sqd^2),
                    res= rt-pred)
    pred <- temp$pred
    n <- length(pred)
    #res <- resid(model)
    res <- temp$res
    w <- rep(1, n)
    rss <- sum(w * res ^ 2)
    resp <- pred + res
    center <- weighted.mean(resp, w)
    
    r.df <- nrow(df_rsqd)-3
    int.df <- 1
    tss <- sum(w * (resp - center)^2)
    r.sq <- 1 - rss/tss
    adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
    
    rsqd0 <- data.frame(cell = CELL, rep = REP, rsqd = adj.r.sq)
    rsqd <- bind_rows(rsqd, rsqd0)
  }
}

rsqd %>%
  dplyr::left_join(sim_str, by=c("cell"= "CELL")) %>%
# dplyr::group_by(num_cats) %>%
 # dplyr::group_by(test_length) %>%
  dplyr::group_by(sample_size) %>%
  dplyr::summarize(mean= mean(rsqd),
                   min = min(rsqd),
                   max = max(rsqd)) %>%
  clipr::write_clip()

m0 <-  aov(
  as.formula(
    paste0("rsqd ~ num_cats*test_length*sample_size")
  ), data=(rsqd %>%
    dplyr::left_join(sim_str, by=c("cell"= "CELL")) )
)

store<- as.data.frame(lsr::etaSquared(m0)) %>% rownames_to_column(var="term")

m1 <- tidy(m0  )%>%
  dplyr::left_join(store, by="term") %>%
  dplyr::mutate(sig = ifelse(p.value <= 0.05, "sig", NA_character_),
                interp = ifelse(eta.sq >= 0.1, "yes", NA_character_)) %>%
  dplyr::select(term, eta.sq, sig, interp)

m1

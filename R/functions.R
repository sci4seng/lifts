# functions.R
#
# Companion library for the lift_*.Rmd notebooks. Written in
# kaiaulu's library style: snake_case verb_noun, data.table-first,
# roxygen comments on every exported helper.
#
# These functions are CANDIDATES for upstream contribution into
# kaiaulu's R/ directory. Each is self-contained.

require(data.table)
require(stringi)
require(magrittr)

# ---- Brooks model helpers ------------------------------------------------

#' Detect Late-Hire Events from Git Log
#'
#' A late hire is a developer whose first commit is at least
#' \code{min_project_age_days} after the project's first commit.
#' Returns a data.table with one row per late hire.
#'
#' @param project_git A gitlog data.table (output of parse_gitlog +
#'   optional identity_match). Must contain identity_id (preferred)
#'   or author_name_email and author_datetimetz columns.
#' @param min_project_age_days Numeric. Minimum days after project
#'   start for a hire to count as "late." Default 365.
#' @return data.table with columns: identity_id (or
#'   author_name_email), first_commit_at, days_after_project_start.
#' @export
detect_late_hires <- function(project_git, min_project_age_days = 365) {
  # Prefer identity_match'd id when present; otherwise raw email.
  # Without identity_id, a dev appearing as both x@old.com and x@new.com
  # would count as two distinct "first commits" → inflated hire count.
  id_col <- if ("identity_id" %in% names(project_git)) "identity_id"
            else "author_name_email"

  # First commit per identity = individual's join date.
  first_commits <- project_git[, .(
    first_commit_at = min(author_datetimetz)
  ), by = id_col]

  # Earliest commit across whole repo = project birth.
  project_start <- min(project_git$author_datetimetz)
  first_commits[, days_after_project_start :=
    as.numeric(difftime(first_commit_at, project_start, units = "days"))]

  # Drop founders + early hires; keep only late arrivals whose first commit
  # is >= min_project_age_days after project birth (Brooks's "added to a
  # late project" cohort).
  first_commits[days_after_project_start >= min_project_age_days]
}

#' Compute Veteran Velocity Before and After Each Late-Hire Event
#'
#' For each late hire, compute the commit rate of veterans (devs who
#' joined before the hire) in the windows before and after. Returns
#' one row per late hire.
#'
#' @param project_git A gitlog data.table.
#' @param late_hires Output of \code{detect_late_hires()}.
#' @param window_days Numeric. Size of the pre/post window in days.
#' @return data.table with columns: identity_id (or
#'   author_name_email), pre_velocity, post_velocity, brooks_tax.
#' @export
compute_velocity_changes <- function(project_git, late_hires,
                                     window_days = 90) {
  id_col <- if ("identity_id" %in% names(project_git)) "identity_id"
            else "author_name_email"

  # One row per late hire. The downstream brooks_tax is the relative
  # change (pre - post) / pre — Brooks's claim is post < pre because
  # new hires distract veterans (training drag + quadratic comm).
  results <- lapply(seq_len(nrow(late_hires)), function(i) {
    hire_id   <- late_hires[i, get(id_col)]
    hire_at   <- late_hires[i, first_commit_at]
    win_start <- hire_at - as.difftime(window_days, units = "days")
    win_end   <- hire_at + as.difftime(window_days, units = "days")

    # Veterans = devs who joined strictly before this hire. Excludes the
    # hire and anyone who joined after — keeps "veteran" measurement
    # uncontaminated by later cohorts arriving in the post-window.
    veterans <- project_git[author_datetimetz < hire_at,
                            unique(get(id_col))]

    # Count distinct commits (not commit-touches) so a single big merge
    # doesn't inflate either window.
    pre_commits <- project_git[
      get(id_col) %in% veterans &
      author_datetimetz >= win_start &
      author_datetimetz <  hire_at,
      uniqueN(commit_hash)
    ]
    post_commits <- project_git[
      get(id_col) %in% veterans &
      author_datetimetz >  hire_at &
      author_datetimetz <= win_end,
      uniqueN(commit_hash)
    ]

    # Velocity = commits/day so window_days choice cancels in the ratio.
    data.table(
      id            = hire_id,
      hire_at       = hire_at,
      pre_velocity  = pre_commits  / window_days,
      post_velocity = post_commits / window_days
    )
  })
  rbindlist(results)
}

# ---- Brooksq model helpers (SZZ-driven) ----------------------------------

#' Parse PyDriller B-SZZ Output
#'
#' Reads a CSV produced by an external PyDriller SZZ pass and returns
#' a data.table joinable to a gitlog on commit_hash. Each row is one
#' (fixing_commit -> introducing_commit, file) triple.
#'
#' @param szz_csv_path Path to the SZZ CSV. Expected columns:
#'   fixing_commit_hash, introducing_commit_hash, file_path, jira_keys,
#'   fixing_date, introducing_date.
#' @return data.table with the same columns, dates parsed to POSIXct.
#' @export
parse_szz_bugfixes <- function(szz_csv_path) {
  # SZZ pairs come from an external PyDriller pass (scripts/szz_pass.py)
  # because B-SZZ requires git-blame walks over modified lines — not
  # cheap in R. We just read the CSV and normalize dates to UTC.
  dt <- data.table::fread(szz_csv_path)
  # UTC normalization: SZZ output may carry mixed TZs across projects;
  # UTC is the only safe reference for cross-project comparison.
  dt[, fixing_date       := as.POSIXct(fixing_date,       tz = "UTC")]
  dt[, introducing_date  := as.POSIXct(introducing_date,  tz = "UTC")]
  dt
}

#' Count Bug Introductions per Late-Hire Window
#'
#' For each late-hire event, counts bug-introducing commits in the
#' pre-window and post-window. Uses the SZZ pairs table: a commit is
#' "bug-introducing" if it appears as introducing_commit_hash for any
#' downstream fix. Returns one row per late hire.
#'
#' @param szz Output of \code{parse_szz_bugfixes()}.
#' @param late_hires Output of \code{detect_late_hires()}.
#' @param window_days Numeric. Pre/post window size in days.
#' @return data.table with: id, hire_at, pre_intros, post_intros,
#'   inj_rate_pre, inj_rate_post.
#' @export
compute_injection_changes <- function(szz, late_hires,
                                      window_days = 90) {
  id_col <- if ("identity_id" %in% names(late_hires)) "identity_id"
            else "author_name_email"
  # Dedupe: an introducing commit can appear N times if it caused N fixes.
  # We want commits-introduced-per-day, not fixes-per-day, so collapse.
  intros <- unique(szz[, .(introducing_commit_hash, introducing_date)])
  results <- lapply(seq_len(nrow(late_hires)), function(i) {
    hire_at   <- late_hires[i, first_commit_at]
    win_start <- hire_at - as.difftime(window_days, units = "days")
    win_end   <- hire_at + as.difftime(window_days, units = "days")
    # Count introductions in pre vs post window. Brooksq's thesis says
    # post > pre because new hires inject bugs at a higher rate than
    # the steady-state veteran cohort.
    pre_n  <- intros[introducing_date >= win_start &
                     introducing_date <  hire_at, .N]
    post_n <- intros[introducing_date >  hire_at &
                     introducing_date <= win_end, .N]
    data.table(
      id            = late_hires[i, get(id_col)],
      hire_at       = hire_at,
      pre_intros    = pre_n,
      post_intros   = post_n,
      # Rate = intros/day — window_days cancels out in the pre/post ratio.
      inj_rate_pre  = pre_n  / window_days,
      inj_rate_post = post_n / window_days
    )
  })
  rbindlist(results)
}

#' Estimate Leak Rate from SZZ Pairs
#'
#' Leak rate = fraction of introductions that are still un-fixed
#' beyond a given latency threshold. Higher = more bugs leak past the
#' immediate review window into the field.
#'
#' @param szz Output of \code{parse_szz_bugfixes()}.
#' @param latency_days Numeric. Bugs taking longer than this to fix
#'   count as "leaked." Default 30.
#' @return Numeric in [0,1].
#' @export
estimate_leak_rate <- function(szz, latency_days = 30) {
  latencies <- as.numeric(difftime(szz$fixing_date,
                                   szz$introducing_date,
                                   units = "days"))
  latencies <- latencies[is.finite(latencies) & latencies >= 0]
  if (length(latencies) == 0) return(NA_real_)
  mean(latencies > latency_days)
}

# ---- Congruence model helpers (radio-silence smell) ----------------------

#' Parse All Mbox Files in a Directory via kaiaulu::parse_mbox
#'
#' Wraps Perceval-via-kaiaulu over every \code{*.mbox} file in
#' \code{mbox_dir}. Returns a single data.table with the union of
#' message records.
#'
#' @param perceval_path Path to perceval executable.
#' @param mbox_dir Directory containing one or more .mbox files.
#' @return data.table with kaiaulu's parse_mbox columns
#'   (message_id, reply_to, sender, ...).
#' @export
parse_mbox_dir <- function(perceval_path, mbox_dir) {
  # kaiaulu's parse_mbox handles ONE file. Many projects (apache/...)
  # ship month-bucketed archives — we need the union. fill=TRUE because
  # different .mbox files can have minor column-set drift.
  files <- list.files(mbox_dir, pattern = "\\.mbox$",
                      full.names = TRUE)
  if (length(files) == 0) {
    stop("no .mbox files in ", mbox_dir)
  }
  rbindlist(lapply(files, function(f) {
    # tryCatch + warn-and-skip: a single corrupt mbox shouldn't kill
    # the lift on the other 100+ month buckets.
    tryCatch(parse_mbox(perceval_path, f),
             error = function(e) {
               warning(sprintf("parse_mbox failed on %s: %s",
                               basename(f), conditionMessage(e)))
               NULL
             })
  }), fill = TRUE)
}

#' Build Reply Edge List from Mbox Messages
#'
#' Resolves each (child, parent_message_id) reply link to a (child,
#' parent_identity) edge by looking up the parent message's sender.
#'
#' kaiaulu::parse_mbox returns columns:
#'   reply_id, in_reply_to_id, reply_from, ...
#' After identity_match on \code{reply_from}, each row gets
#' identity_id. We pivot on (reply_id → identity_id) to build edges.
#'
#' @param msgs data.table with cols reply_id, in_reply_to_id,
#'   identity_id (post-identity_match).
#' @return data.table with cols src_id, dst_id, weight.
#' @export
build_reply_edges <- function(msgs) {
  # message-id → identity lookup. Each message has its own sender; a
  # reply pivots to that sender's identity via the parent message-id.
  mid_to_id <- setNames(msgs$identity_id, msgs$reply_id)
  replies <- msgs[!is.na(in_reply_to_id) & in_reply_to_id != "",
                  .(child_id = identity_id, parent_mid = in_reply_to_id)]
  replies[, parent_id := mid_to_id[parent_mid]]
  # Drop dangling refs (parent message not in our archive) + self-replies
  # (dev replying to own thread = no info about coordination).
  replies <- replies[!is.na(parent_id) & parent_id != child_id]
  # Symmetrise: undirected graph for community detection.
  # pmin/pmax gives canonical edge orientation so (a,b) and (b,a) merge.
  replies[, `:=`(
    src_id = pmin(child_id, parent_id),
    dst_id = pmax(child_id, parent_id)
  )]
  # Edge weight = reply-count for this dyad — Louvain uses it as
  # connection strength.
  replies[, .(weight = .N), by = .(src_id, dst_id)]
}

#' Detect Radio-Silence Brokers
#'
#' Ports kaiaulu R/smells.R:207. A broker bridges otherwise-
#' disconnected community pairs: in any cluster, if a vertex is the
#' SOLE outgoing edge to some other cluster, it is a "radio silence"
#' broker — its absence severs the inter-cluster channel.
#'
#' Implementation: build a Louvain partition of the largest connected
#' component, then for each (cluster, vert) compute the count of
#' edges to each external cluster; flag vert as broker if any
#' external-cluster edge count == 1.
#'
#' @param edges data.table of (src_id, dst_id, weight) on identity_ids.
#' @return list with: graph, partition (named vec id→cluster),
#'   brokers (unique ids), incidents (data.table of one row per
#'   (cluster, dev, sole-cluster-bridged-to) tuple), cluster_sizes.
#' @export
detect_radio_silence <- function(edges) {
  g <- igraph::graph_from_data_frame(
    d = edges[, .(src_id, dst_id, weight)],
    directed = FALSE)
  # Restrict to the largest connected component. Tiny disconnected
  # islands (1-2 devs who only emailed each other) can't carry the
  # "boundary spanner" concept the smell tests for.
  ccs <- igraph::components(g)
  main_ids <- which(ccs$membership == which.max(ccs$csize))
  g_main <- igraph::induced_subgraph(g, main_ids)
  # Louvain = standard modularity-maximisation community detection.
  # Weights = reply counts; ensures heavy back-and-forth dyads stay
  # in the same cluster.
  louv <- igraph::cluster_louvain(g_main,
                                  weights = igraph::E(g_main)$weight)
  membership <- igraph::membership(louv)
  ids <- igraph::V(g_main)$name

  brokers   <- character(0)
  incidents <- data.table(dev = character(),
                          cluster = integer(),
                          bridge_to = integer())

  for (cid in unique(membership)) {
    devs <- ids[membership == cid]
    # Singleton cluster = the only dev in it IS by definition the
    # only bridge to anywhere else. Trivial broker.
    if (length(devs) == 1) {
      brokers <- c(brokers, devs)
      incidents <- rbind(incidents,
                         data.table(dev = devs, cluster = cid,
                                    bridge_to = NA_integer_))
      next
    }
    # Tally: per (vert, target-cluster), how many edges cross over?
    out_counts <- list()
    for (v in devs) {
      nbrs <- igraph::neighbors(g_main, v)
      nbr_ids <- ids[as.integer(nbrs)]
      nbr_cls <- membership[nbr_ids]
      ext <- nbr_cls[nbr_cls != cid]   # external-cluster edges only
      if (length(ext) == 0) next
      for (oc in unique(ext)) {
        key <- paste(oc, v, sep = "|")
        out_counts[[key]] <- (out_counts[[key]] %||% 0L) +
                             sum(ext == oc)
      }
    }
    # For each external cluster, if THIS cluster's total bridge-link
    # count to that target == 1, then the single dev carrying it is the
    # radio-silence broker — their absence severs the channel.
    by_target <- split(names(out_counts), sapply(strsplit(names(out_counts), "\\|"), `[`, 1))
    for (target_str in names(by_target)) {
      total_links <- sum(unlist(out_counts[by_target[[target_str]]]))
      if (total_links == 1) {
        key <- by_target[[target_str]][1]
        v <- strsplit(key, "\\|")[[1]][2]
        brokers <- c(brokers, v)
        incidents <- rbind(incidents,
                           data.table(dev = v, cluster = cid,
                                      bridge_to = as.integer(target_str)))
      }
    }
  }
  list(
    graph         = g_main,
    partition     = membership,
    brokers       = unique(brokers),  # dedupe: a dev may bridge to N clusters
    incidents     = incidents,
    cluster_sizes = sort(as.integer(table(membership)), decreasing = TRUE)
  )
}

# ---- Learn model helpers -------------------------------------------------

#' Compute Workforce Cohort Distribution
#'
#' For each developer, compute tenure (last_commit - first_commit) and
#' assign to Jr / Tr / Sr buckets.
#'
#' @param project_git A gitlog data.table with identity_id.
#' @param jr_max_days Tenure < this = Jr. Default 365.
#' @param sr_min_days Tenure >= this = Sr. Default 1095 (3 years).
#' @return data.table with identity_id, tenure_days, cohort.
#' @export
compute_cohorts <- function(project_git, jr_max_days = 365,
                            sr_min_days = 1095) {
  # Tenure = last - first observed commit, NOT (now - first). A dev who
  # stopped contributing 2 years ago should not keep accumulating tenure.
  per_dev <- project_git[, .(
    first_commit = min(author_datetimetz),
    last_commit  = max(author_datetimetz),
    n_commits    = uniqueN(commit_hash)
  ), by = identity_id]
  per_dev[, tenure_days := as.numeric(difftime(last_commit, first_commit,
                                               units = "days"))]
  # Three-bucket fcase reads top-down: Jr first, then Sr, then everything
  # in between is Tr. The defaults (365d / 1095d = 1y / 3y) match the
  # learn model's pipeline-stock framing in paper/sd.py.
  per_dev[, cohort := fcase(
    tenure_days <  jr_max_days,  "Jr",
    tenure_days >= sr_min_days,  "Sr",
    default                   = "Tr"
  )]
  per_dev
}

#' Estimate Transition Rates from Cohort Trajectories
#'
#' Buckets the project history into time slices and counts devs who
#' moved Jr→Tr (train) and Tr→Sr (promote) between slices. Rates =
#' transitions / starting-bucket-size, normalised to per-year.
#'
#' Slice size defaults to 90 days (not 365) to avoid the artifact where
#' jr_max_days=365 + slice=365 forces every surviving Jr to graduate,
#' saturating train_rate at 1.0.
#'
#' @param project_git A gitlog data.table with identity_id.
#' @param jr_max_days Tenure < this = Jr at slice midpoint.
#' @param sr_min_days Tenure >= this = Sr at slice midpoint.
#' @param slice_days Time-slice width. Default 90.
#' @return list with train_rate, promote_rate (medians over slices,
#'   annualised by multiplying per-slice fractions by 365/slice_days).
#' @export
estimate_transition_rates <- function(project_git, jr_max_days = 365,
                                      sr_min_days = 1095,
                                      slice_days = 90) {
  start <- min(project_git$author_datetimetz)
  end   <- max(project_git$author_datetimetz)
  step  <- as.difftime(slice_days, units = "days")
  cuts  <- seq(start, end, by = step)
  if (length(cuts) < 2) return(list(train_rate = NA_real_,
                                    promote_rate = NA_real_))

  # First-commit-date per dev (anchor for tenure)
  per_dev <- project_git[, .(first_commit = min(author_datetimetz)),
                          by = identity_id]

  cohorts_at <- function(when) {
    active <- per_dev[first_commit <= when]
    active[, td := as.numeric(difftime(when, first_commit, units = "days"))]
    active[, cohort := fcase(
      td <  jr_max_days, "Jr",
      td >= sr_min_days, "Sr",
      default          = "Tr"
    )]
    active[, .(identity_id, cohort)]
  }

  train_n   <- promote_n   <- integer(0)
  jr_at_t   <- tr_at_t     <- integer(0)
  for (i in seq_len(length(cuts) - 1)) {
    # Inner join on identity_id: a dev must exist at BOTH slice endpoints
    # to count as a transition. Devs who appear in c1 only (just-joined)
    # don't have a prior cohort to graduate FROM.
    c0 <- cohorts_at(cuts[i])
    c1 <- cohorts_at(cuts[i + 1])
    m  <- merge(c0, c1, by = "identity_id",
                suffixes = c("_0", "_1"))
    jr_at_t   <- c(jr_at_t,   sum(m$cohort_0 == "Jr"))
    tr_at_t   <- c(tr_at_t,   sum(m$cohort_0 == "Tr"))
    train_n   <- c(train_n,   sum(m$cohort_0 == "Jr" & m$cohort_1 == "Tr"))
    promote_n <- c(promote_n, sum(m$cohort_0 == "Tr" & m$cohort_1 == "Sr"))
  }

  # Annualise: per-slice fractions extrapolated to per-year. Without this,
  # a 90-day slice would underreport vs the SD model's per-year flow rate.
  annualise <- 365 / slice_days
  list(
    # pmax(_, 1) guards against zero-denominator on empty slices; median
    # over slices is more robust to outlier transitions than the mean.
    train_rate   = annualise * median(train_n   / pmax(jr_at_t, 1),
                                      na.rm = TRUE),
    promote_rate = annualise * median(promote_n / pmax(tr_at_t, 1),
                                      na.rm = TRUE),
    n_slices     = length(cuts) - 1,
    slice_days   = slice_days
  )
}

# ---- Rework model helpers ------------------------------------------------

#' Compute Failure Rate per Rolling Window
#'
#' Calibrates the rework model's \code{failrate} parameter as the
#' fraction of commits in each window that introduce a bug (per SZZ).
#'
#' @param szz Output of \code{parse_szz_bugfixes()}.
#' @param project_git A gitlog data.table.
#' @param window_days Numeric. Window size in days.
#' @return data.table with window_start, n_commits, n_intro_commits, failrate.
#' @export
compute_failrate_per_window <- function(szz, project_git, window_days = 90) {
  # Dedupe gitlog by commit_hash — a single commit can appear N rows
  # (one per touched file). We want commit-level failrate, not file-level.
  commits <- unique(project_git[, .(commit_hash, author_datetimetz)])
  intro_hashes <- unique(szz$introducing_commit_hash)
  commits[, has_intro := commit_hash %in% intro_hashes]
  commits <- commits[order(author_datetimetz)]

  start <- min(commits$author_datetimetz)
  end   <- max(commits$author_datetimetz)
  step  <- as.difftime(window_days, units = "days")
  # Tumbling (non-overlapping) windows. Each commit contributes to exactly
  # one window — important for the SD failrate to read as a steady-state
  # fraction rather than a rolling-average artifact.
  windows <- seq(start, end, by = step)
  out <- lapply(windows, function(ws) {
    we <- ws + step
    chunk <- commits[author_datetimetz >= ws & author_datetimetz < we]
    if (nrow(chunk) == 0) return(NULL)
    data.table(
      window_start    = ws,
      n_commits       = nrow(chunk),
      n_intro_commits = sum(chunk$has_intro),
      failrate        = sum(chunk$has_intro) / nrow(chunk)
    )
  })
  rbindlist(out[!sapply(out, is.null)])
}

# ---- Defmap model helpers ------------------------------------------------

#' Compute Per-Release-Phase Defect Flow
#'
#' For each release-tag phase, count:
#' - injected: bug-introducing commits whose date falls in this phase
#' - caught:   bug-fixing commits in same phase as their introducer
#' - leaked:   introducers in this phase whose fix is in a later phase
#'             or absent
#'
#' \code{tst_proxy} = caught / max(injected, 1) — calibrates the defmap
#' model's \code{tst} testing-intensity parameter.
#'
#' @param szz Output of \code{parse_szz_bugfixes()}.
#' @param project_git A gitlog data.table.
#' @param tag_dates data.table with columns: tag, date (POSIXct).
#' @return data.table per phase with injected, caught, leaked, tst_proxy.
#' @export
compute_per_phase_defects <- function(szz, project_git, tag_dates) {
  tag_dates <- tag_dates[order(date)]
  phase_for <- function(ts) {
    # findInterval returns 0 for timestamps STRICTLY before the first
    # tag — bucket them into phase 1 (treat them as "pre-history of
    # release 1") rather than dropping or NA-ing.
    idx <- findInterval(ts, tag_dates$date)
    idx[idx == 0] <- 1L
    tag_dates$tag[idx]
  }
  szz[, phase_intro := phase_for(introducing_date)]
  szz[, phase_fix   := phase_for(fixing_date)]

  # Dedupe on introducing_commit_hash: one introducer can appear N times
  # if it caused N fixes, but we count it ONCE per phase (injected==N
  # would conflate "many bugs introduced" with "one bug caused many fixes").
  intro_per_phase <- unique(szz[, .(introducing_commit_hash, phase_intro,
                                    phase_fix)])
  out <- intro_per_phase[, .(
    injected = .N,
    caught   = sum(phase_intro == phase_fix, na.rm = TRUE),
    leaked   = sum(phase_intro != phase_fix, na.rm = TRUE)
  ), by = phase_intro]
  setnames(out, "phase_intro", "phase")
  # tst_proxy = caught share. Calibrates the defmap model's `tst`
  # testing-intensity parameter: 1.0 means every injected bug is caught
  # before release; 0.0 means everything leaks.
  out[, tst_proxy := caught / pmax(injected, 1)]
  out
}

# ---- Dora model helpers --------------------------------------------------

#' Compute DORA-style Metrics from Tags + SZZ
#'
#' batch_size  : mean commits between consecutive tags
#' cfr         : (bug-fix commits) / (total commits) over the full history
#' arrival_rate: commits per day (mean)
#' rec_rate    : 1 / median(fix_date - intro_date) in days
#'
#' @param szz Output of \code{parse_szz_bugfixes()}.
#' @param project_git A gitlog data.table.
#' @param tag_dates data.table with columns: tag, date (POSIXct).
#' @return list with named numeric values.
#' @export
compute_dora_metrics <- function(szz, project_git, tag_dates) {
  commits <- unique(project_git[, .(commit_hash, author_datetimetz)])
  setorder(commits, author_datetimetz)
  total_commits <- nrow(commits)
  span_days <- as.numeric(difftime(max(commits$author_datetimetz),
                                   min(commits$author_datetimetz),
                                   units = "days"))

  # arrival_rate = commits per day over the full repo lifespan. max(_, 1)
  # guards a one-day-history degenerate (test repos) against div-by-zero.
  arrival_rate <- total_commits / max(span_days, 1)

  # batch_size = commits between releases. n_tags-1 because N tags define
  # N-1 inter-release intervals. NA when <2 tags (no measurable cadence).
  n_tags <- nrow(tag_dates)
  batch_size <- if (n_tags >= 2) total_commits / (n_tags - 1) else NA_real_

  n_fixes <- uniqueN(szz$fixing_commit_hash)
  cfr <- n_fixes / total_commits

  # MTTR uses latency-per-pair (fix - intro). Filter negatives because
  # SZZ can produce inverted pairs when a fix predates its mis-blamed
  # introducer (renames, branch reordering).
  latencies <- as.numeric(difftime(szz$fixing_date, szz$introducing_date,
                                   units = "days"))
  latencies <- latencies[is.finite(latencies) & latencies >= 0]
  median_mttr <- if (length(latencies)) median(latencies) else NA_real_
  # rec_rate = 1/MTTR so DORA's "recovery rate" reads as fixes-per-day
  # (matches the SD model's flow-rate semantics where Rate × Stock = flow).
  rec_rate <- if (is.finite(median_mttr) && median_mttr > 0) 1 / median_mttr
              else NA_real_

  list(
    batch_size   = batch_size,
    cfr          = cfr,
    arrival_rate = arrival_rate,
    rec_rate     = rec_rate,
    n_tags       = n_tags,
    span_days    = span_days
  )
}

#' Build a Tag-Date Table for a Git Repo
#'
#' Wraps git system calls to enumerate tags in v:refname order and
#' fetch each tag's commit date.
#'
#' @param git_repo_path Path to .git or worktree.
#' @return data.table with tag, date (POSIXct).
#' @export
get_tag_dates <- function(git_repo_path) {
  # Strip trailing `/.git` so kaiaulu confs (which point at the .git dir)
  # and worktree-rooted paths both resolve to the same `git -C` target.
  repo <- gsub("/\\.git/?$", "", git_repo_path)
  # --sort=v:refname = SemVer-aware tag order (v1.2 before v1.10). NOT
  # the same as chronological; final sort-by-date below fixes that.
  tags <- system2("git", c("-C", repo, "tag", "--sort=v:refname"),
                  stdout = TRUE)
  if (length(tags) == 0) return(data.table(tag = character(),
                                           date = as.POSIXct(character())))
  # %ct = author commit time as Unix seconds (TZ-stable for cross-project
  # comparison; %ci would emit a local-TZ string and need parsing).
  unix_ts <- vapply(tags, function(tg) {
    out <- system2("git",
                   c("-C", repo, "log", "-1", "--format=%ct", tg),
                   stdout = TRUE)
    if (length(out) == 0) NA_character_ else out[1]
  }, character(1))
  # Sort chronologically — caller (compute_per_phase_defects) does a
  # findInterval lookup that requires monotone-increasing dates.
  data.table(tag  = tags,
             date = as.POSIXct(as.numeric(unix_ts),
                               origin = "1970-01-01", tz = "UTC"))[
                                 order(date)]
}

# ---- Debt model helpers --------------------------------------------------

#' Compute Pay Rate from Refactoring Activity
#'
#' Pay rate = fraction of commits in each rolling window that contain
#' at least one RefactoringMiner-detected refactoring. Calibrates the
#' \code{pay_rate} parameter of the debt SD model.
#'
#' @param project_git A gitlog data.table.
#' @param refactorings Output of \code{flatten_refactoring_json()}.
#' @param window_days Numeric. Rolling window size in days. Default 90.
#' @return data.table with: window_start, window_end, n_commits,
#'   n_refactor_commits, pay_rate.
#' @export
compute_pay_rate <- function(project_git, refactorings,
                             window_days = 90) {
  # Commit-level (not file-level) is the right granularity: pay_rate in
  # the debt SD model is share of WORK that addresses debt, and one
  # refactor-commit = one unit of work regardless of how many files it spans.
  commits <- unique(project_git[, .(commit_hash, author_datetimetz)])
  refactor_hashes <- unique(refactorings$commit_hash)
  commits[, has_refactor := commit_hash %in% refactor_hashes]
  commits <- commits[order(author_datetimetz)]

  start <- min(commits$author_datetimetz)
  end   <- max(commits$author_datetimetz)
  step  <- as.difftime(window_days, units = "days")

  # Tumbling windows. Output one row per 90-day slice; aggregation up to
  # a single pay_rate scalar is the caller's responsibility (median of
  # window pay_rates is more robust to release-cycle bursts than overall
  # mean).
  windows <- seq(start, end, by = step)
  out <- lapply(windows, function(ws) {
    we <- ws + step
    chunk <- commits[author_datetimetz >= ws & author_datetimetz < we]
    if (nrow(chunk) == 0) return(NULL)
    data.table(
      window_start       = ws,
      window_end         = we,
      n_commits          = nrow(chunk),
      n_refactor_commits = sum(chunk$has_refactor),
      pay_rate           = sum(chunk$has_refactor) / nrow(chunk)
    )
  })
  rbindlist(out[!sapply(out, is.null)])
}

#' Crude Born-Rate Proxy from Gitlog Churn
#'
#' Approximates the debt model's \code{born_rate} as the share of
#' commits in each window that touch many files (a proxy for "ship
#' fast → introduce debt"). Threshold defaults to 5 files.
#'
#' @param project_git A gitlog data.table.
#' @param window_days Numeric. Window size in days.
#' @param big_commit_files Numeric. Min files for "big commit." Default 5.
#' @return data.table with: window_start, n_commits, n_big_commits,
#'   born_rate.
#' @export
compute_born_rate_proxy <- function(project_git, window_days = 90,
                                    big_commit_files = 5) {
  # Per-commit aggregate: files_touched = breadth, when = commit time.
  # min(author_datetimetz) is a no-op for clean gitlogs (all rows of a
  # commit share a date) but defensive against parser quirks.
  cs <- project_git[, .(
    files_touched = uniqueN(file_pathname),
    when          = min(author_datetimetz)
  ), by = commit_hash]
  # "Big commit" = touches >= big_commit_files files. The 5-file default
  # is the crude proxy for "shipping fast / introducing debt" — wider
  # blast radius commits correlate weakly with debt accrual. NOT a
  # ground-truth indicator; a better one would use RefMiner anti-refactors.
  cs[, big := files_touched >= big_commit_files]

  start <- min(cs$when); end <- max(cs$when)
  step  <- as.difftime(window_days, units = "days")
  windows <- seq(start, end, by = step)
  out <- lapply(windows, function(ws) {
    we <- ws + step
    chunk <- cs[when >= ws & when < we]
    if (nrow(chunk) == 0) return(NULL)
    data.table(
      window_start  = ws,
      n_commits     = nrow(chunk),
      n_big_commits = sum(chunk$big),
      born_rate     = sum(chunk$big) / nrow(chunk)
    )
  })
  rbindlist(out[!sapply(out, is.null)])
}

# ---- Archpat model helpers -----------------------------------------------

#' Get Release Tags from a Git Repository
#'
#' Wraps system call to \code{git tag}. Returns tags in lexicographic
#' order (which usually approximates release order for SemVer projects).
#'
#' @param git_repo_path Path to the .git directory or working tree.
#' @return Character vector of tag names.
#' @export
get_release_tags <- function(git_repo_path) {
  repo <- gsub("/\\.git/?$", "", git_repo_path)
  out  <- system2("git",
                  args = c("-C", repo, "tag", "--sort=v:refname"),
                  stdout = TRUE)
  out
}

#' Check Out a Git Snapshot to a Temporary Directory
#'
#' Creates a worktree at the given tag/commit. Returns the worktree
#' path. Caller is responsible for cleanup via
#' \code{system2("git", c("-C", repo, "worktree", "remove", path))}.
#'
#' @param git_repo_path Path to the .git directory or working tree.
#' @param ref Tag name, branch name, or commit hash.
#' @return Character path to the snapshot directory.
#' @export
checkout_snapshot <- function(git_repo_path, ref) {
  repo <- gsub("/\\.git/?$", "", git_repo_path)
  snap <- tempfile(pattern = paste0("snap_", gsub("/", "_", ref), "_"))
  system2("git",
          args = c("-C", repo, "worktree", "add", "--detach", snap, ref),
          stdout = FALSE, stderr = FALSE)
  snap
}

#' Run RefactoringMiner on a Git Repository
#'
#' System call to the RefactoringMiner CLI. Returns the path to the
#' resulting JSON file. The JSON is the standard RefactoringMiner
#' \code{-all} output (every refactoring across the project history).
#'
#' @param refminer_jar Path to RefactoringMiner-*.jar.
#' @param git_repo_path Path to the .git directory or working tree.
#' @param out_path Optional output file path. Default: tempfile.
#' @return Character path to the resulting JSON file.
#' @export
run_refactoring_miner <- function(refminer_jar, git_repo_path,
                                  out_path = NULL) {
  repo <- gsub("/\\.git/?$", "", git_repo_path)
  if (is.null(out_path)) {
    out_path <- tempfile(fileext = ".json")
  }
  system2("java",
          args = c("-jar", refminer_jar,
                   "-a", repo,        # -a = all commits
                   "-json", out_path),
          stdout = FALSE, stderr = FALSE)
  out_path
}

#' Flatten RefactoringMiner JSON to a data.table
#'
#' kaiaulu's parse_java_code_refactoring_json returns a nested list.
#' This helper flattens to one row per refactoring event.
#'
#' @param refminer_json_path Path to a RefactoringMiner JSON output
#'   file (from \code{run_refactoring_miner}).
#' @return data.table with: commit_hash, refactoring_type,
#'   refactoring_description, left_locations, right_locations.
#' @export
flatten_refactoring_json <- function(refminer_json_path) {
  # simplifyVector=FALSE preserves the nested {commits → refactorings →
  # leftSideLocations} structure as R lists; default TRUE coerces to
  # arrays and loses the per-event grouping needed for the outer lapply.
  raw <- jsonlite::fromJSON(refminer_json_path, simplifyVector = FALSE)
  # Outer lapply: one entry per COMMIT (may have 0+ refactorings).
  # Inner lapply: one row per REFACTORING within that commit.
  # Two-level structure flattens to a clean row-per-event data.table.
  rows <- lapply(raw$commits, function(c) {
    if (length(c$refactorings) == 0) return(NULL)
    rbindlist(lapply(c$refactorings, function(r) {
      data.table(
        commit_hash             = c$sha1,
        refactoring_type        = r$type,
        refactoring_description = r$description %||% NA_character_,
        # Semicolon-join because multiple files can participate in one
        # refactoring (e.g. Move Method has left = src file, right = dest).
        # Comma would collide with later CSV serialisation.
        left_locations  = paste(sapply(r$leftSideLocations,  `[[`,
                                       "filePath"), collapse = ";"),
        right_locations = paste(sapply(r$rightSideLocations, `[[`,
                                       "filePath"), collapse = ";")
      )
    }))
  })
  rbindlist(rows[!sapply(rows, is.null)])
}

#' Compute Per-File Bug Frequency
#'
#' Joins gitlog (with commit_message_id) to JIRA bugs and counts
#' bug-touching commits per file.
#'
#' @param project_git A gitlog data.table that has been processed
#'   through parse_commit_message_id.
#' @param jira_bugs A data.table of bug issues from
#'   parse_jira()$issues filtered to issue_type == "Bug".
#' @param issue_id_regex Used to extract the issue key from the
#'   commit_message_id column.
#' @return data.table with file_pathname, bug_count.
#' @export
compute_file_bug_frequency <- function(project_git, jira_bugs,
                                       issue_id_regex) {
  # commit_message_id is added upstream by parse_commit_message_id (the
  # gitlog row carries the JIRA key extracted from the commit message
  # via issue_id_regex). Match against jira_bugs$issue_key to find the
  # commits that reference a Bug-typed issue.
  bug_keys <- jira_bugs$issue_key
  bug_commits <- project_git[commit_message_id %in% bug_keys]
  # uniqueN(commit_hash) per file: count distinct bug-fixing commits, not
  # row count. Same commit touching N files = N file_pathname rows.
  bug_commits[, .(bug_count = uniqueN(commit_hash)), by = file_pathname]
}

#' Compute Per-File Churn over a Recent Window
#'
#' @param project_git A gitlog data.table.
#' @param window_days Numeric. Window size for "recent."
#' @return data.table with file_pathname, churn_score in [0,1].
#' @export
compute_file_churn <- function(project_git, window_days = 180) {
  # Cutoff is anchored to the LATEST commit in the repo, not wall-clock
  # now. Important for stale snapshots — a 2020 archive shouldn't
  # report 0 churn just because "now" is 2026.
  cutoff <- max(project_git$author_datetimetz) -
           as.difftime(window_days, units = "days")
  recent <- project_git[author_datetimetz >= cutoff]
  recent_commits <- recent[, .(recent_n = uniqueN(commit_hash)),
                           by = file_pathname]
  total_commits  <- project_git[, .(total_n = uniqueN(commit_hash)),
                                by = file_pathname]
  # all.y=TRUE: keep files with zero recent activity (churn_score=0)
  # so the partition function can place them in Other rather than dropping.
  m <- merge(recent_commits, total_commits, by = "file_pathname",
             all.y = TRUE)
  m[is.na(recent_n), recent_n := 0]
  # Ratio (recent / total) is project-size invariant — comparable across
  # 100-file and 100k-file projects.
  m[, churn_score := recent_n / total_n]
  m[, .(file_pathname, churn_score)]
}

#' Assign Each File to a Stock (Patterned, Legacy, or Drift)
#'
#' Implements the archpat model's partition. A file is Patterned if
#' it participates in any GoF pattern instance; Legacy if it has
#' high accumulated bug count and is not Patterned; Drift if it has
#' recent high churn and is not Patterned or Legacy.
#'
#' @param patterned_files Character vector of file paths in GoF
#'   patterns.
#' @param file_bug_freq Output of compute_file_bug_frequency.
#' @param file_churn Output of compute_file_churn.
#' @param legacy_bug_threshold Numeric. Min bug count for Legacy.
#' @param drift_churn_threshold Numeric in [0,1]. Min churn for Drift.
#' @return data.table with file_pathname, stock in
#'   {"Patterned","Legacy","Drift","Other"}.
#' @export
assign_file_partition <- function(patterned_files,
                                  file_bug_freq,
                                  file_churn,
                                  legacy_bug_threshold  = 5,
                                  drift_churn_threshold = 0.7) {
  # Union of all files that have ANY signal (bug or churn). Files with
  # neither are by definition Other and get dropped — they don't
  # contribute to any stock count.
  all_files <- union(file_bug_freq$file_pathname,
                     file_churn$file_pathname)
  out <- data.table(file_pathname = all_files)
  out <- merge(out, file_bug_freq, by = "file_pathname", all.x = TRUE)
  out <- merge(out, file_churn,    by = "file_pathname", all.x = TRUE)
  # Missing signals → 0, NOT NA. Allows threshold comparisons to work
  # without per-row NA-handling later.
  out[is.na(bug_count), bug_count := 0]
  out[is.na(churn_score), churn_score := 0]
  # Priority order matters: Patterned wins over Legacy wins over Drift.
  # A file in a GoF pattern is "good" regardless of how buggy or churning
  # it is (the pattern is the architectural classification, not the
  # quality metric). Drift only applies to files NOT already in the
  # other two categories — recent activity alone doesn't make a file
  # legacy or patterned.
  out[, stock := fcase(
    file_pathname %in% patterned_files,                       "Patterned",
    bug_count    >= legacy_bug_threshold,                     "Legacy",
    churn_score  >= drift_churn_threshold,                    "Drift",
    default = "Other"
  )]
  out
}

# ---- Stubs for remaining archpat lifts -----------------------------------
# These need implementations once we have pattern4 + RefactoringMiner
# actually running on Helix. Names are placeholders matching the model's
# init keys.

#' @export
compute_feature_velocity      <- function(project_git, jira_issues) NA_real_
#' @export
compute_migration_rate        <- function(gof_per_tag) NA_real_
#' @export
compute_smell_appearance_rate <- function(gof_per_tag) NA_real_
#' @export
compute_legacy_growth_rate    <- function(project_git, patterned_files) NA_real_
#' @export
compute_born_pat_rate         <- function(refactorings, gof_per_tag) NA_real_
#' @export
compute_born_leg_rate         <- function(project_git, patterned_files) NA_real_

# ---- Jira issue extraction ----------------------------------------------

#' Extract Jira Issues and Comments for a Project
#'
#' Reads every \code{*.json} dump under the project's
#' \code{issue_tracker$jira$project_key_1$issues} and
#' \code{issue_comments} directories, runs \code{parse_jira} on each,
#' and row-binds the result. Returns a named list with two
#' data.tables, mirroring \code{parse_jira}'s schema.
#'
#' The issues directory typically holds issue-only dumps; the
#' issue_comments directory holds dumps that include both. Parsing
#' both and de-duplicating on \code{issue_key} keeps the union without
#' double-counting.
#'
#' @param project_conf A parsed kaiaulu project yaml (output of
#'   \code{parse_config}). Must contain
#'   \code{issue_tracker$jira$project_key_1$issues} and
#'   \code{issue_tracker$jira$project_key_1$issue_comments}.
#' @return list with \code{issues} (data.table, one row per issue)
#'   and \code{comments} (data.table, one row per comment).
#' @export
lift_project_jira <- function(project_conf) {
  jira_conf <- project_conf$issue_tracker$jira$project_key_1
  if (is.null(jira_conf)) {
    stop("No issue_tracker$jira$project_key_1 in project_conf")
  }

  # kaiaulu::parse_jira takes a directory and globs everything in it,
  # including .DS_Store. Stage just the .json files into a clean
  # tempdir to avoid choking the JSON reader on Mac sidecar files.
  stage_jsons <- function(src_dir) {
    if (is.null(src_dir) || !dir.exists(src_dir)) return(NULL)
    jsons <- list.files(src_dir, pattern = "\\.json$",
                        full.names = TRUE)
    if (length(jsons) == 0) return(NULL)
    td <- tempfile("jira_stage_"); dir.create(td)
    file.copy(jsons, td)
    td
  }

  parse_dir <- function(src_dir) {
    staged <- stage_jsons(src_dir)
    if (is.null(staged)) {
      return(list(issues = data.table(), comments = data.table()))
    }
    kaiaulu::parse_jira(staged)
  }

  parts <- list(parse_dir(jira_conf$issues),
                parse_dir(jira_conf$issue_comments))

  all_issues   <- rbindlist(lapply(parts, `[[`, "issues"),
                            fill = TRUE)
  all_comments <- rbindlist(lapply(parts, `[[`, "comments"),
                            fill = TRUE)

  # Issues dir and issue_comments dir overlap on issue_key
  if (nrow(all_issues) > 0) {
    all_issues <- unique(all_issues, by = "issue_key")
  }
  if (nrow(all_comments) > 0) {
    all_comments <- unique(all_comments,
                           by = c("issue_key", "comment_id"))
  }

  list(issues = all_issues, comments = all_comments)
}

# ---- Utility -------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Merge Commit Dates Back into a Refactoring Table
#'
#' RefactoringMiner output has commit_hash but no date. Join from
#' gitlog to recover the commit_datetimetz.
#'
#' @export
merge_commit_dates <- function(refactorings, project_git) {
  hashes <- unique(project_git[, .(commit_hash, author_datetimetz)])
  m <- merge(refactorings, hashes, by = "commit_hash", all.x = TRUE)
  m$author_datetimetz
}

# ---- aiwork model helpers (gitlog churn + per-author span) ---------------

#' Default Subject-Line Pattern for Churn Detection
#'
#' Borrowed from common revert / rollback / amend conventions. Matches
#' subject lines that signal the author replaced earlier work rather
#' than producing net-new output. Case-insensitive.
#'
#' @export
AIWORK_CHURN_RX <- paste(
  "\\b(revert|rollback|undo|reset|fix typo|backout|reapply|",
  "re-apply|amend|hotfix)\\b",
  sep = "")

#' Count Churn Commits in a Git Log
#'
#' Flags commits whose subject matches a churn pattern (revert / undo
#' / hotfix etc.). Returns (n_churn, n_commits, churn_base) where
#' \code{churn_base} is the share of all commits that are churn.
#'
#' GitClear / METR treat AI-driven churn as a multiplicative inflation
#' on top of this no-AI baseline, so the lifted \code{churn_base} can
#' calibrate the \code{aiwork} model's baseline churn parameter.
#'
#' @param project_git A gitlog data.table with a \code{commit_message}
#'   (or \code{message}) column. The first line of the message is
#'   treated as the subject.
#' @param pattern A regex string; defaults to \code{AIWORK_CHURN_RX}.
#' @return A 1-row data.table with n_churn, n_commits, churn_base.
#' @export
detect_churn_commits <- function(project_git,
                                 pattern = AIWORK_CHURN_RX) {
  msg_col <- if ("commit_message" %in% names(project_git))
               "commit_message"
             else if ("message" %in% names(project_git))
               "message"
             else stop("project_git lacks commit_message / message")

  # Subject = first line of the commit message. Some parsers strip the
  # newline; in that case the whole message is the subject.
  subjects <- vapply(project_git[[msg_col]], function(m) {
    if (is.na(m)) "" else stringi::stri_split_lines1(m)[1]
  }, character(1))

  n_commits <- length(subjects)
  is_churn  <- stringi::stri_detect_regex(subjects, pattern,
                                          opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  n_churn   <- sum(is_churn, na.rm = TRUE)
  data.table(
    n_commits  = n_commits,
    n_churn    = n_churn,
    churn_base = if (n_commits > 0) n_churn / n_commits else 0
  )
}

#' Compute Per-Author Span Rate (aiwork mature_rate proxy)
#'
#' For each author, span = days between their first and last commit.
#' Returns (mean_span_days, n_authors, mature_rate) where
#' \code{mature_rate = 1 / mean_span_days} is the proxy used to
#' calibrate \code{aiwork}'s Wip -> Kept transition rate. Authors
#' with only one commit (span = 0) are dropped from the mean.
#'
#' @param project_git A gitlog data.table with author and
#'   author_datetimetz columns. Uses identity_id if present.
#' @return 1-row data.table with mean_span_days, n_authors,
#'   mature_rate.
#' @export
compute_author_span_rate <- function(project_git) {
  id_col <- if ("identity_id" %in% names(project_git)) "identity_id"
            else "author_name_email"
  spans <- project_git[, .(
    first_at = min(author_datetimetz),
    last_at  = max(author_datetimetz)
  ), by = id_col]
  spans[, span_days := as.numeric(difftime(last_at, first_at,
                                           units = "days"))]
  active <- spans[span_days > 0]
  mean_span <- if (nrow(active) > 0) mean(active$span_days) else 0
  data.table(
    n_authors      = nrow(spans),
    mean_span_days = mean_span,
    mature_rate    = if (mean_span > 0) 1 / mean_span else 0
  )
}

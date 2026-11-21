# distinct_state / merge_distinct_state / collect_distinct_report --------

shards <- function() {
  list(
    a = tibble(
      region = c("a", "a", "b", "b", NA),
      channel = c("x", "x", "y", "y", "x"),
      user_id = c(1L, 1L, 2L, 3L, NA)
    ),
    b = tibble(
      region = c("a", "b", "b"),
      channel = c("x", "y", "y"),
      user_id = c(4L, 2L, NA)
    )
  )
}

merged_state <- function() {
  s <- shards()
  merge_distinct_state(
    distinct_state(s$a, c(region, channel), user_id),
    distinct_state(s$b, c(region, channel), user_id)
  )
}

test_that("distinct_state() deduplicates and keeps zero-person groups", {
  s <- shards()
  st <- distinct_state(s$a, c(region, channel), user_id)

  expect_true(is_distinct_state(st))
  expect_false(is_distinct_state(1L))
  # (a,x,1) appeared twice, the rest once: 3 unique records
  expect_equal(vec_size(st$records), 3L)
  # the missing-id row leaves one zero-person group (NA, x)
  expect_equal(vec_size(st$zero_groups), 1L)
  expect_s3_class(st, "tbl_df_distinct_state")
  expect_snapshot_output(print(st))
})

test_that("full cube has detail, subtotal and grand-total rows", {
  rep <- collect_distinct_report(merged_state())

  expect_s3_class(rep, "tbl_df")
  expect_named(rep, c("region", "channel", "n", ".grouping"))
  expect_equal(nrow(rep), 8L)
  expect_type(rep$n, "integer")
  expect_type(rep$.grouping, "integer")

  # Grand total: users {1, 2, 3, 4} across both shards
  expect_equal(rep$n[rep$.grouping == 3L], 4L)
  # Region subtotals, deduplicated at the region level
  expect_equal(
    rep$n[rep$.grouping == 2L & !is.na(rep$region) & rep$region == "a"],
    2L
  )
  expect_equal(
    rep$n[rep$.grouping == 2L & !is.na(rep$region) & rep$region == "b"],
    2L
  )
  # Channel subtotals
  expect_equal(
    rep$n[rep$.grouping == 1L & !is.na(rep$channel) & rep$channel == "x"],
    2L
  )
  expect_equal(
    rep$n[rep$.grouping == 1L & !is.na(rep$channel) & rep$channel == "y"],
    2L
  )
  # Detail level: (a, x) holds users 1 and 4; (NA, x) is the zero group
  expect_equal(
    rep$n[rep$.grouping == 0L & !is.na(rep$region) & rep$region == "a"],
    2L
  )
  expect_equal(rep$n[rep$.grouping == 0L & is.na(rep$region)], 0L)
})

test_that("a user spanning groups is counted once at every level", {
  events <- tibble(
    region = c("a", "a", "b", "b", "a"),
    channel = c("x", "y", "x", "y", "x"),
    uid = c(1L, 1L, 1L, 2L, 1L)
  )
  st <- merge_distinct_state(
    distinct_state(events[1:3, ], c(region, channel), uid),
    distinct_state(events[4:5, ], c(region, channel), uid)
  )
  rep <- collect_distinct_report(st)

  # Only two distinct users in total
  expect_equal(rep$n[rep$.grouping == 3L], 2L)
  # User 1 appears under both channels but once in each subtotal
  expect_equal(
    rep$n[rep$.grouping == 2L & !is.na(rep$region) & rep$region == "a"],
    1L
  )
  expect_equal(
    rep$n[rep$.grouping == 1L & !is.na(rep$channel) & rep$channel == "x"],
    1L
  )
})

test_that("merging is order-independent and idempotent", {
  s <- shards()
  st1 <- distinct_state(s$a, c(region, channel), user_id)
  st2 <- distinct_state(s$b, c(region, channel), user_id)

  ref <- collect_distinct_report(merge_distinct_state(st1, st2))

  expect_identical(
    ref,
    collect_distinct_report(merge_distinct_state(st2, st1))
  )
  expect_identical(
    ref,
    collect_distinct_report(merge_distinct_state(st1, st2, st1, st2))
  )
  expect_identical(
    ref,
    collect_distinct_report(
      merge_distinct_state(merge_distinct_state(st1, st2), st2)
    )
  )
  # Nested lists and NULLs are flattened
  expect_identical(
    ref,
    collect_distinct_report(
      merge_distinct_state(list(st1), list(list(st2)), NULL)
    )
  )
})

test_that("missing ids are not counted but retain zero-person groups", {
  st <- distinct_state(
    tibble(g = "k", uid = NA_integer_),
    g,
    uid
  )
  rep <- collect_distinct_report(st)

  expect_equal(rep$n[rep$.grouping == 0L], 0L)
  expect_equal(rep$n[rep$.grouping == 1L], 0L)

  # A later shard with a real id at the same key turns the count positive
  st2 <- merge_distinct_state(
    st,
    distinct_state(tibble(g = "k", uid = 9L), g, uid)
  )
  rep2 <- collect_distinct_report(st2)
  expect_equal(rep2$n[rep2$.grouping == 0L], 1L)
})

test_that("zero-person groups are not invented at coarser levels", {
  events <- tibble(
    g = c(NA, NA, "a"),
    h = c("p", "p", "q"),
    uid = c(1L, 2L, NA)
  )
  rep <- collect_distinct_report(distinct_state(events, c(g, h), uid))

  # The zero group (a, q) exists at the detail level only ...
  zero <- rep[rep$.grouping == 0L & !is.na(rep$g), ]
  expect_equal(nrow(zero), 1L)
  expect_equal(zero$g, "a")
  expect_equal(zero$n, 0L)
  # ... and must not surface as a zero row in the h or g subtotals
  h_sub <- rep[rep$.grouping == 1L, ]
  expect_equal(h_sub$h, "p")
  # Both users 1 and 2 sit under channel p
  expect_equal(h_sub$n, 2L)
  g_sub <- rep[rep$.grouping == 2L, ]
  expect_equal(nrow(g_sub), 1L)
  expect_true(is.na(g_sub$g))
})

test_that("empty input still yields a zero grand total with typed columns", {
  empty <- tibble(
    region = character(),
    channel = character(),
    user_id = integer()
  )
  st <- distinct_state(empty, c(region, channel), user_id)
  rep <- collect_distinct_report(st)

  expect_equal(nrow(rep), 1L)
  expect_equal(rep$n, 0L)
  expect_equal(rep$.grouping, 3L)
  expect_type(rep$region, "character")
  expect_type(rep$channel, "character")
  expect_type(rep$n, "integer")

  # A requested grand total with a custom count name
  rep2 <- collect_distinct_report(
    st,
    levels = list(character(0)),
    count_name = "users"
  )
  expect_true("users" %in% names(rep2))
  expect_equal(rep2$users, 0L)
})

test_that("schema-less empty state merges into any state", {
  s <- shards()
  st <- distinct_state(s$a, c(region, channel), user_id)
  empty <- merge_distinct_state()

  expect_true(is_distinct_state(empty))
  expect_identical(
    collect_distinct_report(merge_distinct_state(empty, st)),
    collect_distinct_report(st)
  )
  expect_equal(
    collect_distinct_report(empty)$n,
    0L
  )
})

test_that("real NA dimension values form their own groups", {
  st <- distinct_state(
    tibble(g = c(NA, "a", NA), uid = c(1L, 2L, 3L)),
    g,
    uid
  )
  rep <- collect_distinct_report(st)
  detail <- rep[rep$.grouping == 0L, ]

  expect_equal(nrow(detail), 2L)
  expect_equal(detail$n[is.na(detail$g)], 2L)
  expect_equal(detail$n[!is.na(detail$g) & detail$g == "a"], 1L)
})

test_that("omitted dimensions are marked separately from real NA", {
  st <- distinct_state(
    tibble(g = c(NA, "a"), h = "x", uid = 1:2),
    c(g, h),
    uid
  )
  rep <- collect_distinct_report(st)

  real_na <- rep[rep$.grouping == 0L & is.na(rep$g), ]
  subtotal <- rep[rep$.grouping == 2L, ]
  expect_equal(nrow(real_na), 1L)
  expect_true(nrow(subtotal) >= 1L)
  expect_true(real_na$.grouping[[1]] != subtotal$.grouping[[1]])
})

test_that("dimension types are preserved in the report", {
  events <- tibble(
    d = as.Date("2024-01-01") + 0:1,
    f = factor(c("p", "q")),
    uid = c("u1", "u2")
  )
  rep <- collect_distinct_report(distinct_state(events, c(d, f), uid))

  expect_s3_class(rep$d, "Date")
  expect_true(is.factor(rep$f))
  expect_equal(levels(rep$f), c("p", "q"))
  # Omitted-dimension fills keep the factor type and levels
  expect_true(is.factor(rep$f))
})

test_that("the grouping bitmask follows dimension order", {
  st <- distinct_state(
    tibble(a = "a", b = "b", c = "c", uid = 1L),
    c(a, b, c),
    uid
  )
  rep <- collect_distinct_report(st)
  expect_equal(
    sort(unique(rep$.grouping)),
    c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L)
  )
  # Grand total omits all three dimensions
  expect_equal(rep$.grouping[rep$.grouping == 7L], 7L)
})

test_that("coercible shard types resolve to the common type", {
  s1 <- distinct_state(tibble(g = 1L, uid = 1L), g, uid)
  s2 <- distinct_state(tibble(g = 2, uid = 2L), g, uid)
  merged <- merge_distinct_state(s1, s2)
  rep <- collect_distinct_report(merged)

  # integer + double combines to double, also in omitted-dimension fills
  expect_type(merged$dim_ptype$g, "double")
  expect_type(rep$g, "double")

  a1 <- distinct_state(tibble(g = 1L, h = "x", uid = 1L), c(g, h), uid)
  a2 <- distinct_state(tibble(g = 2, h = "y", uid = 2L), c(g, h), uid)
  rep2 <- collect_distinct_report(merge_distinct_state(a1, a2))
  expect_type(rep2$g, "double")
  expect_type(rep2$h, "character")
})

test_that("incompatible shard types raise a vctrs error", {
  s1 <- distinct_state(tibble(g = "a", uid = 1L), g, uid)
  s2 <- distinct_state(tibble(g = 1L, uid = 2L), g, uid)
  expect_error(
    merge_distinct_state(s1, s2),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("custom levels are honored", {
  rep <- collect_distinct_report(
    merged_state(),
    levels = list("region", character(0))
  )
  expect_equal(sort(unique(rep$.grouping)), c(2L, 3L))
})

test_that("empty levels produce a zero-row typed report", {
  s <- shards()
  st <- distinct_state(s$a, c(region, channel), user_id)
  rep <- collect_distinct_report(st, levels = list())

  expect_equal(nrow(rep), 0L)
  expect_named(rep, c("region", "channel", "n", ".grouping"))
  expect_type(rep$region, "character")
})

test_that("column selection supports names, c(), strings and positions", {
  events <- tibble(
    region = c("a", "a", "b"),
    channel = c("x", "y", "x"),
    uid = c(1L, 1L, 2L)
  )
  ref <- collect_distinct_report(
    distinct_state(events, c(region, channel), uid)
  )

  expect_identical(
    ref,
    collect_distinct_report(
      distinct_state(events, c("region", "channel"), "uid")
    )
  )
  expect_identical(
    ref,
    collect_distinct_report(distinct_state(events, 1:2, 3))
  )
  vars <- c("region", "channel")
  expect_identical(
    ref,
    collect_distinct_report(distinct_state(events, vars, uid))
  )
})

test_that("name repair keeps dimension names and yields count/marker names", {
  events <- tibble(region = c("a", "b"), channel = c("x", "y"), uid = 1:2)
  st <- distinct_state(events, c(region, channel), uid)

  repaired <- collect_distinct_report(
    st,
    count_name = "region",
    .name_repair = "unique"
  )
  expect_named(repaired, c("region", "channel", "region...1", ".grouping"))

  grp <- tibble(.grouping = "a", uid = 1L)
  repaired_grp <- collect_distinct_report(
    distinct_state(grp, .grouping, uid),
    .name_repair = "unique"
  )
  expect_named(repaired_grp, c(".grouping", "n", ".grouping...1"))

  # Without repair, the collision is an error
  expect_snapshot_error(
    collect_distinct_report(st, count_name = "region")
  )
})

test_that("errors get the tibble_error_distinct class", {
  events <- tibble(region = "a", uid = 1L)
  st <- distinct_state(events, region, uid)

  expect_snapshot_error(collect_distinct_report(1L))
  expect_snapshot_error(distinct_state(1:3, region, uid))
  expect_snapshot_error(distinct_state(events, region, c(uid, region)))
  expect_snapshot_error(distinct_state(events, no_such_column, uid))
  expect_snapshot_error(merge_distinct_state("not a state"))
  expect_snapshot_error(
    merge_distinct_state(
      distinct_state(events, "region", uid),
      distinct_state(
        tibble(channel = "x", uid = 1L),
        "channel",
        uid
      )
    )
  )
  expect_snapshot_error(
    collect_distinct_report(st, levels = list("no_such_column"))
  )
})

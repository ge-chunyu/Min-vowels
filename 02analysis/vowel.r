library(tidyverse)
library(ggpubr)
library(readxl)
library(lmerTest)

################################################################################
# %% Set theme
theme_set(theme_bw(base_size = 18))

################################################################################
# %%

path <- "~/Desktop/min-vowel/02analysis/"
figpath <- paste0(path, "figure/")
# %% Read data and data preparation

setwd(paste0(path, "data/xiamen/"))
xiamenformant <- list.files(
  path = paste0(path, "data/xiamen/"),
  pattern = ".txt"
)
xmformant <- xiamenformant |>
  map_df(read.table, sep = "\t", na.strings = "--undefine--")

colnames(xmformant) <- c("file", "vowel", "point", "time", "F1", "F2")
xmformant <- xmformant |>
  mutate(
    across(c(F1, F2), ~ na_if(., "--undefined--")),
    across(c(F1, F2), as.numeric),
    variety = "Xiamen",
    vowel = str_extract(file,
      "[FM]\\d{2}-\\d{2}-\\d{3}-[ptkhs]*(\\w+)\\d{2}",
      group = 1
    ),
    vowel = str_replace(vowel, "O", "ɔ"),
    vowel = as.factor(vowel),
    speaker = str_replace(str_sub(file, 1, 3), "0", ""),
    gender = ifelse(str_sub(file, 1, 1) == "F", "Female", "Male"),
    repetition = str_sub(file, 5, 6),
    type = ifelse(vowel %in% c("i", "a", "u", "o", "e", "ɔ"),
      "monophthong", "diphthong"
    ),
    type = ifelse(vowel %in% c("iau", "uai"),
      "triphthong", type
    )
  )


setwd(paste0(path, "data/cangnan/"))
cangnanformant <- list.files(
  path = paste0(path, "data/cangnan/"),
  pattern = ".txt"
)
cnformant <- cangnanformant |>
  map_df(read.table, sep = "\t", na.strings = "--undefine--")

colnames(cnformant) <- c("file", "vowel", "point", "time", "F1", "F2")
cnformant <- cnformant |>
  mutate(
    across(c(F1, F2), ~ na_if(., "--undefined--")),
    across(c(F1, F2), as.numeric),
    variety = "Cangnan",
    vowel = str_extract(file,
      "[FM]\\d-\\d{2}-\\d{2}-[ptkhs]*(\\w+)\\d",
      group = 1
    ),
    vowel = str_replace(vowel, "O", "ɔ"),
    vowel = str_replace(vowel, "A", "ɐ"),
    vowel = str_replace(vowel, "M", "ɯ"),
    vowel = str_replace(vowel, "E", "ə"),
    vowel = as.factor(vowel),
    speaker = str_sub(file, 1, 2),
    gender = ifelse(str_sub(file, 1, 1) == "F", "Female", "Male"),
    repetition = str_sub(file, 5, 5),
    type = ifelse(vowel %in% c(
      "i", "a", "u", "o", "e", "ɔ",
      "ə", "ɐ", "ɯ"
    ),
    "monophthong", "diphthong"
    ),
    type = ifelse(vowel %in% c("iau", "uai", "ieu"),
      "triphthong", type
    )
  )

formant <- rbind(xmformant, cnformant)
formant |>
  write.table(paste0(path, "data/formant.txt"),
    sep = "\t", quote = FALSE, na = "NA", row.names = FALSE
  )

################################################################################
# %% plot monophthong

mean_mono <- formant |>
  filter(
    type == "monophthong",
    point == "6"
  ) |>
  group_by(gender, vowel, variety) |>
  summarize(
    F1 = mean(F1, na.rm = TRUE),
    F2 = mean(F2, na.rm = TRUE)
  )

formant |>
  filter(type == "monophthong", point == "6") |>
  ggplot(aes(x = F2, y = F1, color = vowel, label = vowel)) +
  stat_ellipse(geom = "polygon", aes(fill = vowel), alpha = 0.1) +
  geom_text(data = mean_mono, size = 6, col = "black") +
  scale_x_reverse() +
  scale_y_reverse() +
  facet_grid(variety ~ gender) +
  theme(legend.position = "none")
ggsave(paste0(path, "figure/monophthong.png"), width = 8, height = 8)

################################################################################
# %%

tfor <- formant |>
  filter(
    case_when(
      type == "monophthong" ~ point == "5",
      type == "diphthong" ~ point == "target"
    )
  ) |>
  group_by(file) |>
  mutate(count = row_number()) |>
  ungroup() |>
  mutate(
    element = ifelse(count == 1 & type == "diphthong", "onset", "offset"),
    element = ifelse(type == "monophthong", "monophthong", element),
    target = ifelse(count == 1 & type == "diphthong",
      paste0(str_sub(vowel, 1, 1), "(", str_sub(vowel, 2, 2), ")"),
      paste0("(", str_sub(vowel, 1, 1), ")", str_sub(vowel, 2, 2))
    ),
    target = ifelse(type == "monophthong", as.character(vowel), target)
  )

tfor |>
  write.table(paste0(path, "data/target-formant.txt"),
    sep = "\t", quote = FALSE, na = "NA", row.names = FALSE
  )


t_mean <- tfor |>
  group_by(gender, vowel, target, variety) |>
  summarize(
    F1 = mean(F1, na.rm = TRUE),
    F2 = mean(F2, na.rm = TRUE)
  )

vowel_sets <- list(
  c("i", "u", "a", "ia", "ua"),
  c("i", "u", "a", "ai", "au"),
  c("i", "u", "a", "ui", "iu"),
  c("i", "e", "u", "a", "ie", "ue"),
  c("i", "o", "ɔ", "u", "a", "ɐ", "io", "iɔ", "uɐ")
)

io <- tfor |>
  filter(vowel %in% vowel_sets[[5]]) |>
  ggplot(aes(x = F2, y = F1, color = target)) +
  stat_ellipse(
    geom = "polygon", aes(fill = element),
    alpha = 0.1, linewidth = 0.5
  ) +
  geom_text(
    data = subset(t_mean, vowel %in% vowel_sets[[5]]),
    size = 6, aes(label = target), col = "black"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_fill_grey(start = 0.6, end = 0.1) +
  facet_grid(variety ~ gender) +
  labs(title = "/iɔ uɐ/ in Cangnan and /io/ in Xiamen") +
  theme(legend.position = "none")
ia <- tfor |>
  filter(vowel %in% vowel_sets[[1]]) |>
  ggplot(aes(x = F2, y = F1, color = target)) +
  stat_ellipse(
    geom = "polygon", aes(fill = element),
    alpha = 0.1, linewidth = 0.5
  ) +
  geom_text(
    data = subset(t_mean, vowel %in% vowel_sets[[1]]),
    size = 6, aes(label = target), col = "black"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_fill_grey(start = 0.6, end = 0.1) +
  facet_grid(variety ~ gender) +
  labs(title = "/ia ua/ in Cangnan and Xiamen") +
  theme(legend.position = "none")
iu <- tfor |>
  filter(vowel %in% vowel_sets[[3]]) |>
  ggplot(aes(x = F2, y = F1, color = target)) +
  stat_ellipse(
    geom = "polygon", aes(fill = element),
    alpha = 0.1, linewidth = 0.5
  ) +
  geom_text(
    data = subset(t_mean, vowel %in% vowel_sets[[3]]),
    size = 6, aes(label = target), col = "black"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_fill_grey(start = 0.6, end = 0.1) +
  facet_grid(variety ~ gender) +
  labs(title = "/iu ui/ in Cangnan and Xiamen") +
  # facet_wrap(~gender) +
  theme(legend.position = "none")
ie <- tfor |>
  filter(vowel %in% vowel_sets[[4]]) |>
  ggplot(aes(x = F2, y = F1, color = target)) +
  stat_ellipse(
    geom = "polygon", aes(fill = element),
    alpha = 0.1, linewidth = 0.5
  ) +
  geom_text(
    data = subset(t_mean, vowel %in% vowel_sets[[4]]),
    size = 6, aes(label = target), col = "black"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_fill_grey(start = 0.6, end = 0.1) +
  facet_grid(variety ~ gender) +
  labs(title = "/ie ue/ in Cangnan and /ue/ in Xiamen") +
  theme(legend.position = "none")

ggarrange(iu, io, ia, ie,
  labels = c("A", "B", "C", "D"),
  nrow = 2, ncol = 2
)

ggsave(paste0(figpath, "rising.png"), width = 14, height = 14)

for (vowel_set in vowel_sets) {
  tfor |>
    filter(vowel %in% vowel_set) |>
    ggplot(aes(x = F2, y = F1, color = target)) +
    stat_ellipse(
      geom = "polygon", aes(fill = element),
      alpha = 0.1, linewidth = 0.5
    ) +
    geom_text(
      data = subset(t_mean, vowel %in% vowel_set),
      size = 6, aes(label = target), col = "black"
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    scale_fill_grey(start = 0.6, end = 0.1) +
    facet_grid(variety ~ gender) +
    # facet_wrap(~gender) +
    theme(legend.position = "none")
  ggsave(paste0(figpath, paste0(
    vowel_set[length(vowel_set) - 1], "-",
    vowel_set[length(vowel_set)]
  ), ".png"), width = 8, height = 8)
}

ai_au <- c("i", "a", "u", "ai", "au")
vowel_set <- c("i", "a", "u", "ə", "əu")
ai <- tfor |>
  filter(
    vowel %in% ai_au
  ) |>
  ggplot(aes(x = F2, y = F1, color = target)) +
  stat_ellipse(
    geom = "polygon", aes(fill = element),
    alpha = 0.2, linewidth = 0.5
  ) +
  geom_text(
    data = subset(t_mean, vowel %in% ai_au),
    size = 6, aes(label = target), col = "black"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_fill_grey() +
  facet_grid(variety ~ gender) +
  labs(title = "/ai au/ in Cangnan and Xiamen") +
  theme(legend.position = "none")
eu <- tfor |>
  filter(
    vowel %in% vowel_set,
    variety == "Cangnan"
  ) |>
  ggplot(aes(x = F2, y = F1, color = target)) +
  stat_ellipse(
    geom = "polygon", aes(fill = element),
    alpha = 0.2, linewidth = 0.5
  ) +
  geom_text(
    data = subset(t_mean, vowel %in% vowel_set & variety == "Cangnan"),
    size = 6, aes(label = target), col = "black"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_fill_grey() +
  facet_wrap(~gender) +
  labs(title = "/əu/ in Cangnan") +
  theme(legend.position = "none")
ggarrange(ai, eu,
  labels = c("A", "B"),
  ncol = 2
)
ggsave(paste0(figpath, "falling.png"), width = 15, height = 6)
ggsave(paste0(figpath, "e-ue.png"), width = 8, height = 8)

################################################################################
# %% Plot trajectories
formant |>
  filter(type == "diphthong", !(point %in% c("1", "11", "target"))) |>
  mutate(point = as.numeric(point)) |>
  group_by(gender, vowel, variety, point) |>
  summarize(F1 = median(F1, na.rm = TRUE), F2 = median(F2, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(F2, F1, color = vowel, group = vowel)) +
  geom_path(arrow = arrow(
    ends = "last", type = "closed",
    length = unit(0.1, "inches")
  ), linewidth = 0.6) +
  geom_text(
    data = mean_mono, aes(F2, F1, label = vowel),
    col = "black", size = 8
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  facet_grid(variety ~ gender)
ggsave(paste0(figpath, "diphthong-path.png"), width = 10, height = 8)

################################################################################
# %% Plot formant trajectory
formant |>
  filter(
    point != "target", type != "monophthong",
    # vowel %in% c("iu", "ua", "ue", "ui")
    # vowel %in% c("ai", "au", "iau", "uai")
  ) |>
  # vowel %in% c("ai", "au", "iu", "ua", "ue", "ui", "iau", "uai")) |>
  pivot_longer(c("F1", "F2"), names_to = "formant", values_to = "value") |>
  mutate(
    formant = as.factor(formant), point = as.numeric(point)
  ) |>
  ggplot(aes(point, value)) +
  stat_summary(fun = "mean", geom = "line", aes(col = formant, lty = gender)) +
  stat_summary(
    fun.data = mean_se, geom = "ribbon",
    alpha = 0.2, aes(fill = formant, fill = gender)
  ) +
  facet_grid(variety ~ vowel) +
  theme(legend.position = "bottom")
ggsave(paste0(figpath, "diphthong-trajectory.png"), width = 16, height = 10)

################################################################################
# %% Calculate Pillai score
pillai <- function(...) {
  summary(manova(...))$stats["vowel", "Pillai"]
}
# 1-14: ia ua ue iu ui ai au, diphthongs shared by Xiamen and Cangnan
# 15-16: io, only in Xiamen
# 17-24: ie iɔ uɐ əu, only in Cangnan
target_sets <- list(
  c("i", "i(a)"), c("a", "(i)a"),
  c("u", "u(a)"), c("a", "(u)a"),
  c("u", "u(e)"), c("e", "(u)e"),
  c("i", "i(u)"), c("u", "(i)u"),
  c("u", "u(i)"), c("i", "(u)i"),
  c("i", "(a)i"), c("a", "a(i)"),
  c("u", "(a)u"), c("a", "a(u)"),
  c("i", "i(o)"), c("o", "(i)o"),
  c("i", "i(e)"), c("e", "(i)e"),
  c("i", "i(ɔ)"), c("ɔ", "(i)ɔ"),
  c("u", "u(ɐ)"), c("ɐ", "(u)ɐ"),
  c("ə", "ə(u)"), c("u", "(ə)u")
)
sep <- "-\n"
pillai_score <- tfor |>
  filter(
    target %in% target_sets[[15]]
  ) |>
  group_by(speaker) |>
  summarize(pillai = pillai(cbind(F1, F2) ~ vowel)) |>
  mutate(
    pair = paste(target_sets[[15]], collapse = sep),
    variety = "Xiamen",
    type = "rising"
  )


for (target_set in target_sets[1:14]) {
  pillai_score <- pillai_score |>
    rbind(tfor |>
      filter(
        target %in% target_set
      ) |>
      group_by(speaker, variety) |>
      summarize(pillai = pillai(cbind(F1, F2) ~ vowel)) |>
      mutate(
        pair = paste(target_set, collapse = sep),
        type = ifelse(str_sub(str_replace_all(
          target_set[2],
          "[\\(\\)]", ""
        ), 1, 1) %in% c("i", "u"),
        "rising", "falling"
        )
      ))
}
for (target_set in target_sets[16:24]) {
  pillai_score <- pillai_score |>
    rbind(tfor |>
      filter(
        target %in% target_set
      ) |>
      group_by(speaker) |>
      summarize(pillai = pillai(cbind(F1, F2) ~ vowel)) |>
      mutate(
        pair = paste(target_set, collapse = sep),
        variety = ifelse(str_sub(str_replace_all(
          target_set[2],
          "[\\(\\)]", ""
        ), 2, 2) == "o", "Xiamen", "Cangnan"),
        type = ifelse(str_sub(str_replace_all(
          target_set[2],
          "[\\(\\)]", ""
        ), 1, 1) %in% c("i", "u"),
        "rising", "falling"
        )
      ))
}
table(pillai_score$variety, pillai_score$pair)

pillai_score <- pillai_score |>
  mutate(
    element = ifelse(str_sub(pair, -1) == ")", "onset", "offset"),
    element = factor(element, ordered = TRUE, levels = c("onset", "offset")),
    # type = ifelse(str_replace_all(
    #   str_sub(pair, -4, -1),
    #   "[\\(\\)]", ""
    # ) %in% c("iu", "ui"), "level", type)
  )

pillai_score |>
  mutate(pair = str_replace(pair, "\n", "")) |>
  write.table(paste0(path, "data/pillai.txt"),
    sep = "\t",
    quote = FALSE, na = "NA", row.names = FALSE
  )
ps <- read.table(paste0(path, "data/pillai.txt"), sep = "\t", header = TRUE)
ps1 <- ps |>
  mutate(diphthong = str_replace_all(str_sub(pair, 3, 6), "[()]", ""))
ps1 |>
  head(2)
ai_ia_cn <- ps |>
  filter(
    str_sub(pair, 1, 1) == "a",
    variety == "Cangnan"
  )
a_cn0 <- lmer(pillai ~ 1 + (1 | speaker),
  data = subset(
    ps1,
    str_sub(pair, 1, 1) == "a" & variety == "Xiamen"
  )
)
a_cn1 <- lmer(pillai ~ element + (1 | speaker),
  data = subset(
    ps1,
    str_sub(pair, 1, 1) == "a" & variety == "Xiamen"
  )
)
a_cn2 <- lmer(pillai ~ element + diphthong + (1 | speaker),
  data = subset(
    ps1,
    str_sub(pair, 1, 1) == "a" & variety == "Xiamen"
  )
)
anova(a_cn0, a_cn1)
summary(a_cn1)$coefficients

au <- ps1 |>
  filter(
    variety == "Cangnan",
    (diphthong == "au" & element == "offset") |
      (diphthong == "ua" & element == "onset")
  )
au
au0 <- lmer(pillai ~ 1 + (1 | speaker), data = au)
au1 <- lmer(pillai ~ element + (1 | speaker), data = au)
au2 <- lmer(pillai ~ element + diphthong + (1 | speaker), data = au)
anova(au0, au1, au2)

summary(au1)$coefficients

pillai_score |>
  mutate(gender = str_sub(speaker, 1, 1)) |>
  ggplot(aes(pair, pillai, col = type)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.75, lty = "dashed") +
  facet_grid(variety ~ element, scales = "free_x")
ggsave(paste0(figpath, "pillai.png"), width = 12, height = 8)
################################################################################

formant_long <- formant |>
  pivot_longer(c("F1", "F2"), names_to = "fn", values_to = "formant") |>
  filter(point != "target") |>
  mutate(
    fn = as.factor(fn), point = as.numeric(point),
    formant = as.numeric(formant)
  ) |>
  group_by(file, fn) |>
  mutate(deriv = c(0, diff(formant)))

# %%

formant <- read.table("~/Desktop/min-vowel/02analysis/data/xiamen/F01.txt",
  stringsAsFactors = FALSE,
  header = FALSE, sep = "\t", na.strings = "--undefined--",
  fileEncoding = "utf-8"
)
colnames(formant) <- c("file", "vowel", "point", "time", "F1", "F2")
target <- formant |>
  filter(point == "target")
formant <- formant |>
  pivot_longer(c("F1", "F2"), names_to = "fn", values_to = "formant") |>
  filter(point != "target") |>
  mutate(fn = as.factor(fn), point = as.numeric(point)) |>
  group_by(file, fn) |>
  mutate(deriv = c(0, diff(formant)))

formant |>
  filter(point != 11) |>
  ggplot(aes(point, formant)) +
  stat_summary(fun = "mean", geom = "line", aes(col = fn, lty = gender)) +
  stat_summary(
    fun.data = mean_se, geom = "ribbon",
    alpha = 0.2, aes(fill = fn, fill = gender)
  ) +
  facet_wrap(~vowel) +
  theme_bw()

formant |>
  ggplot(aes(point, deriv)) +
  stat_summary(fun = "mean", geom = "line", aes(col = fn, lty = gender)) +
  stat_summary(
    fun.data = mean_se, geom = "ribbon",
    alpha = 0.2, aes(fill = fn, fill = gender)
  ) +
  facet_wrap(~vowel) +
  theme_bw()

formant |>
  filter(vowel == "ue") |>
  ggplot(aes(point, formant)) +
  stat_summary(fun = "mean", geom = "line", aes(col = fn)) +
  stat_summary(
    fun.data = mean_se, geom = "ribbon",
    alpha = 0.2, aes(fill = fn)
  ) +
  facet_wrap(~speaker, nrow = 2) +
  theme_bw()

formant |>
  filter(vowel == "uai") |>
  ggplot(aes(point, deriv)) +
  stat_summary(fun = "mean", geom = "line", aes(col = fn, lty = gender)) +
  stat_summary(
    fun.data = mean_se, geom = "ribbon",
    alpha = 0.2, aes(fill = fn, fill = gender)
  ) +
  facet_wrap(~speaker, nrow = 2) +
  theme_bw()


################################################################################
# %%

drawAllVowels <- function(data) {
  ggplot(data, aes(point, formant)) +
    stat_summary(fun = "mean", geom = "line", aes(col = fn)) +
    stat_summary(
      fun.data = mean_se, geom = "ribbon",
      alpha = 0.2, aes(fill = fn)
    ) +
    facet_wrap(~vowel) +
    theme_bw()
}

drawSinglefile <- function(data) {
  ggplot(data, aes(point, formant, col = fn)) +
    geom_line() +
    lims(y = c(100, 3500)) +
    facet_wrap(~file) +
    theme_bw()
}
formant |>
  filter(vowel == "a") |>
  drawSinglefile()
# %%

cnpath <- "~/Desktop/min-vowel/02analysis/data/cangnan/"
cnfiles <- list.files(cnpath,
  pattern = ".txt"
)
for (file in cnfiles) {
  formant <- read.table(paste0(cnpath, file),
    stringsAsFactors = FALSE,
    header = FALSE, sep = "\t", na.strings = "--undefined--",
    fileEncoding = "utf-8"
  )
  colnames(formant) <- c("file", "vowel", "point", "time", "F1", "F2")
  formant <- formant |>
    # mutate(
    #   vowel = str_extract(file,
    #     "[FM]\\d-\\d{2}-\\d{2}-[ptkhs]*(\\w+)\\d",
    #     group = 1
    #   ),
    #   vowel = str_replace(vowel, "O", "ɔ"),
    #   vowel = str_replace(vowel, "A", "ɐ"),
    #   vowel = str_replace(vowel, "M", "ɯ"),
    #   vowel = str_replace(vowel, "E", "ə"),
    #   vowel = as.factor(vowel)
    # ) |>
    pivot_longer(c("F1", "F2"), names_to = "fn", values_to = "formant") |>
    filter(point != "target") |>
    mutate(fn = as.factor(fn), point = as.numeric(point)) |>
    group_by(file, fn) |>
    mutate(deriv = c(0, diff(formant)))

  formant |>
    drawAllVowels()
  ggsave(paste0(cnpath, "figs/", str_sub(file, 1, 3), ".jpg"), width = 10, height = 8)
  vowels <- unique(formant$vowel)
  for (v in vowels) {
    formant |>
      filter(vowel == v) |>
      drawSinglefile()
    ggsave(paste0(cnpath, "figs/", str_sub(file, 1, 3), "-", v, ".jpg"), width = 10, height = 8)
  }
}

################################################################################
###
age <- data.frame(
  male = 2015 - c(1952, 1941, 1961, 1958, 1951),
  female = 2015 - c(1959, 1988, 1952, 1962, 1964)
)
age |>
  pivot_longer(c("male", "female"), names_to = "gender", values_to = "age") |>
  group_by(gender) |>
  summarize(mean = mean(age), st = sd(age))

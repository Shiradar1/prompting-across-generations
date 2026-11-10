###############################################################
# Prompting Across Generations - analysis code
# Reproduces all results reported in the manuscript from the
# de-identified data files in this folder.
#
# Data files (greeting and trip tasks only):
#   participants.csv  - one row per participant (n = 90)
#     participant, age_group (younger / older), completion_minutes
#   interactions.csv  - one row per participant x task (n = 180)
#     conversation_length (number of messages), total_prompt_words,
#     total_gpt_words, mean_gpt_response_time (seconds),
#     relevance, usefulness, clarity, sufficiency (self-evaluations, 1-5)
#   prompts.csv       - one row per analyzed prompt (n = 870;
#                       prompts with fewer than three words excluded)
#     message_order, conversation_length, prompt_words, response_words,
#     prompt_complexity (PrediQT), prompt_dsi, response_dsi (DSI)
#
# Set the working directory to this folder before running.
###############################################################
packages <- c("dplyr", "ggplot2", "lme4", "lmerTest", "performance", "emmeans",
              "effectsize", "ggeffects", "afex", "ordinal", "psych")
invisible(lapply(packages, library, character.only = TRUE))

# Figure colors
task_colors <- c(greeting = "#2a78d6", trip = "#eb6834")
var_colors  <- c(complexity = "#008300", creativity = "#c98500", message_order = "#d55181",
                 conversation_length = "#0b8ca6", word_count = "#b8860b")
task_labels <- c(greeting = "Greeting", trip = "Trip")

# Reference levels: greeting task and older adults
participants <- read.csv("participants.csv") %>%
  mutate(age_group = factor(age_group, levels = c("older", "younger")))
interactions <- read.csv("interactions.csv") %>%
  left_join(participants, by = "participant") %>%
  mutate(participant = factor(participant), task = factor(task, levels = c("greeting", "trip")))
prompts <- read.csv("prompts.csv") %>%
  left_join(participants, by = "participant") %>%
  mutate(participant = factor(participant), task = factor(task, levels = c("greeting", "trip")))

z_values <- seq(-2, 2, by = 0.1)


###############################################################
# 1. Experiment completion time by age group (Section 2.1)
###############################################################

participants %>%
  group_by(age_group) %>%
  summarise(M = mean(completion_minutes), SD = sd(completion_minutes),
            min = min(completion_minutes), max = max(completion_minutes), .groups = "drop")

t.test(completion_minutes ~ age_group, data = participants, var.equal = TRUE)
effectsize::cohens_d(completion_minutes ~ age_group, data = participants)


###############################################################
# 2. Surface-level and interaction measures (Table 1, Appendix D)
# 2 x 2 mixed ANOVAs: age group (between) x task (within)
###############################################################

surface_measures <- c("conversation_length", "total_prompt_words",
                      "total_gpt_words", "mean_gpt_response_time")

aov_surface <- lapply(surface_measures, function(dv)
  aov_ez(id = "participant", dv = dv, data = interactions,
         between = "age_group", within = "task", type = 3))
names(aov_surface) <- surface_measures

for (dv in surface_measures) {
  cat("\n=====", dv, "=====\n")
  print(anova(aov_surface[[dv]]))
  print(eta_squared(aov_surface[[dv]], partial = TRUE))
  print(emmeans(aov_surface[[dv]], ~ age_group))
  print(emmeans(aov_surface[[dv]], ~ task))
}

# Total GPT response length: follow-up of the age group x task interaction
# Age-group difference within each task, Holm correction across both tasks
emm_gpt_words_age_by_task <- emmeans(aov_surface$total_gpt_words, ~ age_group | task)
emm_gpt_words_age_by_task
test(pairs(emm_gpt_words_age_by_task), by = NULL, adjust = "holm")
# Task difference within each age group
pairs(emmeans(aov_surface$total_gpt_words, ~ task | age_group))


###############################################################
# 3. Prompt complexity (Section 3.2.1, Table E1, Figures 1-2)
###############################################################

null_complexity <- lmer(prompt_complexity ~ 1 + (1 | participant) + (1 | participant:task),
                        data = prompts, REML = FALSE)
word_complexity <- lmer(prompt_complexity ~ prompt_words + (1 | participant) + (1 | participant:task),
                        data = prompts, REML = FALSE)
full_complexity_ml <- lmer(prompt_complexity ~ task * age_group + prompt_words + message_order +
                             conversation_length + (1 | participant) + (1 | participant:task),
                           data = prompts, REML = FALSE)

anova(null_complexity, word_complexity, full_complexity_ml)   # null vs word count vs full
anova(null_complexity, full_complexity_ml)                    # null vs full

model_complexity <- update(full_complexity_ml, REML = TRUE)
summary(model_complexity)                  # Table E1 (b)
anova(model_complexity)                    # F tests (Satterthwaite)
r2(model_complexity)
standardize_parameters(model_complexity)   # Table E1 (beta)

emm_task_complexity <- emmeans(model_complexity, ~ task, lmer.df = "satterthwaite")
emm_task_complexity
confint(pairs(emm_task_complexity))
pairs(emm_task_complexity)

# Figure 1: observed distributions and model-estimated means by task
fig1 <- ggplot(prompts, aes(x = task, y = prompt_complexity, fill = task)) +
  geom_violin(alpha = 0.35, trim = FALSE, color = NA) +
  geom_boxplot(width = 0.10, outlier.shape = NA, alpha = 0.7, color = "gray25") +
  geom_point(data = as.data.frame(emm_task_complexity), aes(x = task, y = emmean),
             inherit.aes = FALSE, size = 3.8, color = "black") +
  geom_errorbar(data = as.data.frame(emm_task_complexity),
                aes(x = task, ymin = lower.CL, ymax = upper.CL),
                inherit.aes = FALSE, width = 0.10, linewidth = 0.9, color = "black") +
  scale_fill_manual(values = task_colors) +
  scale_x_discrete(labels = task_labels) +
  labs(x = "Task", y = "Prompt complexity") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", axis.text.x = element_text(size = 12), axis.title = element_text(size = 14))
fig1

# Figure 2: predicted complexity across message order (averaged over task and age group)
pred_order <- as.data.frame(predict_response(model_complexity, terms = "message_order",
                                             margin = "marginalmeans"))
fig2 <- ggplot(pred_order, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = var_colors[["message_order"]], alpha = 0.25) +
  geom_line(color = var_colors[["message_order"]], linewidth = 1.2) +
  labs(x = "Message order", y = "Predicted prompt complexity") +
  theme_classic(base_size = 14) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
fig2


###############################################################
# 4. Prompt semantic creativity (Section 3.2.2, Table E2, Figure 3)
###############################################################

null_creativity <- lmer(prompt_dsi ~ 1 + (1 | participant) + (1 | participant:task),
                        data = prompts, REML = FALSE)
word_creativity <- lmer(prompt_dsi ~ prompt_words + (1 | participant) + (1 | participant:task),
                        data = prompts, REML = FALSE)
full_creativity_ml <- lmer(prompt_dsi ~ task * age_group + prompt_words + message_order +
                             conversation_length + (1 | participant) + (1 | participant:task),
                           data = prompts, REML = FALSE)

anova(null_creativity, word_creativity, full_creativity_ml)
anova(null_creativity, full_creativity_ml)

model_creativity <- update(full_creativity_ml, REML = TRUE)
summary(model_creativity)                  # Table E2 (b)
anova(model_creativity)
r2(model_creativity)
standardize_parameters(model_creativity)   # Table E2 (beta)

emm_age_creativity <- emmeans(model_creativity, ~ age_group, lmer.df = "satterthwaite")
emm_age_creativity
confint(pairs(emm_age_creativity))
pairs(emm_age_creativity)

# Predictions for continuous predictors on a z-score scale (display only).
# Each predictor ranges from -2 to +2 SD; the other continuous predictors are held
# at their means; predictions are averaged over task and age group.
predict_z <- function(model, outcome, predictors, continuous) {
  y_mean <- mean(prompts[[outcome]]); y_sd <- sd(prompts[[outcome]])
  bind_rows(lapply(names(predictors), function(var) {
    m <- mean(prompts[[var]]); s <- sd(prompts[[var]])
    others <- setdiff(continuous, var)
    pred <- predict_response(model,
                             terms = paste0(var, " [", paste(m + s * z_values, collapse = ","), "]"),
                             condition = sapply(others, function(v) mean(prompts[[v]])),
                             margin = "marginalmeans")
    as.data.frame(pred) %>%
      mutate(x = (x - m) / s,
             predicted = (predicted - y_mean) / y_sd,
             conf.low  = (conf.low  - y_mean) / y_sd,
             conf.high = (conf.high - y_mean) / y_sd,
             predictor = predictors[[var]])
  })) %>% mutate(predictor = factor(predictor, levels = predictors))
}

# Figure 3
fig3_predictors <- c(prompt_words = "Word count", message_order = "Message order",
                     conversation_length = "Conversation length")
fig3_colors <- c("Word count" = var_colors[["word_count"]], "Message order" = var_colors[["message_order"]],
                 "Conversation length" = var_colors[["conversation_length"]])
pred_fig3 <- predict_z(model_creativity, "prompt_dsi", fig3_predictors,
                       c("prompt_words", "message_order", "conversation_length"))

fig3 <- ggplot(pred_fig3, aes(x = x, y = predicted, color = predictor, fill = predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.18, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = fig3_colors) +
  scale_fill_manual(values = fig3_colors) +
  labs(x = "Standardized predictor value (z)", y = "Predicted prompt semantic creativity (z)",
       color = "Predictor", fill = "Predictor") +
  theme_classic(base_size = 14) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.title = element_text(size = 13), legend.text = element_text(size = 12),
        legend.position = "inside", legend.position.inside = c(0.03, 0.97),
        legend.justification = c(0, 1), legend.background = element_blank())
fig3


###############################################################
# 5. GPT response semantic creativity (Section 3.3.1, Tables E3-E4, Figure 4)
# Random intercept for participant x task only (the participant-level
# variance was estimated at zero)
###############################################################

null_gpt <- lmer(response_dsi ~ 1 + (1 | participant:task), data = prompts, REML = FALSE)
control_gpt <- lmer(response_dsi ~ age_group + task + prompt_words + response_words + message_order +
                      conversation_length + (1 | participant:task), data = prompts, REML = FALSE)
full_gpt_ml <- lmer(response_dsi ~ prompt_complexity + prompt_dsi + age_group + task + prompt_words +
                      response_words + message_order + conversation_length + (1 | participant:task),
                    data = prompts, REML = FALSE)

anova(null_gpt, control_gpt, full_gpt_ml)   # null vs control vs full

model_gpt <- update(full_gpt_ml, REML = TRUE)
summary(model_gpt)                  # Table E3 (b)
anova(model_gpt)
r2(model_gpt)
standardize_parameters(model_gpt)   # Table E3 (beta)
confint(model_gpt, parm = c("prompt_dsi", "prompt_complexity"), method = "Wald")

# Check: participant-level random intercept is estimated at zero
model_gpt_both_re <- update(model_gpt, . ~ . + (1 | participant))
VarCorr(model_gpt_both_re)
isSingular(model_gpt_both_re)

# Within / between participant-task decomposition (Table E4)
prompts_decomposed <- prompts %>%
  group_by(participant, task) %>%
  mutate(dsi_pt_mean = mean(prompt_dsi), cx_pt_mean = mean(prompt_complexity)) %>%
  ungroup() %>%
  mutate(dsi_within  = prompt_dsi - dsi_pt_mean,
         dsi_between = dsi_pt_mean - mean(prompt_dsi),
         cx_within   = prompt_complexity - cx_pt_mean,
         cx_between  = cx_pt_mean - mean(prompt_complexity))

decomposed_gpt_ml <- lmer(response_dsi ~ cx_within + cx_between + dsi_within + dsi_between +
                            age_group + task + prompt_words + response_words + message_order +
                            conversation_length + (1 | participant:task),
                          data = prompts_decomposed, REML = FALSE)
anova(update(full_gpt_ml, data = prompts_decomposed), decomposed_gpt_ml)   # combined vs decomposed

model_gpt_decomposed <- update(decomposed_gpt_ml, REML = TRUE)
summary(model_gpt_decomposed)                  # Table E4 (b)
standardize_parameters(model_gpt_decomposed)   # Table E4 (beta)

# Figure 4 (prompt complexity, not significant, drawn semi-transparent)
fig4_predictors <- c(prompt_dsi = "Prompt semantic creativity", prompt_complexity = "Prompt complexity")
fig4_colors <- c("Prompt semantic creativity" = var_colors[["creativity"]],
                 "Prompt complexity" = var_colors[["complexity"]])
pred_fig4 <- predict_z(model_gpt, "response_dsi", fig4_predictors,
                       c("prompt_dsi", "prompt_complexity", "prompt_words", "response_words",
                         "message_order", "conversation_length")) %>%
  mutate(line_alpha   = ifelse(predictor == "Prompt complexity", 0.35, 1),
         ribbon_alpha = ifelse(predictor == "Prompt complexity", 0.07, 0.20))

fig4 <- ggplot(pred_fig4, aes(x = x, y = predicted, color = predictor, fill = predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, alpha = I(ribbon_alpha)), color = NA) +
  geom_line(aes(alpha = I(line_alpha)), linewidth = 1.2) +
  scale_color_manual(values = fig4_colors) +
  scale_fill_manual(values = fig4_colors) +
  guides(color = guide_legend(override.aes = list(alpha = c(1, 0.35), fill = NA)), fill = "none") +
  labs(x = "Standardized predictor value (z)", y = "Predicted GPT response semantic creativity (z)",
       color = "Predictor", fill = "Predictor") +
  theme_classic(base_size = 14) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.title = element_text(size = 13), legend.text = element_text(size = 12),
        legend.position = "inside", legend.position.inside = c(0.03, 0.97),
        legend.justification = c(0, 1), legend.background = element_blank())
fig4


###############################################################
# 6. Self-evaluation of GPT response quality (Section 3.3.2, Table 2, Appendix F)
###############################################################

eval_dimensions <- c("relevance", "usefulness", "clarity", "sufficiency")

aov_self <- lapply(eval_dimensions, function(dv)
  aov_ez(id = "participant", dv = dv, data = interactions,
         between = "age_group", within = "task", type = 3))
names(aov_self) <- eval_dimensions

for (dv in eval_dimensions) {
  cat("\n=====", dv, "=====\n")
  print(anova(aov_self[[dv]]))
  print(eta_squared(aov_self[[dv]], partial = TRUE))
  print(emmeans(aov_self[[dv]], ~ age_group))
  print(emmeans(aov_self[[dv]], ~ task))
}

# Relevance: follow-up of the age group x task interaction
# Task difference within each age group, Holm correction across both groups
emm_relevance <- emmeans(aov_self$relevance, ~ task | age_group)
emm_relevance
test(pairs(emm_relevance), by = NULL, adjust = "holm")
confint(pairs(emm_relevance), by = NULL, adjust = "none")

# Clarity and sufficiency: main-effect contrasts
confint(pairs(emmeans(aov_self$clarity, ~ age_group)))
confint(pairs(emmeans(aov_self$clarity, ~ task)))
confint(pairs(emmeans(aov_self$sufficiency, ~ task)))

# Robustness: cumulative-link mixed models (ratings as ordinal)
lrt <- function(reduced, full) {
  chisq <- 2 * (as.numeric(logLik(full)) - as.numeric(logLik(reduced)))
  df <- attr(logLik(full), "df") - attr(logLik(reduced), "df")
  data.frame(Chisq = chisq, df = df, p = pchisq(chisq, df, lower.tail = FALSE))
}
clmm_tests <- function(dv) {
  dat <- interactions %>% mutate(rating = ordered(.data[[dv]]))
  fit <- function(rhs) clmm(as.formula(paste("rating ~", rhs, "+ (1 | participant)")),
                            data = dat, link = "logit", Hess = TRUE)
  m_full <- fit("age_group * task"); m_add <- fit("age_group + task")
  rbind("Age group x task" = lrt(m_add, m_full),
        "Age group"        = lrt(fit("task"), m_add),
        "Task"             = lrt(fit("age_group"), m_add))
}
sapply(eval_dimensions, clmm_tests, simplify = FALSE)

# Internal consistency of the four dimensions
eval_items <- interactions[, eval_dimensions]
psych::alpha(eval_items)
psych::omega(eval_items, nfactors = 1)
cor(eval_items)

# Overall self-evaluation score and mean prompt / response measures per interaction
self_eval_data <- interactions %>%
  mutate(self_eval_overall = rowMeans(across(all_of(eval_dimensions)))) %>%
  select(participant, task, age_group, self_eval_overall) %>%
  inner_join(prompts %>%
               group_by(participant, task) %>%
               summarise(mean_prompt_complexity = mean(prompt_complexity),
                         mean_prompt_creativity = mean(prompt_dsi),
                         mean_gpt_creativity    = mean(response_dsi),
                         conversation_length    = first(conversation_length),
                         .groups = "drop"),
             by = c("participant", "task")) %>%
  mutate(mean_prompt_complexity_c = mean_prompt_complexity - mean(mean_prompt_complexity))

null_self <- lmer(self_eval_overall ~ 1 + (1 | participant), data = self_eval_data, REML = FALSE)
full_self_ml <- lmer(self_eval_overall ~ mean_prompt_complexity_c + mean_prompt_creativity +
                       mean_gpt_creativity + conversation_length + task * age_group + (1 | participant),
                     data = self_eval_data, REML = FALSE)
anova(null_self, full_self_ml)

model_self <- update(full_self_ml, REML = TRUE)
summary(model_self)
anova(model_self)
r2(model_self)
confint(model_self, parm = "mean_gpt_creativity", method = "Wald")

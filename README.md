# Prompting Across Generations: Question Complexity, Creativity, and Perceived Response Quality in Human-LLM Interaction

Data and analysis code for the article *Prompting Across Generations: Question Complexity, Creativity, and Perceived Response Quality in Human-LLM Interaction* (Darchi, Pinter, & Sadeh).

The repository contains a de-identified dataset, the R code that reproduces all results reported in the article, and the study preregistrations.

## Contents

| File | Description |
|---|---|
| `participants.csv` | One row per participant (n = 90) |
| `interactions.csv` | One row per participant × task (n = 180) |
| `prompts.csv` | One row per analyzed prompt (n = 870) |
| `analysis.R` | Reproduces all analyses, tables, and figures reported in the article |
| `preregistration_246249.pdf` | Original preregistration (AsPredicted #246249) |
| `preregistration_300809.pdf` | Supplementary preregistration (AsPredicted #300809) |

## Data

All files include only the two experimental tasks (greeting and trip). Participant identifiers were replaced with anonymous codes (P001–P090). Conversation transcripts and other potentially identifying information (Prolific IDs, free-text responses, timestamps, age, and sex) are not included.

**participants.csv**

| Variable | Description |
|---|---|
| `participant` | Anonymous participant code |
| `age_group` | `younger` or `older` |
| `completion_minutes` | Total experiment completion time (minutes) |

**interactions.csv**

| Variable | Description |
|---|---|
| `participant`, `task` | Participant code; task (`greeting` or `trip`) |
| `conversation_length` | Number of messages in the interaction |
| `total_prompt_words` | Total number of words in the participant's prompts |
| `total_gpt_words` | Total number of words in GPT's responses |
| `mean_gpt_response_time` | Mean GPT response time (seconds) |
| `relevance`, `usefulness`, `clarity`, `sufficiency` | Participant self-evaluations of GPT's responses (1–5) |

**prompts.csv**

Prompts with fewer than three words were excluded (n = 29).

| Variable | Description |
|---|---|
| `participant`, `task` | Participant code; task (`greeting` or `trip`) |
| `message_order` | Position of the prompt within the interaction |
| `conversation_length` | Number of messages in the interaction |
| `prompt_words` | Number of words in the prompt |
| `response_words` | Number of words in GPT's response |
| `prompt_complexity` | Prompt complexity (PrediQT; Bloom scale 1–6) |
| `prompt_dsi` | Prompt semantic creativity (DSI) |
| `response_dsi` | GPT response semantic creativity (DSI) |

Complexity and DSI scores were computed in advance (see the Method section of the article); the scoring code is not included.

## Running the analyses

1. Install R (tested with R 4.4) and the required packages:

   ```r
   install.packages(c("dplyr", "ggplot2", "lme4", "lmerTest", "performance", "emmeans",
                      "effectsize", "ggeffects", "afex", "ordinal", "psych"))
   ```

2. Set the working directory to the folder containing the files and run `analysis.R`.

The script is organized by the sections of the article:

| Script section | Article |
|---|---|
| 1. Experiment completion time | Section 2.1 |
| 2. Surface-level and interaction measures | Table 1, Appendix D |
| 3. Prompt complexity | Section 3.2.1, Table E1, Figures 1–2 |
| 4. Prompt semantic creativity | Section 3.2.2, Table E2, Figure 3 |
| 5. GPT response semantic creativity | Section 3.3.1, Tables E3–E4, Figure 4 |
| 6. Self-evaluation of GPT response quality | Section 3.3.2, Table 2, Appendix F |

## Preregistrations

- Original preregistration: AsPredicted #246249 (https://aspredicted.org/9r2u2c.pdf)
- Supplementary preregistration: AsPredicted #300809 (https://aspredicted.org/ic6py9.pdf)

## Citation

If you use these materials, please cite the article:

Darchi, S., Pinter, Y., & Sadeh, T. (in press). Prompting across generations: Question complexity, creativity, and perceived response quality in human-LLM interaction. *Journal of Intelligence*.

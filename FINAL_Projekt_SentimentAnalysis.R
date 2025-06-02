# 1. Wczytujemy potrzebne biblioteki
library(tidyverse)
library(SentimentAnalysis)
library(tidytext)
library(textdata)
library(ggplot2)
library(ggthemes)
library(scales)
library(stringr)

# 2. Wybieramy pliki dla kolejnych postaci
chandler_path <- file.choose()
rachel_path  <- file.choose()
monica_path  <- file.choose()

# 3. Tworzymy listę słów do usunięcia
custom_stops <- c(
  "i'm", "i've", "i'd", "i'll",
  "you're", "they're", "we're",
  "it's", 
  "would've", 
  "ve", "re", "ll", "d", "s",
  "yeah", "uh", "huh", "oh", "um",
  "gonna", "wanna", "gotta",
  "ya", "yknow", "okay", "ok", "hey",
  "hmm", "ah", "ha", "whoa",
  "right", "like", "just",
  "kinda",
  "i", "did", "a", "an", "it", "of"
)
stop_pattern <- regex(paste0("\\b(", paste(custom_stops, collapse="|"), ")\\b"),
                      ignore_case = TRUE)

# 4. Tworzymy funkcję do wczytania i oczyszczenia dialogów z pliku
clean_text <- function(path) {
  raw <- readLines(path, encoding = "UTF-8")
  # ujednolicenie apostrofów i wyciągnięcie po tabulatorze
  raw <- str_replace_all(raw, "’|‘|`", "'")
  lines <- str_extract(raw, "(?<=\\t).*")
  lines <- lines[!is.na(lines) & nzchar(lines)]
  # usuwanie stop-słów, interpunkcji, zmiana na małe litery
  lines %>%
    str_remove_all(stop_pattern) %>%
    str_remove_all("[[:punct:]]") %>%
    str_to_lower() %>%
    str_squish()
}

# 5. Wczytujemy oraz czyścimy nasze dialogi
cleaned <- list(
  Chandler = clean_text(chandler_path),
  Rachel  = clean_text(rachel_path),
  Monica  = clean_text(monica_path)
)

# 6. Prowadzimy analizę przez słownik QDAP
df_qdap <- imap_dfr(cleaned, ~{
  sent <- analyzeSentiment(.x)
  dir  <- convertToDirection(sent$SentimentQDAP)
  tibble(character = .y,
         sentence_index = seq_along(dir),
         direction = dir)
})

# 6. Generujemy wykres rozkładu sentymentu dla kolejnych postaci dla słownika QDAP
ggplot(df_qdap, aes(x = direction, fill = direction)) +
  geom_bar(alpha = 0.8) +
             facet_wrap(~ character) +
             labs(
               title = "Sentyment QDAP z podziałem na postacie",
               x = "Sentyment",
               y = "Liczba zdań"
             ) +
             theme_bw() +
             theme(legend.position = "none")
           
# 7. Prowadzimy analizę przez słownik Bing
           bing_counts <- imap_dfr(cleaned, ~{
             tibble(text = .x) %>%
               unnest_tokens(word, text) %>%
               # odfiltrowanie tych samych stop- słów
               filter(!word %in% custom_stops) %>%
               inner_join(get_sentiments("bing"), by = "word") %>%
               count(character = .y, sentiment)
           })
           
# 8. Generujemy wykres rozkładu sentymentu positive/negative dla słownika Bing
           ggplot(bing_counts, aes(x = character, y = n, fill = sentiment)) +
             geom_col(position = "dodge") +
             labs(
               title = "Sentyment Bing (Positive, Negative) z podziałem na postacie",
               x = "Postać",
               y = "Liczba słów"
             ) +
             theme_minimal()
           
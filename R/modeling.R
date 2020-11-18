#' The following script contains functions related to generating
#' models with the provided training dataset.
#

# Library ---------------------------------------------------------------------
if (!require(pacman)) {install.packages('pacman')}
p_load(
  caTools,
  dplyr,
  quanteda,
  stringr,
  textstem,
  tidyr,
  tidytext
)


# Functions -------------------------------------------------------------------

#' Trim Strings
#' The following function trims they hypothesis strings. The trim methodology is
#' consistent with the original python package. This function identifies
#' where two terms are in each string, node1 and node2, although the function
#' is written in a general form in case these terms change. The function then
#' concatenates all tokens into a single token which occur after the first
#' node2 instance which occurs after a node1 instance. If this condition does
#' not exist, no changes are made.
#'
#

trim_strings <- function(input_string, key_1="node1", key_2="node2"){

  # Convert String into Word Tokens
  tokens <-  str_split(input_string, pattern =  " ") %>% unlist()

  # Determine All Indices of Key 1 and Key 2
  index_k1_all <- which(tokens %in% key_1)
  index_k2_all <- which(tokens %in% key_2)
  if(length(index_k1_all) == 0 | length(index_k2_all) == 0){
    print("Length Zero")
    print(index_k1_all)
    print(index_k2_all)

  }

  # Verify An Instance of Key 2 exists After Key 1
  # If Not, No Action
  if(length(index_k1_all) == 0 | length(index_k2_all) == 0){
    return(input_string)
  }
  else if (max(index_k2_all) < min(index_k1_all)){
    return(input_string)
  }

  # Reduce Key 2 Indices to First Instance After Key 1 Intial Instance
  logical_k2 <- (index_k2_all) > min(index_k1_all)
  index_k2_post_k1 <- min(index_k2_all[logical_k2])

  # Determine String Length
  num_tokens <- length(tokens)

  # Initialize
  output_string <- input_string

  # Trim String After Key 2
  if(num_tokens > index_k2_post_k1){
    # Collapse Tokens
    index_trim_start <- index_k2_post_k1 + 1
    tokens_trim <- str_c(tokens[index_trim_start:num_tokens], collapse = "")

    string_maintain <- str_c(tokens[1:index_k2_post_k1], collapse = " ")

    # Replace Tokens With Collapsed Trim
    output_vec <- c(string_maintain, tokens_trim)
    output_string <- str_c(output_vec, collapse = " ")
  }

  output_string <- unname(output_string)
  return(output_string)
}

# Vectorize Function
trim_strings <- Vectorize(trim_strings)


#' Process Data
#' The following function performs all processing steps the precede
#' vectorization into a DTM.
#'
#' Input (Required):
#' * Dataframe with the following columns:
#'  * sentence
#'  * node_1
#'  * node_2
#'  * file_name
#'  * hypothesis_num
#

process_data <- function(input_df){

  # Missing Values -----------------------------------------------------------------
  ## Drop Rows with Missing Values
  processing_df <- input_df %>%
    drop_na()

  # Normalize -----------------------------------------------------------------
  ## Normalize Sentence and Node Text
  processing_df <- processing_df %>%
    mutate(
      sentence = tolower(sentence),
      node_1 = tolower(node_1),
      node_2 = tolower(node_2)
    )

  # Entity Replacement --------------------------------------------------------
  ## Replace Patterns in Sentence Which Match Node_1 or Node_2 Columns
  ### Remove Punctuation from Node_1 and Node_2

  regex_punct <- "[.!?]"

  processing_df <- processing_df %>%
    mutate(
      node_1 = str_remove_all(node_1, pattern = regex_punct),
      node_2 = str_remove_all(node_2, pattern = regex_punct)
    )

  ## Repalce Node Entities
  processing_df <- processing_df %>%
    mutate(
      sentence = str_replace_all(sentence, pattern = node_1, replacement = "node1"),
      sentence = str_replace_all(sentence, pattern = node_2, replacement = "node2"),
    )

  # Tokenize ------------------------------------------------------------------
  ## Convert Sentence String into Word Tokens
  ## Method Removes Punctuation By Default
  processing_df <- processing_df %>%
    unnest_tokens(word, sentence)


  # Remove Stop Words ---------------------------------------------------------

  data(stop_words)

  processing_df <- processing_df %>%
    anti_join(stop_words, by = "word")


  # Lemmatize Sentence --------------------------------------------------------
  processing_df <- processing_df %>%
    mutate(
      word = lemmatize_words(word)
    )

  # Recombine Tokens into Strings ---------------------------------------------
  processing_df <- processing_df %>%
    group_by(file_name, hypothesis_num) %>%
    mutate(
      sentence = str_c(word, collapse = " ")
    ) %>%
    ungroup() %>%
    select(-word) %>%
    distinct()

  # Trim Strings --------------------------------------------------------------
  processing_df <- processing_df %>%
    mutate(
      sentence = trim_strings(sentence)
    )

  # Generate Unique Identifier ------------------------------------------------
  # Create Unique ID  ---------------------------------------------------------
  processing_df <- processing_df %>%
    mutate(
      hyp_id = str_c(file_name, hypothesis_num, row_number(), sep = "_")
    )

  return(processing_df)

}


#' Generate Document Term Matrix - Bag of Words - NGrams = 3
#' Creats a Document Term Matrix. Uses a Bag-of-Words approach
#' with an n-gram size of 3
#'
#' Input (Required):
#' * Dataframe with the following columns:
#'  * sentence
#'  * node_1
#'  * node_2
#'  * file_name
#'  * hypothesis_num
#

gen_dtm_bow <- function(input_df){

  # Create Corpus -------------------------------------------------------------
  corpus_hypo <- corpus(
    x = input_df,
    text_field = "sentence",
    docid_field = "hyp_id"
  )

  # Generate Tokens------------------------------------------------------------
  tokens_hyp <- quanteda::tokens(corpus_hypo)

  ## Ngram = 3
  tokens_hyp_ngram <- tokens_ngrams(tokens_hyp, n = 3:3)

  # Document Term Matrix ------------------------------------------------------
  dtm_hyp <- dfm(tokens_hyp_ngram)

  ## Convert to Dataframe
  dtm_hyp <- convert(x = dtm_hyp, to = "data.frame")

  return(dtm_hyp)

}


#' Train / Test Split
#' The following function splits any provided dataset into training and test
#' sets.
#'
#' Inputs, Required:
#' * Data:
#'   This input will be a dataframe containing at a minimum, the text data
#' * Column, Text: Column name of the raw text
#'
#' * Inputs, Optional:
#' * train_split_ratio: Percent of data split into training set
#'


train_test_split <- function(input_data,
                             train_split_ratio = 0.75){
  ## Create Temporary Column For Train/Test Split
  input_data_split <- input_data %>%
    mutate(
      splitting_col = row_number()
    )

  ## Split into Training/Test Set
  sample <- sample.split(input_data_split$splitting_col,
                         SplitRatio = train_split_ratio)
  train <- subset(input_data, sample == TRUE)
  test <- subset(input_data, sample == FALSE)

  split_data <- list("train" = train, "test" = test)

  return(split_data)

}

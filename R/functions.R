#' 2023 Color Palette
#' 
#' A color palette for the year 2023. 
#' @export
twenty3pal <- c("#87CEEB", "#9FE2BF", "#FA8072",
                             "#CC8899", "#D3D3D3")

#' Addition
#' 
#' Add together two numbers
#' @param x A number.
#' @param y A number.
#' @returns A numeric vector.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}

#' Drop Empty
#' 
#' Drop empty rows from a dataframe
#' @param x A dataframe, matrix, or vector.
#' @returns The same object without any empty rows
#' @export
#' 
dropMT <- function(x){

    if(class(x) != "data.frame" &
       class(x) != "character" &
       class(x) != "matrix"){
       stop("object is not a dataframe, matrix or vector")
    }

    if(class(x) == "character"){
        x <- x[x != ""]
        return(x)
    }

    if(class(x) == "data.frame" | class(x) == "matrix"){
    x <- x[!apply(x == "", 1, all), ]
    return(x)
    }
}

#' Count the number of unique elements in a list or vector
#' @param x A list or vector
#' @returns the number of unique elements in the vector or list
#' @export
#' 
#' @examples
#' x 
howmuch <- function(x){
  # count the unique values in a vector or list
  n <- length(unique(x))
  return(n)
}

#' Calculate norm of vector
#' 
#' @export
#' 
norm_vec <- function(x) {
    sqrt(sum(x^2))
}

#Normalize vector#
nrm <- function(x) {
    x / norm_vec(x)
}

#' Calculate semantic dimension from antonym pair
#'
#' @export
dimension <- function(x, y) {
    nrm(nrm(x) - nrm(y))
}

#' Dot product
#' 
#' @export
#' 
dot <- function(x, y){
    sum(x * y)
}

#' Cosine Similarity
#' 
#' @export
#' 
#' 
cos <- function(x, y) {
    dot(x, y) / norm_vec(x) / norm_vec(y)
}

#' Make first column of matrix the rownames and drop
#' 
#' @param embedding A dataframe.
#' @return The same dataframe with the first row as the row.names
#' @export
#' 
#' @examples
#' embedding <- assign_rownames(embedding)
assign_rownames <- function(embedding) {

    embedding <- embedding[!is.na(embedding$V1),]

    embedding <- embedding[!duplicated(embedding$V1),]

    row.names(embedding) <- embedding[, 1]

    embedding <- embedding[, 2:ncol(embedding)]

    embedding <- embedding[,colSums(is.na(embedding))<nrow(embedding)]

    return(embedding)
}

#' Load word embedding 
#' 
#' Function for loading a word embedding
#' @param path A valid string path to the word embedding file
#' @param source indicating the source of the embedding file either, 
#'      "Google", "GloVe", or "fastText"
#' @export
#' 
load_embedding <- function(path, source = c("Google", "GloVe", "fastText")) {
    source <- match.arg(source)

    if (source == "Google") {
        embedding <- read.csv(file = path, header = FALSE, sep = " ")
        embedding <- assign_rownames(embedding)
        return(embedding)

    }

    if (source == "GloVe") {
        embedding <- read.csv(file = path, header = FALSE, sep = " ", quote = "")
        embedding <- assign_rownames(embedding)
        return(embedding)

    }

    if (source == "fastText") {
        embedding <- read.csv(file = path,
                    header = FALSE, sep = " ", quote = "")
        embedding <- embedding[2:nrow(embedding),]
        embedding <- assign_rownames(embedding)
        return(embedding)

    }
}

#' Make semantic dimension
#' 
#' @param embedding A word embedding in the form of a matrix 
#' @param pairs A set of antonym pairs 
#' @returns A dataframe with the projections
#' @export
#' 
make_dim <- function(embedding, pairs) {

    pairs <- dropMT(pairs)

    # Returns projection along specified dimension
    word_dims <- data.frame(matrix(NA, nrow(pairs), ncol(embedding)))
    for (j in 1:nrow(pairs)) {
        rp_word1 <- pairs[j, 1]
        rp_word2 <- pairs[j, 2]
        tryCatch(word_dims[j, ] <- dimension(embedding[rp_word1, ],
                                            embedding[rp_word2, ]),
                                            error = function(e) {
                                                print("some anto pairs missing")
                                            })
    }
    dim_ave <- colMeans(word_dims, na.rm = TRUE)
    dim_ave_n <- nrm(dim_ave)
return(dim_ave_n)
}

make_projections <- function(embedding) {

    common_lexicon <- scan("data/common_lexicon.txt", character(), quote = "")

    pairs <- read.csv("code/word_pairs/SD_word_pairs_EXPANDED_G.csv",
                      header = FALSE)

    ant_pairs_aff <- read.csv("code/word_pairs/affluence_pairs.csv",
                            header = FALSE, stringsAsFactor = FALSE)

    ant_pairs_gen <- read.csv("code/word_pairs/gender_pairs.csv",
                            header = FALSE, stringsAsFactor = FALSE)

    ant_pairs_race <- read.csv("code/word_pairs/race_pairs.csv",
                            header = FALSE, stringsAsFactor = FALSE)

    ant_pairs_eval <- pairs[, c(5,6)]

    ant_pairs_moral <- pairs[, c(1,2)]

    ant_pairs_aes <- pairs[, c(7,8)]

    ant_pairs_embod <- pairs[, c(45, 46)]

    ant_pairs_emot <- pairs[,c(65,66)]

    embedding <- as.matrix(embedding)

    embedding <- embedding[rownames(embedding) %in% common_lexicon, ]

    embedding <- t(apply(embedding, 1, nrm))

    aff_dim    <- make_dim(embedding, ant_pairs_aff)
    gender_dim <- make_dim(embedding, ant_pairs_gen)
    race_dim   <- make_dim(embedding, ant_pairs_race)
    eval_dim   <- make_dim(embedding, ant_pairs_eval)
    moral_dim  <- make_dim(embedding, ant_pairs_moral)
    aes_dim    <- make_dim(embedding, ant_pairs_aes)
    embod_dim  <- make_dim(embedding, ant_pairs_embod)
    emot_dim   <- make_dim(embedding, ant_pairs_emot)

    aff_proj    <- embedding %*% aff_dim
    gender_proj <- embedding %*% gender_dim
    race_proj   <- embedding %*% race_dim
    eval_proj   <- embedding %*% eval_dim
    moral_proj  <- embedding %*% moral_dim
    aes_proj    <- embedding %*% aes_dim
    embod_proj  <- embedding %*% embod_dim
    emot_proj   <- embedding %*% emot_dim

    #THIS IS where YOU WANT FOR EACH EMBEDDINGS
    projections_embedding <- cbind(aff_proj, gender_proj, race_proj, eval_proj,
                                   moral_proj, aes_proj, embod_proj, emot_proj)

    colnames(projections_embedding) <- c("aff_proj", "gender_proj", "race_proj",
                                         "eval_proj", "moral_proj", "aes_proj",
                                         "embod_proj", "emot_proj")

    projections_embedding <- as.data.frame(projections_embedding)

    projections_embedding$rank <- seq(1, nrow(projections_embedding), by = 1)

    return(projections_embedding)
}

produce_rank <- function(projection) {
    rank <- seq(1, nrow(projection), by = 1)

    print(length(rank) == nrow(projection))

    projection <- cbind(projection, rank)

    return(projection)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cor_mat){
  cor_mat[lower.tri(cor_mat)]<- NA
  return(cor_mat)
}

#' Standardize a variable
#' 
#' This needs to be changed to keep the rownames
#' @param x A variable,
#' @retruns The same variable standardized so mean is 0 and SD is 1
#' 
#' @export
standardize <- function(x){
    if(class(x) ==  "numeric"){
        x <- (x - mean(x)) / sd(x)
    return(x)
    }

    if(class(x) == "matrix" | class(x) == "data.frame"){
        x <- sapply(x, function(x) x <- (x - mean(x)) / sd(x))
    return(x)
    }

    if(class(x) != "data.frame" &
       class(x) != "numeric" &
       class(x) != "matrix"){
       stop("object is not a dataframe, matrix or vector")
    }
}

#' Exponentiate
#' 
#' Exponentiate a number or vector to a given power
#' @param x The number or vector to exponentiate
#' @param power The power to exponentiate to
#' @export 
#' 
#' @example 
#' number <- 2
#' exponent(2, 2)
exponent <- function(x, power) {
    (abs(x) ^ power) * sign(x)
    }
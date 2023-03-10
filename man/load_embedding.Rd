% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{load_embedding}
\alias{load_embedding}
\title{Load word embedding}
\usage{
load_embedding(path, source = c("Google", "GloVe", "fastText"))
}
\arguments{
\item{path}{A valid string path to the word embedding file}

\item{source}{indicating the source of the embedding file either,
"Google", "GloVe", or "fastText"}
}
\description{
Function for loading a word embedding
}

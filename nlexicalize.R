### @author: henryre ###

### Enable n-gramming in corpus lexicalization for 'lda' package
nlexicalize <- function (doclines, n = 1, sep = " ", lower = TRUE, count = 1L, vocab = NULL) 
{
  # Use standard implementation in unigram case
  if (n == 1) {
    split.docs <- strsplit(if (lower) 
      tolower(doclines)
      else doclines, sep, fixed = TRUE)
  }
  # Otherwise, use an n-gram tokenizer
  # Max length cannot exceed number of words in document
  else {
    require("RWeka")
    split.docs <- lapply(docs, function(x) {NGramTokenizer(x,Weka_control(min=1,
                                                                          max=min(n,length(unlist(strsplit(x, ' '))))))})
  }
  # Clone from standard lexicalize
  if (is.null(vocab)) {
    local.vocab <- unique(unlist(split.docs))
  }
  else {
    local.vocab <- vocab
  }
  split.docs <- lapply(split.docs, function(x) {
    m <- match(x, local.vocab)
    m <- m[!is.na(m)]
    rbind(m - 1L, rep(as.integer(count), length(m)))
  })
  if (is.null(vocab)) {
    return(list(documents = split.docs, vocab = local.vocab))
  }
  else {
    return(split.docs)    
  }
}

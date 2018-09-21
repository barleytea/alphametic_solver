# constants
OPERATOR <- c("+", "-", "*")
EQUAL_SIGN <- "="
PARENTHESES <- c("(", ")")
NUMBERS <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
NO_ALP <- c(OPERATOR, EQUAL_SIGN, PARENTHESES, NUMBERS)

recite.permutations <- function (r) {
  n <- 10
  v <- 0:9

  if (mode(r) != "numeric" || length(r) != 1 || r < 1 || (r%%1) != 0) {
    stop("bad value of r")
  }
  if ((r > n)) {
    stop("r > n")
  }

  v0 <- vector(mode(v), 0)
  sub <- function(n, r, v) {
    if (r == 1) {
      matrix(v, n, 1)
    } else {
      X <- NULL
      for (i in 1:n) {
        X <- rbind(X, cbind(v[i], Recall(n - 1, r - 1, v[-i])))
      }
      X
    }
  }
  sub(n, r, v[1:n])
}

FindNonZeroIndexes <- function(exp, alp.ind) { # only alp
  exp.parentheses.removed <- exp[-which(exp %in% PARENTHESES)]
  nonzero.indexes <- c();
  if (!(exp.parentheses.removed[1] %in% NUMBERS)) {
    nonzero.indexes <- c(nonzero.indexes, 1)
  }
  nonzero.indexes <-  c(nonzero.indexes, which(exp.parentheses.removed %in% NO_ALP))
  return(nonzero.indexes)
}

AlphameticSolver <- function() {
  lines <- readLines("stdin")

  # split character by character
  splitted <- noquote(strsplit(lines, split=NULL)[[1]])
  # split into the left and the right
  left <- noquote(strsplit(substr(lines, 1, which(splitted == EQUAL_SIGN) - 1), split=NULL)[[1]])
  right <- noquote(strsplit(substr(lines, which(splitted == EQUAL_SIGN) + 1, length(splitted)), split=NULL)[[1]])

  # indexes of alphabets
  left.alp.indexes <- which(!(left %in% NO_ALP))
  right.alp.indexes <- which(!(right %in% NO_ALP))

  # find non-zero indexes
  left.nonzero.indexes <- FindNonZeroIndexes(left, left.alp.indexes)
  right.nonzero.indexes <- FindNonZeroIndexes(right, right.alp.indexes)

  nonzero.characters <- unique(c(left[left.alp.indexes[left.nonzero.indexes]], right[right.alp.indexes[right.nonzero.indexes]]))

  letters.candidates <- unique(splitted[!(splitted %in% NO_ALP)])

  solutions <- rep(0, length(letters.candidates))
  names(solutions) <- letters.candidates

  res <- c()
  combination.candidates <- recite.permutations(length(letters.candidates))
  combination.candidates <- combination.candidates[, which(names(solutions) %in% nonzero.characters)] != 0
  for (i in 1:dim(combination.candidates)[1]) {
    left.comp <- left; right.comp <- right;
    solutions[1:length(solutions)] <- combination.candidates[i, ]
    for (j in 1:length(left.alp.indexes)) {
      left.comp[left.alp.indexes[j]] <- solutions[names(solutions) == left[left.alp.indexes[j]]]
    }
    for (k in 1:length(right.alp.indexes)) {
      right.comp[right.alp.indexes[k]] <- solutions[names(solutions) == right[right.alp.indexes[k]]]
    }
    left.pasted <- paste(left.comp, collapse="")
    right.pasted <- paste(right.comp, collapse="")
    if (eval(parse(text=left.pasted)) == eval(parse(text=right.pasted))) {
      res <- c(res, paste(left.pasted, right.pasted, sep="="))
    }
  }

  for (i in 1:length(res)) {
    write(res[i], stdout())
  }
}
# driver code
AlphameticSolver()

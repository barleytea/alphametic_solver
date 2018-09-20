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


AlphameticSolver <- function() {
  # constants
  OPERATOR <- c("+", "-", "*")
  EQUAL_SIGN <- "="
  PARENTHESES <- c("(", ")")
  NUMBERS <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
  NO_ALP <- c(OPERATOR, EQUAL_SIGN, PARENTHESES, NUMBERS)

  lines <- readLines("stdin")

  # split character by character
  splitted <- noquote(strsplit(lines, split=NULL)[[1]])
  # split into the left and the right
  left <- noquote(strsplit(substr(lines, 1, which(splitted == "=") - 1), split=NULL)[[1]])
  right <- noquote(strsplit(substr(lines, which(splitted == "=") + 1, length(splitted)), split=NULL)[[1]])

  # indexes of alphabets
  left.alp.indexes <- which(!(left %in% NO_ALP))
  right.alp.indexes <- which(!(right %in% NO_ALP))

  left.items <- unlist((strsplit(paste(left, collapse=""), "(\\+|\\-|\\*)")))
  right.items <- unlist((strsplit(paste(right, collapse=""), "(\\+|\\-|\\*)")))

  letters.candidates <- unique(splitted[!(splitted %in% NO_ALP)])

  solutions <- rep(0, length(letters.candidates))
  names(solutions) <- letters.candidates

  res = c()
  combination.candidates <- recite.permutations(length(letters.candidates))
  combination.candidates <- combination.candidates[combination.candidates[,1] != 0, ]
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
    if (eval(parse(text=left.pasted)) == eval(parse(text=right.pasted)) & right.comp[right.alp.indexes[1]] != 0) { # XXX
      res <- c(res, paste(left.pasted, right.pasted, sep="="))
    }
  }

  for (i in 1:length(res)) {
    write(res[i], stdout())
  }
}
# driver code
AlphameticSolver()

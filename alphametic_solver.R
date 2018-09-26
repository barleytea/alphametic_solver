# constants
OPERATOR <- c("+", "-", "*")
EQUAL_SIGN <- "="
PARENTHESES <- c("(", ")")
NUMBERS <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
NO_ALP <- c(OPERATOR, EQUAL_SIGN, PARENTHESES, NUMBERS)
LINES <- readLines("stdin")

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

SplittedLinesList <- function() {
  splitted.lines <- list()
  for (i in 1:length(LINES)) {
    splitted.lines <- c(splitted.lines, list(noquote(strsplit(LINES, split=NULL)[[i]])))
  }
  return (splitted.lines)
}

FindCharacterCandidates <- function() {
  character.candidates <- c()
  for (i in 1:length(LINES)) {
    splitted <- noquote(strsplit(LINES, split=NULL)[[i]])
    character.candidates <- c(character.candidates, unique(splitted[!(splitted %in% NO_ALP)]))
  }
  return(unique(character.candidates))
}

FindNonZeroIndexes <- function(expr, alp.ind) { # only alp
  parentheses.indexes <- which(expr %in% PARENTHESES)
  if(length(parentheses.indexes) == 0) {
    expr.parentheses.removed <- expr
  } else {
    expr.parentheses.removed <- expr[-parentheses.indexes]
  }
  nonzero.indexes <- c();
  if (!(expr.parentheses.removed[1] %in% NUMBERS)) {
    nonzero.indexes <- c(nonzero.indexes, 1)
  }
  nonzero.indexes <-  c(nonzero.indexes, which(expr.parentheses.removed %in% NO_ALP))
  return(nonzero.indexes)
}

AlphameticSolver <- function() {
  res.solution <- c(); res.expr <- c()
  for (i in 1:length(LINES)) {
    # split character by character
    splitted <- SplittedLinesList()[[i]]
    # split into the left and the right
    left <- noquote(strsplit(substr(LINES[i], 1, which(splitted == EQUAL_SIGN) - 1), split=NULL)[[1]])
    right <- noquote(strsplit(substr(LINES[i], which(splitted == EQUAL_SIGN) + 1, length(splitted)), split=NULL)[[1]])

    # indexes of alphabets
    left.alp.indexes <- which(!(left %in% NO_ALP))
    right.alp.indexes <- which(!(right %in% NO_ALP))

    left.nonzero.indexes <- FindNonZeroIndexes(left, left.alp.indexes)
    right.nonzero.indexes <- FindNonZeroIndexes(right, right.alp.indexes)
    nonzero.characters <- unique(c(left[left.alp.indexes[left.nonzero.indexes]], right[right.alp.indexes[right.nonzero.indexes]]))

    # prepare solutions vector whose column is named 
    character.candidates <- FindCharacterCandidates()
    solutions <- rep(0, length(character.candidates))
    names(solutions) <- character.candidates

    combination.candidates <- recite.permutations(length(character.candidates))
    nonzero.indexes <- which(names(solutions) %in% nonzero.characters)
    for (j in 1:length(nonzero.indexes)) {
      combination.candidates <- combination.candidates[combination.candidates[, nonzero.indexes[j]] != 0, ]
    }

    for (k in 1:dim(combination.candidates)[1]) {
      left.comp <- left; right.comp <- right;
      solutions[1:length(solutions)] <- combination.candidates[k, ]
      if (length(left.alp.indexes) > 0) {
        for (l in 1:length(left.alp.indexes)) {
          left.comp[left.alp.indexes[l]] <- solutions[names(solutions) == left[left.alp.indexes[l]]]
        }
      }
      if (length(right.alp.indexes) > 0) {
        for (m in 1:length(right.alp.indexes)) {
          right.comp[right.alp.indexes[m]] <- solutions[names(solutions) == right[right.alp.indexes[m]]]
        }
      }
      left.pasted <- paste(left.comp, collapse="")
      right.pasted <- paste(right.comp, collapse="")
      if (eval(parse(text=left.pasted)) == eval(parse(text=right.pasted))) {
        res.solution <- rbind(res.solution, solutions)
        res.expr <- c(res.expr, paste(left.pasted, right.pasted, sep="="))
      }
    }
  }
  if (length(LINES) == 1) {
    for (i in 1:length(res.expr)) {
      write(paste0(res.expr[i], ifelse(i != length(res.expr), "\n", "")), stdout())
    }
  } else {
    res.ind <- res.ind <- which(mapply(any, duplicated(res.solution), duplicated(res.solution, fromLast=TRUE)))
    res <- matrix(res.expr[res.ind], nrow=length(LINES), ncol=length(res.ind)/2, byrow=TRUE)
    for (i in 1:dim(res)[2]) {
      for (j in 1:dim(res)[1]) {
        write(paste0(res[j, i], ifelse(i != dim(res)[2] & j == dim(res)[1], "\n", "")), stdout())
      }
    }
  }
}

# driver code
AlphameticSolver()

# Scratch work
A <- matrix(101:115, ncol=3)
B <- matrix(21:30, ncol=2)

C <- matrix(nrow = nrow(A), ncol=(ncol(A) * ncol(B)))
k = 0
for (i in 1:ncol(B)) {
        for (j in 1:ncol(A)) {
          k = k + 1
          C[, k] <- A[, j]/B[,i]
        }
}

browser()
D <- matrix(nrow = nrow(A), ncol = (ncol(A)* ncol(B)))
D <- apply(B,2,apply(A,2,function(x, y) x/y))

rm(C)
C <- NULL
for (i in seq_along(B[1,]))
  C <- cbind(C, sweep(A, 1, B[,i], "/"))

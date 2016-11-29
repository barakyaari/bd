assert COLS == 1 : "Number of columns should be: 1"

// a zero-argument function
def five() {
  5
}

// This is the definition of the first column.
def TABLE[i==1][j] {
  TABLE[1][j] * five()
}
// A constraint on the number of columns in the input table.
assert COLS == 3 : "Number of columns should be:\t3"
var COLS2 = -COLS * -2 // This defines a constant COLS2

// This is a function definition. x and y a argument variables.
def weirdAverage(x, y) {
  x * y / (x + y)
}

// This is the definition of the first column.
def table[i==1][j] {
  TABLE[1][j] * -18.7
}

// This column definition simply copies the values from the first input column.
def TABLE[i==2][i] {
  TABLE[1][j]
}

/* This is a multi-line comment.
def TABLE[i==2][j] {
  TABLE[1][j] * -1
}
*/

def TABLE[i==3][j] {
  if (TABLE[2][j] + TABLE[3][j] > 0 /\ TABLE[2][j] > 0) {
    weirdAverage(TABLE[2][j], TABLE[3][j])
  }
  else {
    0
  }
}

def TABLE[i==4][j] {
  if (j < ROWS) {
    0
  }
  else {
    sum = 0;
    for (k = 1..ROWS) {
      sum += TABLE[1][k]
    };
    average /= ROWS
  }
}
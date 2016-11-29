assert (COLS == 3) : "Number of columns should be:\t3"

var COLS2 = ((0 - COLS) * (0 - 2))

def weirdAverage(x, y) {
    ((x * y) / (x + y))
}

def TABLE[(i == 1)][j] {
    (TABLE[1][j] * (0 - 18.7))
}

def TABLE[(i == 2)][i] {
    TABLE[1][j]
}

def TABLE[(i == 3)][j] {
    if ((((TABLE[2][j] + TABLE[3][j]) > 0) /\ (TABLE[2][j] > 0))) {
        weirdAverage(TABLE[2][j], TABLE[3][j])
    }
    else {
        0
    }
}

def TABLE[(i == 4)][j] {
    if ((j < ROWS)) {
        0
    }
    else {
        sum = 0;
        for (k = 1..ROWS) {
            sum = (sum + TABLE[1][k])
        };
        average = (average / ROWS)
    }
}
"I am thinking of a 6-digit number. The sum of the digits is 43.

And only two of the following three statements about the number are true:

(1) it's a square number,
(2) it's a cube number, and
(3) the number is under 500000."

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
is.square <- function(x) is.wholenumber(sqrt(x))
is.cube <- function(x) is.wholenumber(x^(1/3))

add6digits <- function(x) {tot=0
                           str = as.character(x)
                           for (i in 1:6) tot = tot + as.numeric(substr(str,i,i))
tot
}

addto43check <- function(x) add6digits(x)==43

#functions all defined - start using them
dig6=seq(from = 100000, to = 999999)
tot43s = dig6[addto43check(dig6)]
squares = tot43s[is.square(tot43s)]
cubes = tot43s[is.cube(tot43s)]

#Looking for (square < 500k) or (cube < 500k) or (square&cube > 500k)
i=intersect(squares, cubes)
c(squares[squares < 500000], cubes[cubes < 500000],  i[i>500000] )

#So 499849

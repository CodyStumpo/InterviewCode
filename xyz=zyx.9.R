"Challenge: Find three digits X, Y and Z such that XYZ in base10 (ten) is equal to ZYX in base9 (nine)"
for (i in 100:999){ str = as.character(i)
 x = substr(str,1,1)
 y = substr(str,2,2)
 z = substr(str,3,3)
 zyx = as.numeric(z)*(9^2) + as.numeric(y)*9 + as.numeric(x)
 if(zyx == i) print(i)
}
  
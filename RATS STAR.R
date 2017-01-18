# RATS x 4 = STAR

# Each instance of a letter stands for the same digit, no two letters represent the same digit, 
# and R cannot be 0. Which digits do the four letters represent?

rats = seq(1000, 2500)
starFn = function(x) as.numeric(paste(rev(strsplit(as.character(x),"")[[1]]),collapse="")) 
star=mapply(starFn,rats)
rats[which(star==rats*4)]

# 2178 * 4 = 8712

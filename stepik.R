### Stepik
### 18/06/2021

### welcome Q1
df <- read.table("./input.txt", sep = " ")

write.table(apply(df, 1, sum), file = "./answer.txt", 
            sep = "\n", row.names = F, col.names = F, quote = F)

### epigentic Q1
# theory to solve the Q: 
# read 2nd line: number of col, number of row 
# read (number of col) line
# pipe them into df
library(tidyverse)

## if string > 1
# too lazy to read the whole file: 1.txt
# manually add strings

df <- cbind(unlist(strsplit(x = "1101110111011010110000100011111100010110011000100010100010100001100000010010100101101101111101011110", "")),
            unlist(strsplit(x = "0011110101100000110011000110111101100001100010001001101001111101011010100010010100111110011001101010", "")),
            unlist(strsplit(x = "1110010000011101011111111110010000101100110111001111111101111010100111000011011101010001110011111111", "")),
            unlist(strsplit(x = "0100011011010011011000100110010111010100110101101101000101011011001111111101011110111110100100111100", "")))

df <- as.data.frame(df) 

for (i in 1:ncol(df)) {
  df[,i] <- as.numeric(df[,i])
}
df
dim(df)

## sum by 2 bit logic
# if ncol = 3
df$sum <- factor(df$V1*8 + df$V2*4 + df$V3*2)
#if ncol = 4
df$sum <- factor(df$V1*16 + df$V2*8 + df$V3*4 + df$V4*2)


levels(df$sum)
levels(df$sum) %>% length()

rank <- df %>% distinct(sum)

levels(df$sum) <- list("1"=rank[1,], "2"=rank[2,], "3"=rank[3,], "4"=rank[4,], 
                       "5"=rank[5,], "6"=rank[6,], "7"=rank[7,], "8"=rank[8,],
                       "9"=rank[9,], "10"=rank[10,], "11"=rank[11,], "12"=rank[12,],
                       "13"=rank[13,], "14"=rank[14,], "15"=rank[15,], "16"=rank[16,])

paste(df$sum, collapse = " ")


## if string = 1
s = "0000101100011010110011110101110101101010101011010101100011010111001000100100010110111011101010000010"
# if 0 comes first
df_s <- gsub("1","2", s) 
df_s <- gsub("0","1", df_s) 
gsub("", " ",df_s) 
# if 1 comes first
df_s <- gsub("0","1", s) 
gsub("", " ",df_s)


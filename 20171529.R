##¹®Á¦1. µ¥ÀÌÅÍ ¼±Á¤ ÀÌÀ¯, µ¥ÀÌÅÍ ±¸Á¶ ¹× ³»¿ë ¼³¸í 

install.packages("dplyr")
library(dplyr)
install.packages("tidytext")
library(tidytext)
install.packages("stringr")
library(stringr)

#Rain, Donation ÀÌ¶ó´Â º¯¼ö¿¡ deta ÆÄÀÏ ³Ö±â
Rain <- readLines("Rain.txt", encoding = "UTF-8")
head(Rain)

#Æ¯¼ö,ÇÑÀÚ,°ø¹é Á¦°Å
Rain0 = Rain %>% str_replace_all("[^°¡-ÆR]", " ")
head(Rain0)

#µ¥ÀÌÅÍ¸¦ tibble ±¸Á¶·Î º¯°æ
Rain=Rain %>% as_tibble()
Rain

#text¶ó´Â º¯¼ö¿¡ value °ªÀ» ³ÖÀº tibble ¸¸µé±â
text <- tibble(value="³»´Ş 17ÀÏ±îÁö ÁßÁ¡ °ü¸®±â°£ ¾ÈÀü½ÇÅÂ È®ÀÎ. ³ó¸²Ãà»ê½ÄÇ°ºÎ´Â 2ÀÏºÎÅÍ Àå¸¶ ÀüÀÎ 6¿ù ÇÏ¼ø±îÁö ÅÂÇ³, ÁıÁßÈ£¿ì, Æø¿° µî ¿©¸§Ã¶ ÀçÇØ¸¦ ¿¹¹æÇÏ±â À§ÇÑ ºĞ¾ßº° Ãë¾à½Ã¼³ »çÀüÁ¡°ËÀ» ½Ç½ÃÇÑ´Ù°í ¹àÇû´Ù.")
text

#text º¯¼ö¸¦ ¹®ÀåÀ¸·Î ³ª´©´Â ÅäÅ«È­ ¸¸µé±â
text %>% unnest_tokens(input=value, output=word, token="sentences")

#text º¯¼ö¸¦ ¶ç¾î¾²±â·Î ³ª´©´Â ÅäÅ«È­ ¸¸µé±â
word_text <- Rain %>% unnest_tokens(input=value, output="word")
word_text


##¹®Á¦2. °¡Àå ÀÚÁÖ »ç¿ëµÈ ´Ü¾î ÃßÃâ ¹× ºóµµ ±×·¡ÇÁ ¸¸µé±â

#´Ü¾îºóµµ ±¸ÇÏ±â-count()ÇÔ¼ö »ç¿ë
word_text <- word_text %>% count(word, sort=T)
word_text

str_count("µî")
str_count("µîÀ»")

#´Ü¾îÀÇ ±æÀÌ°¡ 1º¸´Ù ±ä Çà¸¸ ÃßÃâ
word_text=word_text %>% filter(str_count(word)>1)
word_text

#ÀÚÁÖ »ç¿ëµÈ ´Ü¾î ÃßÃâ
top30 <- word_text %>% head(30)
top30

#¹ß»ıºóµµ
install.packages("ggplot2")
library(ggplot2)

ggplot(top30, aes(x=reorder(word, n), y=n))+ geom_col() + 
  coord_flip() + geom_text(aes(label=n), hjust=-0.3) +
  labs(title = "Àå¸¶,ÅÂÇ³ ÀçÇØ¿¹¹æ ´Ü¾îºóµµ", x=NULL, y=NULL )+
  theme(title=element_text(size=20))

#¹®Á¦3. ¿ÀÁîºñ ¶Ç´Â TF-IDF È°¿ëÇÏ¿© ºĞ¼®ÇÏ±â
install.packages("stringr")
library(stringr)
install.packages("tidytext")
library(tidytext)

#Rain,Donatin µ¥ÀÌÅÍ¸¦ Å×ÀÌºí ÇüÅÂ·Î ¸¸µé°í ÇÕÄ¡±â
Rain <- readLines("Rain.txt", encoding = "UTF-8")
Rain2 <- Rain %>% as_tibble() %>% mutate(president = "Rain2")
head(Rain2)

Donation <- readLines("Donation.txt", encoding = "UTF-8")
Donation2 <- Donation %>% as_tibble() %>% mutate(president = "Donation2")
head(Donation2)

#µÎ°³ÀÇ µ¥ÀÌÅÍ ÇÕÄ¡°í Ãâ·ÂÇÏ±â
Sum <- bind_rows(Rain2, Donation2) %>% select(president, value)
head(Sum)
tail(Sum)

#ÇÑ±Û ÀÌ¿ÜÀÇ ¹®ÀÚÁ¦°Å, ¿¬¼Ó°ø¹é Á¦°Å
speeches <- Sum %>% mutate(value=str_replace_all(value,
          "[^°¡-ÆR]", " "), value=str_squish(value))
        
speeches

speeches <- speeches %>% unnest_tokens(input=value, output=word, token=extractNoun)
#-------------------------------------------------------------------
install.packages("readr")
library(readr)

raw_Donation <- read_csv("Donation.csv")
raw_Donation

Donation <- raw_Donation %>% mutate(value=str_replace_all(value,"[^°¡-ÆR]", " "), value=str_squish(value))


Donation <- Donation %>% unnest_tokens(input=value, output=word, token=extractNoun)










#¹®Á¦4. °¨Á¤»çÀüÀ» Àû¿ëÇÏ¿©, ÅØ½ºÆ®ÀÇ °¨Á¤ °æÇâÀ» ºĞ¼®ÇÏ±â
#°¨Á¤»çÀü ºÒ·¯¿À±â
dic <- read_csv("knu_sentiment_lexicon.csv")
dic

#º¯¼ö¿¡ ÅØ½ºÆ®¸¦ tibble ÇüÅÂ·Î ¸¸µé±â
text <- tibble(sentence=c("±âºĞÀÌ ÁÁ´Ù.", 
                        "³¯¾¾°¡ ÁÁ´Ù.",
                        "ÇÇ°ïÇÏ´Ù.",
                        "ºÒÆíÇÏ´Ù.",
                        "±âºĞÀÌ ³ª»ÚÁö¸¸ ±¦Âú´Ù.",
                        "±ÍÂúÁö¸¸ »ÑµíÇÏ´Ù."))
text



text <- text %>% unnest_tokens(input=sentence, output=word, token="words", drop=F)
text

text <- text %>% left_join(dic, by="word") %>% mutate(polarity=ifelse(is.na(polarity), 0, polarity))
text

score_df <- text %>% group_by(sentence) %>% summarise(scord=sum(polarity))
score_df



#¹®Á¦5. °¨Á¤»çÀü ¼öÁ¤ÇÏ¿© Àû¿ëÇÏ°í, ¼öÁ¤Àü°ú ºñ±³ºĞ¼®ÇÏ±â

new_dic <- dic %>% mutate(polarity=ifelse(word %in% c("ÇÇ°ïÇÏ´Ù"),-4,polarity))
new_dic %>% filter(word %in% c("ÇÇ°ï"))
new_dic

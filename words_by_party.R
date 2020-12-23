#Laina Emmons 7/15/2020
#sort emails w/ data by each party to determine info surrounding each

setwd("/Users/lainaemmons/Documents/summer_2020/R")
getwd()

partisan_data <- read.csv("data/immigration_with_partisanship.csv")
partisan_data$Date <- as.Date(partisan_data$Date, "%m/%d/%y")
partisan_data = subset(partisan_data, select = -c(Numeric.counter.for.order.in.THOMAS.data.from.93rd...110th.Congresses,
                                                  From, ICPSR.number..according.to.Poole.and.Rosenthal, Subject, 
                                                  Congress.number, Year.at.start.of.Congress, Congressional.district.number)) 

library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(stringr)
library(tm)
library(widyr)
library(sentimentr)
library(igraph)
library(ggraph)
data("stop_words")
immigration_stopwords <- tibble(word = c( "\n", "to", "from", "friends", "gmail", "instagram", "repkevinyoder",
                                          "facebook", "twitter", "youtube", "fax", "washington", "d.c", "d.c.", "dc", 
                                          "unsubscribe", "newsletter", "congress", "sincerely", "u.s", "u.s.",
                                          "rep", "representative", "enewsletter", "office", "senate", "tx", 
                                          "city", "county", "am", "pm", "week", "congressman", "monday", "tuesday", 
                                          "wednesday", "thursday", "friday", "saturday", "sunday", "cuellar", "00", "january", 
                                          "february", "april", "may", "june", "july", "august", "september", "october", 
                                          "december", "2016", "2017", "2018", "2019", "2020", "phone", "202", "225", "20515", 
                                          "mr", "ms", "mrs", "congresswoman", "p.o", "box", "plain", "text", "subscription", 
                                          "hall", "click", "<NA>", "lindsey", "hours", "hour", "et", "ct", "pt", "senator", 
                                          "senators", "friend", "dear", "browser", "federal", "government",
                                          "house", "july", "office", "phone", "rep", "washington", "fax", "click",
                                          "polsexpgmailcom", "congressman", "district", "email", "message", "mst",
                                          "please", "street", "share", "subject", "suite", "unsubscribe", "week",
                                          "act", "congresswoman", "city", "county", "christensen", "corker", "update",
                                          "legislation","time", "hours", "mail", "online", "begich", "day", "senator",
                                          "congressional", "broun", "september", "texas", "cdt", "massie", "ayotte",
                                          "kentucky", "mdt", "murphy", "kelly", "tim", "pennsylvania", "edt",
                                          "october", "november", "est", "december", "january", "kansas", "updates",  
                                          "replyto", "serrano", "doug", "bronx", "lamborn", "\u2028\u2028hr","\u2028", 
                                          "february", "\u2028cfda","march", "april", "june", "minnesota", "browser",  
                                          "information", "indiana", "alaska", "palmer", "oklahoma", "frank", "lucas",  
                                          "laredo", "marco", "\u2028cfda", "cook", "valley,","san", "phoenix", "pedene", 
                                          "representative", "plain", "text", "nebraska", "window", "building", "portman", 
                                          "tuesday", "friday", "\u2028", "forward", "help", "sen", "jones", "view", "news",
                                          "\u2028cfda", "ny", "tn", "il", "am", "pm", "blvd", "th", "nh", "op", "mp",
                                          "subscribe", "mariana", "ca", "rayburn", "mn", "east", "la", "los", "de",
                                          "rio", "tel", "puerto", "porter", "longworth", "percent", "santa", "tinian",
                                          "cannon", "lincolnshire", "brad", "schneider", "korea", "saipan", "rota",
                                          "west", "cooper", "nashville", "korean", "korea's", "kim", "jong", "un",
                                          "a.m", "p.m", "barclay", "jim", "net", "neutrality", "press", "conference",
                                          "weekly", "wrap", "cancer", "capitol", "art", "highlights", "telephone",
                                          "halls", "carolina", "billion", "million", "passed", "main", "library",
                                          "farm", "h.r", "trump's", "administration's", "fox", "az", "mo", "network",
                                          "byrne", "va", "fl", "ph", "pa", "al", "res", "mobile", "hearing", "denham", 
                                          "nc", "sc", "dirksen", "days", "gulfport", "wa", "ks", "ne", "ga", "fy", "ga",
                                          "ste", "h.res", "jackson", "wichita", "pittsburg", "highway", "st", "trillion",
                                          "options", "including", "privacy", "tupelo", "vietnam", "hernando", "facing",
                                          "ar", "roll", "courthouse", "ribbon", "jody", "hice", "rick", "scott",
                                          "alabama", "lake", "app", "mailing", "matt", "gaetz", "olathe", "merit",
                                          "overland", "remain", "contact", "nation's", "nancy", "pelosi", "orange",
                                          "td", "nbsp", "width", "px", "comm", "org", "img", "pixel", "po", "mj",
                                          "safeunsubscribe", "tm", "victor", "ave", "rw", "migrationpolicy", "amp",
                                          "bodythis", "mailto", "link", "mailto", "subjectfw", "webkit", "africacenter",
                                          "cnt", "onn", "txt", "cnt", "externalclass", "preferences", "gheen", 
                                          "readmsgbody", "span", "hotmail", "font", "content", "massachusetts", "csis",
                                          "yoo", "subscriptions", "footer", "header", "grey", "utf", "html", "onn",
                                          "applelinkwhite", "rss", "gt", "ldquo", "div", "mpi", "externalclassfont",
                                          "blog", "ios", "mi", "nw", "nav", "du", "web", "table", "worldfrom", "display",
                                          "styles", "body", "resize", "resized", "image", "getty", "arial", "helvetica", 
                                          "serif", "yourname", "logo", "templatebody", "mcntextcontent", "templateheader",
                                          "templatepreheader", "ps", "hoover", "manhattan", "baker", "enterprise", 
                                          "hudson", "cato", "artic", "fordham", "berggruen", "brookings", "stamos",
                                          "center", "cepr", "iwpr", "john", "pewresearch", "issr", "doha", "reserved",
                                          "minister", "york", "social", "viewing", "i'm", "dakota", "island", "mueller",
                                          "kavanaugh", "god", "includes", "judiciary", "newsroom", "subcommittee", "matter",
                                          "college", "academics", "opioid", "survivors", "candidates", "aa", "cbf", 
                                          "png", "aaa", "foundationfor", "fundorg", "aaafund", "accurateand", "aacute", 
                                          "rdenas", "aadhaar", "aai", "wantsto", "aaif", "aaifs", "aais", "aaaf", 
                                          "aabrahim", "shahif", "aaccurateand", "aajc", "alslaunched", "aak", 
                                          "acquiredbritish", "aalac", "atlanticcouncil", "aam", "aadmibut", 
                                          "aamia", "aanhpis", "aanna", "aansel", "aapi", "aaron", "aakshi",
                                          "aarhus", "aarp", "aashto", "aasivissuit", "nipisat", "aasland", 
                                          "aawws", "aayog", "ab", "aba", "ababa", "ababi", "abadula", "aafdeea",
                                          "abalanced", "aban", "abandonedan", "abandonedby", "abandoningthe",
                                          "abarca", "abawd", "abbas", "abbasi", "abbeys", "abbie", "abbott", "abbtt", 
                                          "galvoexplains", "abby", "abc", "abdel", "fatah", "fattah", "abdelaziz", 
                                          "bouteflika", "actionkit", "civilrights", "adc", "address", "rsquo",
                                          "adrienne", "arsht", "aei", "aeideas", "afl", "cio", "alipac", "alerts",
                                          "amanda", "glassman", "americaby", "ammar", "campa", "amy", "quot", "anders",
                                          "aslund", "andreas", "raspotniks", "andrew", "andrs", "angela", "angeles",
                                          "anit", "ann", "antonio", "ashishsen", "monthly", "style", "color", "obamas",
                                          "bc", "cca", "bd", "bd", "bfcca", "bdc", "outlook", "benjamin", "beto", "betsy",
                                          "bernie", "hr", "billy", "kimberly", "reinsch", "whalen", "bin", "birthday",
                                          "blockquote", "thread", "boko", "boris", "boston", "ma", "brent", "brian", 
                                          "james", "burkina", "faso", "cambridge", "campa", "nrcat", "cebrian", "karen",
                                          "cepeda", "ulloa", "chanda", "katherine", "charles", "chen", "li", "colors", 
                                          "chuck", "chris", "cindy", "cisac", "claude", "cohen", "colby", "colinas", 
                                          "colm", "colbrancaleb", "asd", "encoding", "youll", "ivn", "ifda", "duque",
                                          "laura", "manuel", "lopez", "movie", "pad", "sbe", "ntn", "fpif", "tomorrow",
                                          "alain", "alex", "alexei", "anthony", "bruce", "cameron", "jacques", "nakagawa",
                                          "diego", "diehl", "dorsey", "mohamed", "gonzalez", "fred", "kathleen", "george",
                                          "gerhard", "isaacson", "jeff", "jeremy", "jimmy", "jonathan", "jon", "juan", 
                                          "keating", "sergio", "khanna", "seung", "kudrin", "pascal", "lee", "jared", "lamy",
                                          "luiza", "bandeira", "mahbubani", "marek", "belka", "raghuram", "myers", "jaffe",
                                          "naim", "nayan", "nat", "omidyar", "eric", "otmar", "issing", "peter", "pierre",
                                          "john", "rajan", "cnbc", "nicolas", "zack", "flickr", "hutchins", "mark", 
                                          "objfull", "objfullmed", "objmed", "objsmall", "sender", "tps", "dany", "hunter",
                                          "duncan", "thompson", "moyo", "greg", "edward", "larry", "oren", "emma", "jenny",
                                          "jessica", "karli", "pramila", "jayapal", "sq", "ft", "tyler", "wallace", 
                                          "dan", "emails", "tweet", "fwd", "follow", "foodservice", "kelli", "comtuesday",
                                          "sli", "todd", "maki", "tammy", "mdash", "alertfriday", "en", "el", "nicols",
                                          "kevin", "elon", "updateyour", "fcnl", "janis", "jonah", "linkedin", "megan",
                                          "rugy", "lng", "heising", "simons", "simon", "joe", "ken", "pfaw", "policymay",
                                          "brett", "dilogo", "interamericano", "julia", "dfrlab", "nicole", "tv", "ad",
                                          "imran", "khan", "reply", "moveon", "dailydeduction", "taxpolicycenter",
                                          "jbs", "russell", "michael", "cory", "id", "herald", "craig", "comwednesday", 
                                          "fukuyama", "marie", "kirstjen", "nielsen", "robert", "gornick", "luxembourg",
                                          "mchugh", "ops", "fin", "janet", "gardels", "markos", "desmond", "lachman", 
                                          "vijaya", "richard", "recep", "ted", "russ", "michael", "cuny", "graduate", 
                                          "jamal", "webinar", "margie", "dracopoulos", "cgd", "sri", "byengaging", 
                                          "dont", "mike", "doris", "joseph", "strengthenour", "schwartz", "bde",
                                          "allrights", "ofthe", "billingsley", "connecticut", "lloyd", "seth", 
                                          "bailey", "amrica", "fsi", "hamilton", "rdquo", "eagle", "receiving", 
                                          "removal", "davis", "fellow", "fellows", "saudi", "arabia", "inter", "join",
                                          "pew", "institute", "institution", "center", "preheader", "rashida", "tab",
                                          "professor", "health", "pas", "champions", "laureate", "tech", "transatlantic",
                                          "conduct", "cheif", "read", "transportation", "red", "blue", "executive", "ceo",
                                          "director", "editor", "magazine", "paper", "promote", "reuters", "reporters",
                                          "reporter", "copyright", "emmanuel", "signed", "voter", "feel", "pope", "francis",
                                          "french", "medium", "button", "insightful", "scholarly", "emailing", "views",
                                          "view", "transatlanticmustreads", "key", "austrian", "pdas", "hillary", "lgbtq",
                                          "lgbt", "russian", "researchand", "hemisphere", "jair", "tb", "media", "itif",
                                          "buttonstyleshhha", "bolsonaro", "callout", "preheader", "tlaib", "footerlogo",
                                          "dr", "tableblock", "daniel", "ilhan", "omar", "tech", "tony", "payan", 
                                          "ru", "commentary", "advisory", "georgetown", "university", "info", "ips", 
                                          "profile", "tayyip", "erdogan", "mt", "denver", "sw", "plaza", "efant", "pda",
                                          "page", "trade", "prices", "selee", "sydicate", "naral", "alexandria", "ocasio",
                                          "lisa", "viscidi", "birch", "koch", "staff", "award", "awards", "kamala", "harris",
                                          "vladimir", "tuned", "jack", "goldsmith", "prince", "crown", "nuclear", "jason",
                                          "marczak", "ll", "ii", "welt", "headings", "links", "bank", "ben", "hanson", 
                                          "heather", "mac", "daily", "xi", "jinping", "lpez", "obrador", "rfe", "rl",
                                          "michelle", "mittelstadt", "condoleezza", "secular", "palestine", "palestinian",
                                          "elementary", "coalitiob", "trusts", "donate", "behalf", "delivery", "donot",
                                          "leland", "stanford", "thomas", "sowell", "johns", "hopkins", "event", "newsletters",
                                          "robby", "soave", "stacey", "abrams", "fluid", "printable", "padding", "line", 
                                          "layout", "spacing", "rendering", "line", "nick", "gillespie", "internet", 
                                          "stories", "kingdom", "fran", "burwell", "hangzhou", "kinpings", "merkel",
                                          "ashish", "putin", "tend", "farc", "weekend", "jacob", "sullum", "backgrounder",
                                          "sign", "macessarily", "sierra", "hong", "kong", "elizabeth", "warren", "niall",
                                          "german", "rapids", "mesmer", "silicon", "min", "kumar", "max", "fort", "lesley",
                                          "set", "immigrationand"))



### dems ###
dem_data <- partisan_data %>% filter(X1_if_dem == 1)

dem_words <- dem_data %>%
  unnest_tokens(word, Message) %>%
  anti_join(stop_words) %>%
  anti_join(immigration_stopwords)
dem_words

count_dem_words <- dem_words %>%
  count(word, sort = TRUE)
count_dem_words

joined_dem_words <- count_dem_words %>%
  inner_join(dem_words)
joined_dem_words

count_dem_words %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most Frequently Used Word: Democrats")

dem_sentiment <- dem_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = FALSE) %>%
  arrange(desc(value))
dem_sentiment

dem_sentiment %>%  
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  filter(!word == c("care", "support")) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentiment Value of Democrats's Top Words", caption = 
         "Contribution = number of word instances * sentiment value scaled from -5 to 5", 
       y = "contribution")

###bigrams
dem_bigrams <- dem_data %>%
  unnest_tokens(bigram, Message, token = "ngrams", n = 2)

sep_dem_bigrams <- dem_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

dem_bigrams_filtered <- sep_dem_bigrams %>%
  transmute(sep_dem_bigrams, word1 = gsub('[0-9]', '', sep_dem_bigrams$word1)) %>%
  filter(!word1 == ',') %>%
  filter(!word1 == '.')
dem_bigrams_filtered <- dem_bigrams_filtered %>%
  transmute(dem_bigrams_filtered, word2 = gsub('[0-9]', '', dem_bigrams_filtered$word2)) %>%
  filter(!word2 == ',') %>%
  filter(!word2 == '.')
dem_bigrams_filtered <- dem_bigrams_filtered %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% immigration_stopwords$word) %>%
  filter(!word2 %in% immigration_stopwords$word)
dem_bigrams_filtered 
 
dem_bigrams_counted <- dem_bigrams_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  filter(!word1 == "administration's") %>%
  filter(!word2 == "administration's") %>%
  filter(!word1 == "") %>%
  filter(!word2 == "") %>%
  inner_join(dem_bigrams_filtered)
dem_bigrams_counted 

dem_bigrams_counted %>%
  filter(word1 == "border") %>%
  filter(n > 5) %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot(aes(word2, n, fill = word2)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free_y") + 
  coord_flip() +
  labs(title = "Words Following 'Border': Democrats")

dem_bigrams <- dem_bigrams_counted %>%
  unite(bigram, word1, word2, sep = " ")

dem_bigrams %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most Frequently Used Bigrams: Democrats")

#word web
#bigram word web
dem_web <- dem_bigrams_counted %>%
  filter(n > 60) %>%
  graph_from_data_frame()
dem_web

set.seed(2017)
dem_web %>%
  ggraph(layout = "nicely") +
  geom_edge_link(aes(edge_color = state_abbr), show.legend = TRUE,
                 label_dodge = TRUE) +
  geom_node_point(color = "red") +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, 
                 repel = TRUE) +
  theme_void() +
  labs(title = "  Democrat's Bigram Word Map", 
       subtitle = "  Line color = State")  

### reps ###
rep_data <- partisan_data %>% filter(X1_if_dem == 0)

rep_words <- rep_data %>%
  unnest_tokens(word, Message) %>%
  anti_join(stop_words) %>%
  anti_join(immigration_stopwords)
rep_words         

count_rep_words <- rep_words %>%
  count(word, sort = TRUE)
count_rep_words

count_rep_words %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most Frequently Used Word: Republicans")

#sentiment
rep_sentiment <- rep_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE)
rep_sentiment

rep_sentiment %>%  
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  filter(!word == "support") %>%
  filter(!word == "care") %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentiment Value of Republican's Top Words", caption = 
         "Contribution = number of word instances * sentiment value scaled from -5 to 5", 
       y = "contribution")

#bigrams
rep_bigrams <- rep_data %>%
  unnest_tokens(bigram, Message, token = "ngrams", n = 2)
rep_bigrams <- rep_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

rep_bigrams_filtered <- rep_bigrams %>%
  transmute(rep_bigrams, word1 = gsub('[0-9]', '', rep_bigrams$word1)) %>%
  filter(!word1 == ',') %>%
  filter(!word1 == '.')
rep_bigrams_filtered <- rep_bigrams_filtered %>%
  transmute(rep_bigrams_filtered, word2 = gsub('[0-9]', '', rep_bigrams_filtered$word2)) %>%
  filter(!word2 == ',') %>%
  filter(!word2 == '.')
rep_bigrams_filtered <- rep_bigrams_filtered %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% immigration_stopwords$word) %>%
  filter(!word2 %in% immigration_stopwords$word)
rep_bigrams_filtered 

rep_bigrams_counted <- rep_bigrams_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  filter(!word1 == "") %>%
  filter(!word2 == "") %>%
  inner_join(rep_bigrams_filtered)
rep_bigrams_counted

rep_bigrams_counted %>%
  filter(word1 == "border") %>%
  filter(n > 20) %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot(aes(word2, n, fill = word2)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free_y") +
  coord_flip() +
  labs(title = "Words Following 'Border': Republicans")

#word web
rep_web <- rep_bigrams_counted %>%
  filter(n > 80) %>%
  graph_from_data_frame()
rep_web

set.seed(2016)
rep_web %>%
  ggraph(layout = "nicely") +
  geom_edge_link(aes(edge_color = state_abbr), show.legend = TRUE,
                 label_dodge = TRUE) +
  geom_node_point(color = "red") +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
  theme_void() +
  labs(title = "  Republican's Bigram Word Map",
       subtitle = "  Line color = State")

### tanks ###
tank_data <- read.csv("data/immigration_emails_april2020.csv")
tank_data = subset(tank_data, select = -c(raw_text, raw_text_no_links, url_links,
                                          email, heading, immigration_term1,
                                          immigration_term2, author, group_founding))

tank_words <- tank_data %>%
  unnest_tokens(word, text_no_punct) %>%
  anti_join(stop_words) %>%
  anti_join(immigration_stopwords)
tank_words

count_tank_words <- tank_words %>%
  count(word, sort = TRUE)
count_tank_words

count_tank_words %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most Frequently Used Word: Think Tanks")

#sentiment
tank_sentiment <- tank_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE)
tank_sentiment

tank_sentiment %>%  
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  filter(!word == "illegal") %>%
  filter(!word == "support") %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentiment Value of Think Tanks's Top Words", caption = 
         "Contribution = number of word instances * sentiment value scaled from -5 to 5", 
       y = "contribution")

#bigrams
tank_bigrams <- tank_data %>%
  unnest_tokens(bigram, text_no_punct, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")
tank_bigrams

tank_bigrams_filtered <- tank_bigrams %>%
  transmute(tank_bigrams, word1 = gsub('[0-9]', '', tank_bigrams$word1)) 
tank_bigrams_filtered <- tank_bigrams_filtered %>%
  transmute(tank_bigrams_filtered, word2 = gsub('[0-9]', '', tank_bigrams_filtered$word2)) 
tank_bigrams_filtered <- tank_bigrams_filtered %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% immigration_stopwords$word) %>%
  filter(!word2 %in% immigration_stopwords$word)

count_tank_bigrams <- tank_bigrams_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  filter(!word1 == "") %>%
  filter(!word2 == "") %>%
  inner_join(tank_bigrams_filtered) 
count_tank_bigrams

count_tank_bigrams %>%
  filter(word1 == "illegal") %>%
  filter(!word2 == "immigration") %>%
  top_n(15) %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot(aes(word2, n, fill = word2)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free_y") +
  coord_flip() +
  labs(title = "Words Following 'Illegal': Think Tanks")

tank_bigrams <- count_tank_bigrams %>%
  unite(bigram, word1, word2, sep = " ")
tank_bigrams

tank_bigrams %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  labs(title = "Most Frequently Used Bigrams: Think Tanks") 

#word web
tank_web <- count_tank_bigrams %>%
  top_n(150) %>%
  graph_from_data_frame()
tank_web

set.seed(2017)
tank_web %>%
  ggraph(layout = "nicely") +
  geom_edge_link(aes(color = "blue"), show.legend = FALSE, label_dodge = TRUE) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
  theme_void() +
  labs(title = "  Think Tank's Bigram Word Map")



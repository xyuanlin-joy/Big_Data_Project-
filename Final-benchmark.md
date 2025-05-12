Individual reliance on AI tools, specifically Chatgpt for
decision-making with the engagement of analytical thinking
================
Joy Lin
2025-05-11

# 1 RESEARCH PROJECT

## 1.1 Introduction

Co-chairing the AI Action Summit with French President Emmanuel Macron
in Paris on Feb 12th, Prime Minister Narendra Modi claimed that AI
(Artificial Intelligence) had already re-shaped people’s polity,
economy, security and even the society. The world now is at the “dawn of
the AI age” that relates to each individual(Nah et al. 2023). In school,
there is always a detailed section identifying AI usage policy,
including how to proper citation for AI usage and school’s specific AI
detection. Especially as one of the students majored in emerging media
studies, it doesn’t work if we don’t incorporate AI tools. Instead,
there is a need for figuring out how AI tools like ChatGPT affect
thinking patterns, which is crucial for designing AI technologies that
support an individual’s cognitive processing strategies and maintain
human intellectual capacities. This research aims to figure out to the
relationship between individual’s reliance on AI tools and their
decision-making progress.The dataset is collected from kaggle by the
researcher Khalid Ansari. The title of the data is called 500k
Chatgpt-related Tweets Jan-Mar 2023. The data gather users’ comments on
Twitter buzzing with discussions about conversational large language
models like Chatgpt, BARD, and Alpaca. The collected data spans from
January 4th, 2023 to March 29th, 2023, providing ample time to observe
daily, weekly, and quarterly trends. The dataset also includes
information related to Chatgpt including keywords (ChatGPT, chat GPT)
\#hashtags, and (**mentions?**) about ChatGPT and OpenAI’s
conversational AI model on 500,000 tweets.

### 1.1.1 Research Questions

My RQ is: : To what extent does reliance on AI tools, specifically
Chatgpt for decision-making reduce the engagement of analytical thinking
in the first quarter of 2023?

## 1.2 Methods

Step 1. Data Collection Step 2. Data Cleaning Step 3. Topic Modeling
(Top 20 words) Step 4. Sentiment Analysis Step 5. Visualizations

    ## [1] "/Users/joylim/Desktop/Study/trending insights/Assignment/Final_Data/Twitter Jan Mar.csv"

#### 1.2.0.1 Missing Data

It shows that no missing data in this dataset.

<div class="figure" style="text-align: center">

<img src="Final-benchmark_files/figure-gfm/unnamed-chunk-3-1.png" alt="Missing Data Visualization"  />
<p class="caption">
Missing Data Visualization
</p>

</div>

#### 1.2.0.2 Data Cleaning

    ## # A tibble: 6 × 9
    ##   date                     id username       like_count retweet_count post_index
    ##   <dttm>                <dbl> <chr>               <dbl>         <dbl>      <int>
    ## 1 2023-03-29 22:58:21 1.64e18 RealProfitPros          0             0          1
    ## 2 2023-03-29 22:58:21 1.64e18 RealProfitPros          0             0          1
    ## 3 2023-03-29 22:58:21 1.64e18 RealProfitPros          0             0          1
    ## 4 2023-03-29 22:58:21 1.64e18 RealProfitPros          0             0          1
    ## 5 2023-03-29 22:58:21 1.64e18 RealProfitPros          0             0          1
    ## 6 2023-03-29 22:58:21 1.64e18 RealProfitPros          0             0          1
    ## # ℹ 3 more variables: contentBU <chr>, word <chr>, nchar <int>

#### 1.2.0.3 Topic Modeling

After finishing the data cleaning, i use topic modeling to generate top
20 words of the posts (Blei, Ng, and Jordan 2003). Dominant words like
ai, gpt, chat, and openai indicates users pay attention on the core AI
technologies, suggesting a strong awareness and engagement with
conversational AI tools like ChatGPT and its creators. Words such as
write, asked, make, content, and language show how users interact with
AI for practical tasks, including content creation and communication.
The high frequency of action-oriented terms implies that users are not
just passive observers but active participants, using AI tools in
everyday decisions.

``` r
maxndoc=0.5
minndoc=0.00001
templength<-length(unique(tidy_data$post_index))
good_common_words <- tidy_data |> 
  count(post_index, word, sort = TRUE) |> 
  group_by(word) |> 
  summarize(doc_freq=n()/templength) |> 
  filter(doc_freq<maxndoc) |> 
  filter(doc_freq>minndoc)
tidy_data_pruned <- tidy_data |>  inner_join(good_common_words, by = "word")
tidy_data_pruned |> 
  group_by(word) |> 
  summarise(n=n()) |> 
  arrange(desc(n)) |> 
  mutate(word = reorder(word, n)) |> 
  top_n(20) |>     
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
```

<img src="Final-benchmark_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
#tidy_dfm <- tidy_data_pruned |> 
# count(post_index, word) |> 
# cast_dfm(post_index, word, n)
#tidy_dfm@Dim
#full_data<- convert(tidy_dfm, to = "topicmodels")
#rm(good_common_words, tidy_data, tidy_data_pruned, tidy_dfm, maxndoc, minndoc, templength)
#my computer can't run the number of topics, so i keep tidy data pruned top 20 words, and change to the sentiment analysis
#save.image("Upto_Findk.Rdata")
```

#### 1.2.0.4 Sentiment Analysis

    ## # A tibble: 2,477 × 2
    ##    word       value
    ##    <chr>      <dbl>
    ##  1 abandon       -2
    ##  2 abandoned     -2
    ##  3 abandons      -2
    ##  4 abducted      -2
    ##  5 abduction     -2
    ##  6 abductions    -2
    ##  7 abhor         -3
    ##  8 abhorred      -3
    ##  9 abhorrent     -3
    ## 10 abhors        -3
    ## # ℹ 2,467 more rows

#### 1.2.0.5 Sentiment Visualization

From the AFINN graph, it shows a wider range of positive and negative
intensity ranging from -40 to 20. Some spikes appear in the graph both
positively and negatively, which means contents posted with strong
emotion. From the BING and the NRC graph, they show fewer spikes with a
lower range from -10 to 10, which means a more balanced sentiment.
Compared to AFINN, smaller ranges appear likely due to fewer words being
classified.

<div class="figure" style="text-align: center">

<img src="Final-benchmark_files/figure-gfm/unnamed-chunk-7-1.png" alt="sentiment"  />
<p class="caption">
sentiment
</p>

</div>

I use BING to analyze top 10 words in user comments. In terms of
positive words, strong positive words,such as good and great,appear at
the top. This suggests that users perceive these conversational AI
models positively for their performance and accessibility. Frequent
words related to work efficiency, such as intelligence, prompt,
innovation, etc., reflect users appreciation for the AI’s creative
capabilities and users prefer using AI tools in their work. This also
implies that users increasingly reliance on AI models in professional or
personal decision-making processes, treating them as credible sources
for knowledge-intensive tasks. For negative words, wrong, bad, problem,
such dominant negative words show the unsatisfied outcome when users
apply with AI tools, which also means technical issues of AI tools still
need to be figured out. “hype,” “bias,” and “fake” highlight users
skepticism and concerns about the unrealistic expectations of AI tools
and bias in generated content. Users still keep their subjective
initiative using AI instead of complete dependence.

<div class="figure" style="text-align: center">

<img src="Final-benchmark_files/figure-gfm/unnamed-chunk-8-1.png" alt="BING-Top 20 words in posts"  />
<p class="caption">
BING-Top 20 words in posts
</p>

</div>

#### 1.2.0.6 Descriptive Data Visualizations

The mean number of likes is 7 that is relatively low, indicating most
tweets received little interaction. The max value of like reach to 64094
suggests significant skewness that most tweets have few or no likes,
while few tweets are extremely popular. Retweets are even less frequent
than likes, indicating even greater skewness. Majority of the tweets
have 0 retweets, highlighting a small scale of spread for most content,
yet a few highly influential tweets still exist.

``` r
#Descriptive statistics on likes and retweets
summary(Twitter_Jan_Mar2[, c("like_count", "retweet_count")])
```

    ##    like_count       retweet_count      
    ##  Min.   :    0.00   Min.   :    0.000  
    ##  1st Qu.:    0.00   1st Qu.:    0.000  
    ##  Median :    1.00   Median :    0.000  
    ##  Mean   :    6.96   Mean   :    1.446  
    ##  3rd Qu.:    2.00   3rd Qu.:    0.000  
    ##  Max.   :64094.00   Max.   :16080.000

``` r
st(Twitter_Jan_Mar2)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Summary Statistics
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:left;">
N
</th>
<th style="text-align:left;">
Mean
</th>
<th style="text-align:left;">
Std. Dev.
</th>
<th style="text-align:left;">
Min
</th>
<th style="text-align:left;">
Pctl. 25
</th>
<th style="text-align:left;">
Pctl. 75
</th>
<th style="text-align:left;">
Max
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
id
</td>
<td style="text-align:left;">
493733
</td>
<td style="text-align:left;">
1626965667607912704
</td>
<td style="text-align:left;">
8830164450695059
</td>
<td style="text-align:left;">
1610535734758219776
</td>
<td style="text-align:left;">
1619888453810016256
</td>
<td style="text-align:left;">
1635635181536292864
</td>
<td style="text-align:left;">
1641213230730051584
</td>
</tr>
<tr>
<td style="text-align:left;">
like_count
</td>
<td style="text-align:left;">
493733
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
209
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
64094
</td>
</tr>
<tr>
<td style="text-align:left;">
retweet_count
</td>
<td style="text-align:left;">
493733
</td>
<td style="text-align:left;">
1.4
</td>
<td style="text-align:left;">
44
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
16080
</td>
</tr>
<tr>
<td style="text-align:left;">
post_index
</td>
<td style="text-align:left;">
493733
</td>
<td style="text-align:left;">
249847
</td>
<td style="text-align:left;">
144407
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
124574
</td>
<td style="text-align:left;">
374977
</td>
<td style="text-align:left;">
500002
</td>
</tr>
</tbody>
</table>

The regression line shows that receiving a high number of likes does not
mean a tweet will also receive a high number of retweets. Users may
engage differently with content depending on different tweet
context.Tweets that relate to a timely and relevant topic might
accumulate more retweets to enrich the conversation while a personal or
emotional context could receive more likes as a form of support or
empathy. Tweets with high likes but fewer retweets may indicate AI
content users personally trust but hesitate to share publicly. In
addition, the differing engagement patterns might reflect users’
different information needs. Some users may primarily seek comfort or
validation (likes), while others are driven to spread novel insights
(retweets) relevant to their social decisions.

``` r
# Checking the number of unique values in each column
sapply(Twitter_Jan_Mar2, function(x) n_distinct(x))
```

    ##          date            id       content      username    like_count 
    ##        469805        493728        493733        247991          1047 
    ## retweet_count    post_index     contentBU 
    ##           481        493733        493733

``` r
# Regression line: Sort dataframe by like_count, highest to lowest
df_sorted <- Twitter_Jan_Mar2 |>
  arrange(desc(like_count))
head(df_sorted)
```

    ## # A tibble: 6 × 8
    ##   date                     id content          username like_count retweet_count
    ##   <dttm>                <dbl> <chr>            <chr>         <dbl>         <dbl>
    ## 1 2023-02-03 23:09:47 1.62e18 "Writing erotic… MoistCr…      64094          2624
    ## 2 2023-01-13 04:30:44 1.61e18 "Best AI Tools … johnvia…      63835         16080
    ## 3 2023-03-06 00:28:40 1.63e18 "I am pretty su… rgay          44940          1349
    ## 4 2023-01-07 04:31:47 1.61e18 "ultra-modern g… aaronsi…      42125         11501
    ## 5 2023-01-12 08:04:15 1.61e18 "First #ChatGPT… kevinsc…      38278          4160
    ## 6 2023-03-29 11:23:49 1.64e18 "Chat GPT revea… ProudFe…      28049          1686
    ## # ℹ 2 more variables: post_index <int>, contentBU <chr>

``` r
max_like_count = 2000
max_retweet_count = 500
ggplot(data = Twitter_Jan_Mar2, aes(x =like_count, y =retweet_count)) +
  geom_point(alpha = 5.) + #adjust the transparency of scatter points
  geom_smooth(method = "lm", se = FALSE, color = "blue") + #add a regression line
  labs(
    title = "Likes vs. Retweets",
    x = "Likes",
    y = "Retweets"
  ) +
  theme_minimal()
```

<img src="Final-benchmark_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

\####Timeline Analysis: Tweets per day Visualizing Tweet volume against
the dates will give the richer insights into user engagement(Tweet
volume) on ChatGPT based on major events surrounding the technology. In
general, tweet volume on ChatGPT is lowest on weekends approximately.
Tweet volumes remain relatively consistent on weekdays, reflecting
stable daily interactions or discussions around AI technology. The lower
engagement observed on weekends could reflect professional or
educational reliance suggesting that users interact with ChatGPT
primarily for professional or educational purposes during weekdays. It
is noticeable that there are two peaks that appear on February and March
respectively. This might be related to some events happened in 2023
during the same period. In Feb 2023, Chinese company Alibaba Group
announced that it was developing a rival to OpenAI’s ChatGPT AI chatbot.
Meanwhile, a study explored the potential of ChatGPT, a popular AI
chatbot, in generating academic essays that can evade plagiarism
detection tools, which triggered the discussion across the world
(**kingchang2024artificia?**). When it comes to March 14th 2023, OpenAI
announced GPT-4, the latest and most capable AI language model in its
line of language models at that time (Oca et al. 2023). Then in March 17
2023, after the release of GPT-4, the CEO of OpenAI was in an interview
with ABC News and said that AI technology would reshape society as we
know it, but that it came with real dangers (Tawfeeq, Awqati, and Jasim
2023). These events led to various thoughts and attitudes towards users,
which indirectly boosted the increasing tweets volumes.

``` r
Twitter_Jan_Mar2$date <- as.Date(Twitter_Jan_Mar2$date)
head(Twitter_Jan_Mar2$date)
```

    ## [1] "2023-03-29" "2023-03-29" "2023-03-29" "2023-03-29" "2023-03-29"
    ## [6] "2023-03-29"

``` r
tweets_by_day <- Twitter_Jan_Mar2 |>
  group_by(date)  |>
  summarise(count = n(), .groups = 'drop')
#Visualization
ggplot(tweets_by_day, aes(x = date, y = count)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(
    title = "Number of Tweets per Day",
    x = "Date",
    y = "Tweet Count"
  ) +
  theme_minimal()
```

<img src="Final-benchmark_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

## 1.3 Discussions

Topic modeling revealed a high frequency of terms like ai, gpt, openai,
and competitive mentions such as google, microsoft, etc. These indicate
widespread awareness and critical comparative discussions among users,
highlighting that decisions about reliance on AI tools are not
completely; instead, users actively compare and contrast different tools
based on capabilities,with major AI technologies. In addition, sentiment
analysis using the AFINN, BING, and NRC demonstrated the users’
perception toward AI tools. Users express positive sentiments associated
with words like “intelligence,” “innovation,” and “interesting,”
reflecting an appreciation for AI’s ability to enhance productivity and
efficiency. On the contrary, negative sentiment analysis revealed users’
concerns about information accuracy and ethical issues. This research
also finds that tweet volume correlated with key AI events and also
observed a steady increase in ChatGPT’s popularity since it’s release.
Analyzing tweet volume trends in a specific timeline clearly highlights
that user engagement with AI-related conversations varies significantly
across weekdays and weekends. Specifically, tweet volumes are
consistently lower during weekends, potentially indicating that
interactions with conversational AI tools such as Chatgpt are
academically oriented that correlate with typical weekday activities.
Two distinct peaks in tweet volume during February and March 2023 link
with great AI industry announcements. Such patterns clearly shows that
public discourse and user engagement are strongly event-driven,
influenced by developments that focusing on either technological
advancement or ethical concerns.

## 1.4 Limitations & Ethical Concerns/Data impact statement

Although this research give a basic topic modeling, the optimal topic
numbers haven’t been provided because of the capbility of researcher’s
computer and the large amount of dataset (Logan and Tandoc 2018).
Combined with the optimal topic numbers, a focused deep dive into LDA
Topic Modeling could give more insights. The specific dates of timeline
analysis is also lack, which could be improved by running the pipeline
for each month/week/day to do a more focused analysis. Besides, this
dataset gives an abundant data of various AI tools that provides
different angles for analyzing conversational AI. However, it may affect
the accuracy of analyzing Chatgpt’s running model separately (Wen et al.
2025).

## 1.5 References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-blei2003latent" class="csl-entry">

Blei, David M., Andrew Y. Ng, and Michael I. Jordan. 2003. “Latent
Dirichlet Allocation.” *Journal of Machine Learning Research* 3:
993–1022.

</div>

<div id="ref-logan2018thinking" class="csl-entry">

Logan, Robert K, and Marlie Tandoc. 2018. “Thinking in Patterns and the
Pattern of Human Thought as Contrasted with AI Data Processing.”
*Information* 9 (4): 83.

</div>

<div id="ref-fuihoon2023generativeai" class="csl-entry">

Nah, Fiona Fui-Hoon, Ruilin Zheng, Jingyuan Cai, Keng Siau, and Langtao
Chen. 2023. “Generative AI and ChatGPT: Applications, Challenges, and
AI-Human Collaboration.” *Journal of Information Technology Case and
Application Research* 25 (3): 277–304.
<https://doi.org/10.1080/15228053.2023.2233814>.

</div>

<div id="ref-oca2023bias" class="csl-entry">

Oca, Michael C, Leo Meller, Katherine Wilson, Alomi O Parikh, Allison
McCoy, Jessica Chang, Rasika Sudharshan, Shreya Gupta, and Sandy
Zhang-Nunes. 2023. “Bias and Inaccuracy in AI Chatbot Ophthalmologist
Recommendations.” *Cureus* 15 (9).

</div>

<div id="ref-tawfeeq2023ethical" class="csl-entry">

Tawfeeq, Tawfeeq Mokdada, Ali Jalal Awqati, and Yaser A Jasim. 2023.
“The Ethical Implications of Chatgpt Ai Chatbot: A Review.” *JMCER*
2023: 49–57.

</div>

<div id="ref-wen2025thinkpatterns" class="csl-entry">

Wen, Pengcheng, Jiaming Ji, Chi-Min Chan, Juntao Dai, Donghai Hong,
Yaodong Yang, Sirui Han, and Yike Guo. 2025. “ThinkPatterns-21k: A
Systematic Study on the Impact of Thinking Patterns in LLMs.” *arXiv
Preprint arXiv:2503.12918*.

</div>

</div>

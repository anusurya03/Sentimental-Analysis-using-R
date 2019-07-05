library(rvest)
library(stringr)
library(readr)
library(tm)
library(RSentiment)
library(wordcloud)
library(jpeg)
library(syuzhet)
library(XML)

print("HOSPITAL ANALYSIS")
hospital<-function()
{
  cat("\n\t\t CHOOSE THE CATEGORY FOR WHICH YOU WANT TREATMENT ")
  cat("\n\n\n 1. CANCER")
  cat("\n 2. TUBERCULOSIS")
  
  ch=scan()
  switch(ch,
         "1"=cancer(),
         "2"=tb())
}



#Function for finding out best hospital for treatment of CANCER
cancer<-function()
{
  cat("\n\t\t Top 5 HOSPITALS IN DELHI AND NCR FOR CANCER")
  cat("\n\n\n1. All India Institute of Medical Sciences AIIMS, (Govt. Hospital)")
  cat("\n2. Max Healthcare(Pitampura)")
  cat("\n3. Dharamshila Cancer Hospital")
  cat("\n4. BLK Super Speciality Hospital")
  cat("\n5. Rajiv Gandhi Cancer Institute and Research Centre")
  cat("\n6. Find Best Hospital for cancer")
  cat("\n\n Enter your choice:")
  ch1=scan()
  switch(ch1,
         "1"=aiimsp(),
         "2"=max_healthcare(),
         "3"=dharamshila(),
         "4"=blk(),
         "5"=rajiv_gandhi(),
         "6"=comparison1()
  )
  
}
aiimsp<-function()
{
  
  cat("Address: AIIMS Metro Station, Sri Aurobindo Marg, AIIMS Campus, Ansari Nagar East, New Delhi, Delhi 110029
      phone:097543 17910
      website:aiims.edu
      Open :24 hours")
  jj <- readJPEG("AIIMS-Delhi(jpeg).jpg",native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)
  aiims1<-read_html("http://www.mouthshut.com/product-reviews/AIIMS-Hospital-Ansari-Nagar-Delhi-reviews-925111460")
  
  
  reviews_1_1<-aiims1%>%html_nodes(".reviewdata")%>%html_text()
  write(reviews_1_1, "aiims.txt", sep=" ")  #making a text file for reviews of AIIMS Hospital
  aiims2=readLines("aiims.txt ")
  #DATA CLEANING
  data_cnv(aiims2) #calling data cleaning function
  
}

max_healthcare<-function()
{
  cat("Address: HB Twin Tower, Near TV Tower, Wazirpur District Centre, Pitampura, New Delhi, Delhi 110034
      Phone: 011 4735 1844
      Hours: 
      Saturday	8AM-8PM
      Sunday	Closed
      Monday	8AM-8PM
      Tuesday	8AM-8PM
      Wednesday	8AM-8PM
      Thursday	8AM-8PM
      Friday	8AM-8PM
      Website-www.maxhealthcare.in")
  jj <- readJPEG("max_healthcare.jpg",native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)
  max_healthcare1<-read_html("http://www.mouthshut.com/product-reviews/Max-Hospital-Pitampura-Delhi-reviews-925095904")
  reviews_1_2<-max_healthcare1%>%html_nodes(".reviewdata")%>%html_text()
  write(reviews_1_2, "max_healthcare.txt", sep=" ")
  max_healthcare2=readLines("max_healthcare.txt ")
  #DATA CLEANING
  data_cnv(max_healthcare2) #calling data cleaning function
}

dharamshila<-function()
{
  cat("Address: Dharamshila Marg, Vasundhara Enclave, Near New Ashok Nagar Metro Station, New Delhi, Delhi 110096
      Phone:1860 208 0208
      Website-www.narayanahealth.or")
  jj <- readJPEG("dharamshila(jpeg).jpg",native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)
  dharamshila1<-read_html("http://www.mouthshut.com/product-reviews/Dharamshila-Cancer-Hospital-Delhi-reviews-925020806")
  reviews_1_3<-dharamshila1%>%html_nodes(".reviewdata")%>%html_text()
  write(reviews_1_3, "dharamshila.txt", sep=" ")
  dharamshila2=readLines("dharamshila.txt ")
  #DATA CLEANING
  data_cnv(dharamshila2) #calling data cleaning function
}

blk<-function()
{
  cat("Address: Building No-5, Pusa Road, Rajinder Nagar, New Delhi, Delhi 110005
      Phone:011 3040 3040
      Hours: 
      Sunday	Open 24 hours
      Monday	Open 24 hours
      Tuesday	Open 24 hours
      Wednesday	Open 24 hours
      Thursday	Open 24 hours
      Friday	Open 24 hours
      Saturday	Open 24 hours
      Website-http://www.blkhospital.com/")
  jj <- readJPEG("BLK_Super_Specialty_Hospital.jpg",native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)
  blk1<-read_html("http://www.mouthshut.com/product-reviews/BLK-Super-Speciality-Hospital-Delhi-reviews-925667106")
  reviews_1_4<-blk1%>%html_nodes(".reviewdata")%>%html_text()
  write(reviews_1_4, "blk.txt", sep=" ")
  blk2=readLines("blk.txt ")
  #DATA CLEANING
  data_cnv(blk2) #calling data cleaning function
}

rajiv_gandhi<-function()
{
  cat("Address: Sector 5, Rohini, Delhi, 110085
      Phone:011 4702 2222
      Hours: 
      Sunday	Closed
      Monday	9AM-5PM
      Tuesday	9AM-5PM
      Wednesday	9AM-5PM
      Thursday	9AM-5PM
      Friday	9AM-5PM
      Saturday	9AM-5PM
      Website-http://www.rgcirc.org/")
  jj <- readJPEG("rajiv-gandhi.jpg",native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)
  rajiv_gandhi1<-read_html("http://www.mouthshut.com/product-reviews/Rajiv-Gandhi-Cancer-Institute-and-Research-Center-Delhi-reviews-925719854")
  reviews_1_5<-rajiv_gandhi1%>%html_nodes(".reviewdata")%>%html_text()
  write(reviews_1_5, " rajiv_gandhi.txt", sep=" ")
  rajiv_gandhi2 =readLines(" rajiv_gandhi.txt ")
  #DATA CLEANING 
  data_cnv( rajiv_gandhi2) #calling data cleaning function
  
}




#Function for finding out best hospital for treatment of TUBERCULOSIS
tb<-function()
{
  cat("\n\t\t Top 5 HOSPITALS IN DELHI AND NCR FOR TUBERCULOSIS")
  cat("\n\n\n1. Rajan Babu Tuberculosis Hospital")
  cat("\n2. National Institute of Tuberculosis and Respiratory Diseases.")
  cat("\n3. Max Super Speciality Hospital(Vaishali)")
  cat("\n4. Yashoda Super Speciality Hospital")
  cat("\n5. Kukreja Hospital(Mayur Vihar)")
  cat("\n6. Find Best Hospital for Tuberculosis")
  cat("\n\n Enter your choice:")
  ch2=scan()
  switch(ch2,
         "1"=rajan_babu(),
         "2"=NITRD(),
         "3"=max_super(),
         "4"=yashoda(),
         "5"=kukreja(),
         "6"=comparison2()
  )
  
}
rajan_babu<-function()
{
  cat("Address-Mahatma Gandhi Marg, Tagore Park Extension, GTB Nager, Delhi, 110009
      Hours: 
      Saturday	9AM-6PM
      Sunday	Closed
      Monday	9AM-6PM
      Tuesday	9AM-6PM
      Wednesday	9AM-6PM
      Thursday	9AM-6PM
      Friday	9AM-6PM
      Website-https://www.sulekha.com")
  jj <- readJPEG("rajan_babu.jpg",native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)
  rajan_babu1<-read_html("http://www.mouthshut.com/product-reviews/Rajan-Babu-Tuberculosis-Hospital-Kingsway-Camp-Delhi-reviews-925111494")
  reviews_2_1<-rajan_babu1%>%html_nodes(".reviewdata")%>%html_text()
  write(reviews_2_1, "rajan_babu.txt", sep=" ")
  rajan_babu2=readLines("rajan_babu.txt ")
  #DATA CLEANING
  data_cnv(rajan_babu2) #calling data cleaning function
  
}

NITRD<-function()
{
  cat("Address:Sri Aurobindo Marg, Near Qutub Minar, Mehrauli, Seth Sarai, Mehrauli, New Delhi, Delhi 110030
      website :nitrd.gov
      phone   :011 2651 7826
      Open now:  Open 24 hours
      ")
  jj <- readJPEG("nitrd.jpg",native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)
  NITRD1<-read_html("http://www.mouthshut.com/product-reviews/L-R-S-Institute-of-Tuberculosis-and-Respiratory-Diseases-New-Delhi-reviews-925723389")
  #url2<-read_html("https://www.justdial.com/Delhi-NCR/National-Institute-Of-Tuberculosis-Respiratory-Diseases-Near-Qutub-Minar-Mehrauli/011PXX11-XX11-000453219696-H3Q3_BZDET")
  reviews_2_2<-NITRD1%>%html_nodes(".reviewdata")%>%html_text()
  write(reviews_2_2, "NITRD.txt", sep=" ")
  NITRD2=readLines("NITRD.txt ")
  #DATA CLEANING
  data_cnv(NITRD2) #calling data cleaning function
}

max_super<-function()
{ 
  cat("Address: W-3, Near Radisson Blu Hotel, Sector 1, Vaishali, Ghaziabad, Uttar Pradesh 201012
      Phone: 0120 418 8000
      Hours: 
      Sunday	10AM-1PM
      Monday	9AM-8PM
      Tuesday	9AM-8PM
      Wednesday	9AM-8PM
      Thursday	9AM-8PM
      Friday	9AM-8PM
      Saturday	9AM-8PM
      Website-https://www.maxhealthcare.in")
  jj <- readJPEG("Max-Super(1).jpg",native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)
  #url3<-read_html("https://www.justdial.com/Delhi-NCR/Max-Super-Speciality-Hospital-Near-AEZ-Mall-Dabur-Chowk-Vaishali-Sector-1/011PXX11-XX11-100319095318-D2I3_BZDET") 
  max_super1<-read_html("http://www.mouthshut.com/product-reviews/Max-Super-Speciality-Hospital-Vaishali-Ghaziabad-reviews-925881244")
  reviews_2_3<-max_super1%>%html_nodes(".reviewdata")%>%html_text()
  write(reviews_2_3, "max_super.txt", sep=" ")
  max_super2=readLines("max_super.txt ")
  #DATA CLEANING
  data_cnv(max_super2) #calling data cleaning function
}

yashoda<-function()
{ 
  cat("Address- Nehru Nagar III, Near Nehru Stadium Jogging Track, Ghaziabad, Uttar Pradesh 201001
      Phone: 0120 418 2000
      Website-http://www.yashodahospital.org/")
  jj <- readJPEG("yashoda.jpg",native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)
  yashoda1<-read_html("http://www.mouthshut.com/product-reviews/Yashoda-Super-Speciality-Hospital-Ghaziabad-reviews-925096055")
  reviews_2_4<-yashoda1%>%html_nodes(".reviewdata")%>%html_text()
  write(reviews_2_4, "yashoda.txt", sep=" ")
  yashoda2=readLines("yashoda.txt ")
  #DATA CLEANING
  data_cnv(yashoda2) #calling data cleaning function
}

kukreja<-function()
{  
  cat("Address-D-36, Block C, Acharya Niketan, Mayur Vihar Phase 1, New Delhi, Delhi 110091
      Phone: 011 2275 3123
      Hours: 
      Sunday	Open 24 hours
      Monday	Open 24 hours
      Tuesday	Open 24 hours
      Wednesday	Open 24 hours
      Thursday	Open 24 hours
      Friday	Open 24 hours
      Saturday	Open 24 hours
      Website-http://www.kukrejahospitaldelhi.com/")
  jj <- readJPEG("Kukreja-Hospital.jpg",native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)
  kukreja1<-read_html("http://www.mouthshut.com/product-reviews/Kukreja-Hospital-Mayur-Vihar-Phase-I-Delhi-reviews-925861384")
  reviews_2_5<-kukreja1%>%html_nodes(".reviewdata")%>%html_text()
  write(reviews_2_5, "kukreja.txt", sep=" ")
  kukreja2=readLines("kukreja.txt ")
  #DATA CLEANING 
  data_cnv(kukreja2) #calling data cleaning function
  
}






#Function for data cleaning
data_cnv<-function(t)
{  
  #Convert multiple Lines into One Text
  text1 = paste(t,collapse = "")
  #1. Remove Puntuations
  text1 = gsub(pattern="\\W", replace = " ", text1)
  #2. Remove Numbers or Digits
  text1 = gsub(pattern="\\d", replace = " ", text1)
  #3. Convert entire text to lowercase
  text1 = tolower(text1)
  #5. Removing Certain Words(StopWords)
  #stopwords()
  text1 = removeWords(text1,stopwords())
  #6. Removing Words of Certain Length
  text1 = gsub(pattern="\\b[A-z]\\b{1}", replace=" ", text1)
  #7. Cleaning up all White Space
  text1 = stripWhitespace(text1)
  #Sentiment Analysis
  
  #S+ One or More Space
  tb = str_split(text1, pattern="\\s+")
  tb1 = unlist(tb)
  cat("\n \n")
  #Creating WordCloud
  wordcloud(tb1,min.freq = 1,random.order = FALSE,scale=c(3,0.5),color = rainbow(3))
  #to lower
  text1=tolower(text1)
  #cat("\n text1=",text1)
  
  
  #Calculating Polarity and Sentiment Score
  g_score = get_nrc_sentiment(text1)
  #cat("\n\n",g_score)
  #g_polarity = g_score[1,9:10]
  #cat("\n\n",g_polarity)
  g_polarity = g_score[1,9:10]
  g_polarity = data.matrix(g_polarity, rownames.force = TRUE)
  print(g_polarity)
 
  g_sentiment = g_score[1,1:8]
  g_sentiment = data.matrix(g_sentiment,rownames.force = TRUE)
  print(g_sentiment)
  
  old.par <- par(mfcol=c(1, 3))
  
  #Visualizing a WordCloud
  wordcloud(tb1,min.freq = 1,random.order = FALSE,scale=c(3,0.5),color = rainbow(3))
  #Visualize Polarity and Sentiment
  barplot(g_polarity,col ='lightblue')
  barplot(g_sentiment,col = 'green')
  par(old.par)
}

comparison1<-function()
{ 
  c_1<-readLines("aiims.txt")
  c_1
  gscore1<-get_nrc_sentiment(c_1)
  p<-gscore1[1,10]
  p
  c_2<-readLines("max_healthcare.txt")
  c_2
  gscore2<-get_nrc_sentiment(c_2)
  q<-gscore2[1,10]
  p<-append(p,q)
  c_3<-readLines("dharamshila.txt")
  c_3
  gscore3<-get_nrc_sentiment(c_3)
  q<-gscore3[1,10]
  p<-append(p,q)
  c_4<-readLines("blk.txt")
  c_4
  gscore4<-get_nrc_sentiment(c_4)
  q<-gscore4[1,10]
  p<-append(p,q)
  c_5<-readLines("rajiv_gandhi.txt")
  c_5
  gscore5<-get_nrc_sentiment(c_5)
  q<-gscore5[1,10]
  p<-append(p,q)
  p
  
  ch<-which.max(p)
  ch
  cat("\n BEST HOSPITAL IN DELHI IS")
  switch(ch,
         "1"=cat("\n\n All India Institute of Medical Sciences AIIMS, (Govt. Hospital)\n\n",aiimsp()),
         "2"=cat("\n\nMax Healthcare(Pitampura)\n\n",max_healthcare()),
         "3"=cat("\n\nDharamshila Cancer Hospital\n\n",dharamshila()),
         "4"=cat("\n\nBLK Super Speciality Hospital\n\n",blk()),
         "5"=cat("\n\nRajiv Gandhi Cancer Institute and Research Centre\n\n",rajiv_gandhi()))
}

comparison2<-function()
{ 
  c_1<-readLines("rajan_babu.txt")
  c_1
  gscore1<-get_nrc_sentiment(c_1)
  p<-gscore1[1,10]
  p
  c_2<-readLines("NITRD.txt")
  c_2
  gscore2<-get_nrc_sentiment(c_2)
  q<-gscore2[1,10]
  p<-append(p,q)
  c_3<-readLines("max_super.txt")
  c_3
  gscore3<-get_nrc_sentiment(c_3)
  q<-gscore3[1,10]
  p<-append(p,q)
  c_4<-readLines("yashoda.txt")
  c_4
  gscore4<-get_nrc_sentiment(c_4)
  q<-gscore4[1,10]
  p<-append(p,q)
  c_5<-readLines("kukreja.txt")
  c_5
  gscore5<-get_nrc_sentiment(c_5)
  q<-gscore5[1,10]
  p<-append(p,q)
  p
  
  ch<-which.max(p)
  ch
  cat("\n BEST HOSPITAL IN DELHI IS")
  switch(ch,
         "1"=cat(" \n\nRajan Babu Tuberculosis Hospital\n\n",rajan_babu()),
         "2"=cat(" \n\nNational Institute of Tuberculosis and Respiratory Diseases\n\n",NITRD()),
         "3"=cat(" \n\nMax Super Speciality Hospital(Vaishali)\n\n",max_super()),
         "4"=cat(" \n\nYashoda Super Speciality Hospital\n\n",yashoda()),
         "5"=cat(" \n\nKukreja Hospital(Mayur Vihar)\n\n",kukreja())
         )
         
}
1
kukreja()

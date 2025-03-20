# Voici le fichier pour le cleaning des questions tactiques


# tactical_social_media -----------------------------------------------
#### Which social media do you use the most? 

table(DataRaw$tactical_social_media)
DataClean$tactical_socialMedia <- NA
DataClean$tactical_socialMedia[DataRaw$tactical_social_media == "Facebook"] <- "facebook"
DataClean$tactical_socialMedia[DataRaw$tactical_social_media == "Instagram"] <- "instagram"
DataClean$tactical_socialMedia[DataRaw$tactical_social_media == "LinkedIn"] <- "linkedin"
DataClean$tactical_socialMedia[DataRaw$tactical_social_media == "Pinterest"] <- "pinterest"
DataClean$tactical_socialMedia[DataRaw$tactical_social_media == "Snapchat"] <- "snapchat"
DataClean$tactical_socialMedia[DataRaw$tactical_social_media == "TikTok"] <- "tiktok"
DataClean$tactical_socialMedia[DataRaw$tactical_social_media == "Twitter / X"] <- "x"
DataClean$tactical_socialMedia[DataRaw$tactical_social_media == "YouTube"] <- "youtube"
DataClean$tactical_socialMedia[DataRaw$tactical_social_media == "Other"] <- "other"
DataClean$tactical_socialMedia <- factor(DataClean$tactical_socialMedia,
                                                   levels = c("facebook",
                                                              "instagram",
                                                              "linkedin",
                                                              "pinterest",
                                                              "snapchat",
                                                              "tiktok",
                                                              "x",
                                                              "youtube",
                                                              "other"),
                                                   ordered = TRUE)
table(DataClean$tactical_socialMedia)

# tactical_socialMedia bin

DataClean$tactical_socialMediaFacebook <- NA
DataClean$tactical_socialMediaFacebook[DataRaw$tactical_social_media == "Facebook"] <- 1
DataClean$tactical_socialMediaFacebook[DataRaw$tactical_social_media != "Facebook"] <- 0
table(DataClean$tactical_socialMediaFacebook)

DataClean$tactical_socialMediaInstagram <- NA
DataClean$tactical_socialMediaInstagram[DataRaw$tactical_social_media == "Instagram"] <- 1
DataClean$tactical_socialMediaInstagram[DataRaw$tactical_social_media != "Instagram"] <- 0
table(DataClean$tactical_socialMediaInstagram)

DataClean$tactical_socialMediaLinkedIn <- NA
DataClean$tactical_socialMediaLinkedIn[DataRaw$tactical_social_media == "LinkedIn"] <- 1
DataClean$tactical_socialMediaLinkedIn[DataRaw$tactical_social_media != "LinkedIn"] <- 0
table(DataClean$tactical_socialMediaLinkedIn)

DataClean$tactical_socialMediaPinterest <- NA
DataClean$tactical_socialMediaPinterest[DataRaw$tactical_social_media == "Pinterest"] <- 1
DataClean$tactical_socialMediaPinterest[DataRaw$tactical_social_media != "Pinterest"] <- 0
table(DataClean$tactical_socialMediaPinterest)

DataClean$tactical_socialMediaSnapchat <- NA
DataClean$tactical_socialMediaSnapchat[DataRaw$tactical_social_media == "Snapchat"] <- 1
DataClean$tactical_socialMediaSnapchat[DataRaw$tactical_social_media != "Snapchat"] <- 0
table(DataClean$tactical_socialMediaSnapchat)

DataClean$tactical_socialMediaTikTok <- NA
DataClean$tactical_socialMediaTikTok[DataRaw$tactical_social_media == "TikTok"] <- 1
DataClean$tactical_socialMediaTikTok[DataRaw$tactical_social_media != "TikTok"] <- 0
table(DataClean$tactical_socialMediaTikTok)

DataClean$tactical_socialMediaX <- NA
DataClean$tactical_socialMediaX[DataRaw$tactical_social_media == "Twitter / X"] <- 1
DataClean$tactical_socialMediaX[DataRaw$tactical_social_media != "Twitter / X"] <- 0
table(DataClean$tactical_socialMediaX)

DataClean$tactical_socialMediaYouTube <- NA
DataClean$tactical_socialMediaYouTube[DataRaw$tactical_social_media == "YouTube"] <- 1
DataClean$tactical_socialMediaYouTube[DataRaw$tactical_social_media != "YouTube"] <- 0
table(DataClean$tactical_socialMediaYouTube)

DataClean$tactical_socialMediaOther <- NA
DataClean$tactical_socialMediaOther[DataRaw$tactical_social_media == "Other"] <- 1
DataClean$tactical_socialMediaOther[DataRaw$tactical_social_media != "Other"] <- 0
table(DataClean$tactical_socialMediaOther)


# tactical_political_news_source ------------------------------------------
#### Which of the following sources do you primarily consult to stay informed about politics?

table(DataRaw$tactical_political_news_source)
DataClean$tactical_politicalNewsSource <- NA
DataClean$tactical_politicalNewsSource[DataRaw$tactical_political_news_source == "Alternative and/or independent media"] <- "alternativeIndependentMedia"
DataClean$tactical_politicalNewsSource[DataRaw$tactical_political_news_source == "Newspapers"] <- "newspapers"
DataClean$tactical_politicalNewsSource[DataRaw$tactical_political_news_source == "None of these"] <- "none"
DataClean$tactical_politicalNewsSource[DataRaw$tactical_political_news_source == "Online news websites"] <- "onlineNewsWebsites"
DataClean$tactical_politicalNewsSource[DataRaw$tactical_political_news_source == "Other source"] <- "other"
DataClean$tactical_politicalNewsSource[DataRaw$tactical_political_news_source == "Radio"] <- "radio"
DataClean$tactical_politicalNewsSource[DataRaw$tactical_political_news_source == "Social media (X, Instagram, Reddit, etc.)"] <- "socialMedia"
DataClean$tactical_politicalNewsSource[DataRaw$tactical_political_news_source == "Television"] <- "television"
DataClean$tactical_politicalNewsSource[DataRaw$tactical_political_news_source == "Word of mouth (discussions with family, friends, etc.)"] <- "discussions"
DataClean$tactical_politicalNewsSource[DataRaw$tactical_political_news_source == "YouTube"] <- "youtube"
DataClean$tactical_politicalNewsSource <- factor(DataClean$tactical_politicalNewsSource,
                                                 levels = c("alternativeIndependentMedia",
                                                            "newspapers",
                                                            "none",
                                                            "onlineNewsWebsites",
                                                            "other",
                                                            "radio",
                                                            "socialMedia",
                                                            "television",
                                                            "discussions",
                                                            "youtube"),
                                                 ordered = TRUE)
table(DataClean$tactical_politicalNewsSource)

# tactical_politicalNewsSource bin

DataClean$tactical_politicalNewsSourceAlternativeIndependentMedia <- NA
DataClean$tactical_politicalNewsSourceAlternativeIndependentMedia[DataRaw$tactical_political_news_source == "Alternative and/or independent media"] <- 1
DataClean$tactical_politicalNewsSourceAlternativeIndependentMedia[DataRaw$tactical_political_news_source != "Alternative and/or independent media"] <- 0
table(DataClean$tactical_politicalNewsSourceAlternativeIndependentMedia)

DataClean$tactical_politicalNewsSourceNewspapers <- NA
DataClean$tactical_politicalNewsSourceNewspapers[DataRaw$tactical_political_news_source == "Newspapers"] <- 1
DataClean$tactical_politicalNewsSourceNewspapers[DataRaw$tactical_political_news_source != "Newspapers"] <- 0
table(DataClean$tactical_politicalNewsSourceNewspapers)
  
DataClean$tactical_politicalNewsSourceNone <- NA
DataClean$tactical_politicalNewsSourceNone[DataRaw$tactical_political_news_source == "None of these"] <- 1
DataClean$tactical_politicalNewsSourceNone[DataRaw$tactical_political_news_source != "None of these"] <- 0
table(DataClean$tactical_politicalNewsSourceNone)

DataClean$tactical_politicalNewsSourceOnlineNewsWebsites <- NA
DataClean$tactical_politicalNewsSourceOnlineNewsWebsites[DataRaw$tactical_political_news_source == "Online news websites"] <- 1
DataClean$tactical_politicalNewsSourceOnlineNewsWebsites[DataRaw$tactical_political_news_source != "Online news websites"] <- 0
table(DataClean$tactical_politicalNewsSourceOnlineNewsWebsites)

DataClean$tactical_politicalNewsSourceOther <- NA
DataClean$tactical_politicalNewsSourceOther[DataRaw$tactical_political_news_source == "Other source"] <- 1
DataClean$tactical_politicalNewsSourceOther[DataRaw$tactical_political_news_source != "Other source"] <- 0
table(DataClean$tactical_socialMediaOther)

DataClean$tactical_politicalNewsSourceRadio <- NA
DataClean$tactical_politicalNewsSourceRadio[DataRaw$tactical_political_news_source == "Radio"] <- 1
DataClean$tactical_politicalNewsSourceRadio[DataRaw$tactical_political_news_source != "Radio"] <- 0
table(DataClean$tactical_politicalNewsSourceRadio)

DataClean$tactical_politicalNewsSourceSocialMedia <- NA
DataClean$tactical_politicalNewsSourceSocialMedia[DataRaw$tactical_political_news_source == "Social media (X, Instagram, Reddit, etc.)"] <- 1
DataClean$tactical_politicalNewsSourceSocialMedia[DataRaw$tactical_political_news_source != "Social media (X, Instagram, Reddit, etc.)"] <- 0
table(DataClean$tactical_politicalNewsSourceSocialMedia)

DataClean$tactical_politicalNewsSourceTelevision <- NA
DataClean$tactical_politicalNewsSourceTelevision[DataRaw$tactical_political_news_source == "Television"] <- 1
DataClean$tactical_politicalNewsSourceTelevision[DataRaw$tactical_political_news_source != "Television"] <- 0
table(DataClean$tactical_politicalNewsSourceTelevision)

DataClean$tactical_politicalNewsSourceDiscussions <- NA
DataClean$tactical_politicalNewsSourceDiscussions[DataRaw$tactical_political_news_source == "Word of mouth (discussions with family, friends, etc.)"] <- 1
DataClean$tactical_politicalNewsSourceDiscussions[DataRaw$tactical_political_news_source != "Word of mouth (discussions with family, friends, etc.)"] <- 0
table(DataClean$tactical_politicalNewsSourceTelevision)

DataClean$tactical_politicalNewsSourceYouTube <- NA
DataClean$tactical_politicalNewsSourceYouTube[DataRaw$tactical_political_news_source == "YouTube"] <- 1
DataClean$tactical_politicalNewsSourceYouTube[DataRaw$tactical_political_news_source != "YouTube"] <- 0
table(DataClean$tactical_politicalNewsSourceYouTube)



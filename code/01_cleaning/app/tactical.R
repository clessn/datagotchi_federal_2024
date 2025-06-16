# Voici le fichier pour le cleaning des questions tactiques


# tactical_social_media -----------------------------------------------
#### Which social media do you use the most? 

table(DataRaw$tactical_social_media)
DataClean$tactical_socialMedia_factor <- NA
DataClean$tactical_socialMedia_factor[DataRaw$tactical_social_media == "Facebook"] <- "facebook"
DataClean$tactical_socialMedia_factor[DataRaw$tactical_social_media == "Instagram"] <- "instagram"
DataClean$tactical_socialMedia_factor[DataRaw$tactical_social_media == "LinkedIn"] <- "linkedin"
DataClean$tactical_socialMedia_factor[DataRaw$tactical_social_media == "Pinterest"] <- "pinterest"
DataClean$tactical_socialMedia_factor[DataRaw$tactical_social_media == "Snapchat"] <- "snapchat"
DataClean$tactical_socialMedia_factor[DataRaw$tactical_social_media == "TikTok"] <- "tiktok"
DataClean$tactical_socialMedia_factor[DataRaw$tactical_social_media == "Twitter / X"] <- "x"
DataClean$tactical_socialMedia_factor[DataRaw$tactical_social_media == "YouTube"] <- "youtube"
DataClean$tactical_socialMedia_factor[DataRaw$tactical_social_media == "Other"] <- "other"
DataClean$tactical_socialMedia_factor <- factor(DataClean$tactical_socialMedia_factor,
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
table(DataClean$tactical_socialMedia_factor)

# tactical_socialMedia bin

DataClean$tactical_socialMediaFacebook_bin <- NA
DataClean$tactical_socialMediaFacebook_bin[DataRaw$tactical_social_media == "Facebook"] <- 1
DataClean$tactical_socialMediaFacebook_bin[DataRaw$tactical_social_media != "Facebook"] <- 0
table(DataClean$tactical_socialMediaFacebook_bin)

DataClean$tactical_socialMediaInstagram_bin <- NA
DataClean$tactical_socialMediaInstagram_bin[DataRaw$tactical_social_media == "Instagram"] <- 1
DataClean$tactical_socialMediaInstagram_bin[DataRaw$tactical_social_media != "Instagram"] <- 0
table(DataClean$tactical_socialMediaInstagram_bin)

DataClean$tactical_socialMediaLinkedIn_bin <- NA
DataClean$tactical_socialMediaLinkedIn_bin[DataRaw$tactical_social_media == "LinkedIn"] <- 1
DataClean$tactical_socialMediaLinkedIn_bin[DataRaw$tactical_social_media != "LinkedIn"] <- 0
table(DataClean$tactical_socialMediaLinkedIn_bin)

DataClean$tactical_socialMediaPinterest_bin <- NA
DataClean$tactical_socialMediaPinterest_bin[DataRaw$tactical_social_media == "Pinterest"] <- 1
DataClean$tactical_socialMediaPinterest_bin[DataRaw$tactical_social_media != "Pinterest"] <- 0
table(DataClean$tactical_socialMediaPinterest_bin)

DataClean$tactical_socialMediaSnapchat_bin <- NA
DataClean$tactical_socialMediaSnapchat_bin[DataRaw$tactical_social_media == "Snapchat"] <- 1
DataClean$tactical_socialMediaSnapchat_bin[DataRaw$tactical_social_media != "Snapchat"] <- 0
table(DataClean$tactical_socialMediaSnapchat_bin)

DataClean$tactical_socialMediaTikTok_bin <- NA
DataClean$tactical_socialMediaTikTok_bin[DataRaw$tactical_social_media == "TikTok"] <- 1
DataClean$tactical_socialMediaTikTok_bin[DataRaw$tactical_social_media != "TikTok"] <- 0
table(DataClean$tactical_socialMediaTikTok_bin)

DataClean$tactical_socialMediaX_bin <- NA
DataClean$tactical_socialMediaX_bin[DataRaw$tactical_social_media == "Twitter / X"] <- 1
DataClean$tactical_socialMediaX_bin[DataRaw$tactical_social_media != "Twitter / X"] <- 0
table(DataClean$tactical_socialMediaX_bin)

DataClean$tactical_socialMediaYouTube_bin <- NA
DataClean$tactical_socialMediaYouTube_bin[DataRaw$tactical_social_media == "YouTube"] <- 1
DataClean$tactical_socialMediaYouTube_bin[DataRaw$tactical_social_media != "YouTube"] <- 0
table(DataClean$tactical_socialMediaYouTube_bin)

DataClean$tactical_socialMediaOther_bin <- NA
DataClean$tactical_socialMediaOther_bin[DataRaw$tactical_social_media == "Other"] <- 1
DataClean$tactical_socialMediaOther_bin[DataRaw$tactical_social_media != "Other"] <- 0
table(DataClean$tactical_socialMediaOther_bin)


# tactical_political_news_source ------------------------------------------
#### Which of the following sources do you primarily consult to stay informed about politics?

table(DataRaw$tactical_political_news_source)
DataClean$tactical_politicalNewsSource_factor <- NA
DataClean$tactical_politicalNewsSource_factor[DataRaw$tactical_political_news_source == "Alternative and/or independent media"] <- "alternativeIndependentMedia"
DataClean$tactical_politicalNewsSource_factor[DataRaw$tactical_political_news_source == "Newspapers"] <- "newspapers"
DataClean$tactical_politicalNewsSource_factor[DataRaw$tactical_political_news_source == "None of these"] <- "none"
DataClean$tactical_politicalNewsSource_factor[DataRaw$tactical_political_news_source == "Online news websites"] <- "onlineNewsWebsites"
DataClean$tactical_politicalNewsSource_factor[DataRaw$tactical_political_news_source == "Other source"] <- "other"
DataClean$tactical_politicalNewsSource_factor[DataRaw$tactical_political_news_source == "Radio"] <- "radio"
DataClean$tactical_politicalNewsSource_factor[DataRaw$tactical_political_news_source == "Social media (X, Instagram, Reddit, etc.)"] <- "socialMedia"
DataClean$tactical_politicalNewsSource_factor[DataRaw$tactical_political_news_source == "Television"] <- "television"
DataClean$tactical_politicalNewsSource_factor[DataRaw$tactical_political_news_source == "Word of mouth (discussions with family, friends, etc.)"] <- "discussions"
DataClean$tactical_politicalNewsSource_factor[DataRaw$tactical_political_news_source == "YouTube"] <- "youtube"
DataClean$tactical_politicalNewsSource_factor <- factor(DataClean$tactical_politicalNewsSource_factor,
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
table(DataClean$tactical_politicalNewsSource_factor)

# tactical_politicalNewsSource bin

DataClean$tactical_politicalNewsSourceAlternativeIndependentMedia_bin <- NA
DataClean$tactical_politicalNewsSourceAlternativeIndependentMedia_bin[DataRaw$tactical_political_news_source == "Alternative and/or independent media"] <- 1
DataClean$tactical_politicalNewsSourceAlternativeIndependentMedia_bin[DataRaw$tactical_political_news_source != "Alternative and/or independent media"] <- 0
table(DataClean$tactical_politicalNewsSourceAlternativeIndependentMedia_bin)

DataClean$tactical_politicalNewsSourceNewspapers_bin <- NA
DataClean$tactical_politicalNewsSourceNewspapers_bin[DataRaw$tactical_political_news_source == "Newspapers"] <- 1
DataClean$tactical_politicalNewsSourceNewspapers_bin[DataRaw$tactical_political_news_source != "Newspapers"] <- 0
table(DataClean$tactical_politicalNewsSourceNewspapers_bin)
  
DataClean$tactical_politicalNewsSourceNone_bin <- NA
DataClean$tactical_politicalNewsSourceNone_bin[DataRaw$tactical_political_news_source == "None of these"] <- 1
DataClean$tactical_politicalNewsSourceNone_bin[DataRaw$tactical_political_news_source != "None of these"] <- 0
table(DataClean$tactical_politicalNewsSourceNone_bin)

DataClean$tactical_politicalNewsSourceOnlineNewsWebsites_bin <- NA
DataClean$tactical_politicalNewsSourceOnlineNewsWebsites_bin[DataRaw$tactical_political_news_source == "Online news websites"] <- 1
DataClean$tactical_politicalNewsSourceOnlineNewsWebsites_bin[DataRaw$tactical_political_news_source != "Online news websites"] <- 0
table(DataClean$tactical_politicalNewsSourceOnlineNewsWebsites_bin)

DataClean$tactical_politicalNewsSourceOther_bin <- NA
DataClean$tactical_politicalNewsSourceOther_bin[DataRaw$tactical_political_news_source == "Other source"] <- 1
DataClean$tactical_politicalNewsSourceOther_bin[DataRaw$tactical_political_news_source != "Other source"] <- 0
table(DataClean$tactical_socialMediaOther_bin)

DataClean$tactical_politicalNewsSourceRadio_bin <- NA
DataClean$tactical_politicalNewsSourceRadio_bin[DataRaw$tactical_political_news_source == "Radio"] <- 1
DataClean$tactical_politicalNewsSourceRadio_bin[DataRaw$tactical_political_news_source != "Radio"] <- 0
table(DataClean$tactical_politicalNewsSourceRadio_bin)

DataClean$tactical_politicalNewsSourceSocialMedia_bin <- NA
DataClean$tactical_politicalNewsSourceSocialMedia_bin[DataRaw$tactical_political_news_source == "Social media (X, Instagram, Reddit, etc.)"] <- 1
DataClean$tactical_politicalNewsSourceSocialMedia_bin[DataRaw$tactical_political_news_source != "Social media (X, Instagram, Reddit, etc.)"] <- 0
table(DataClean$tactical_politicalNewsSourceSocialMedia_bin)

DataClean$tactical_politicalNewsSourceTelevision_bin <- NA
DataClean$tactical_politicalNewsSourceTelevision_bin[DataRaw$tactical_political_news_source == "Television"] <- 1
DataClean$tactical_politicalNewsSourceTelevision_bin[DataRaw$tactical_political_news_source != "Television"] <- 0
table(DataClean$tactical_politicalNewsSourceTelevision_bin)

DataClean$tactical_politicalNewsSourceDiscussions_bin <- NA
DataClean$tactical_politicalNewsSourceDiscussions_bin[DataRaw$tactical_political_news_source == "Word of mouth (discussions with family, friends, etc.)"] <- 1
DataClean$tactical_politicalNewsSourceDiscussions_bin[DataRaw$tactical_political_news_source != "Word of mouth (discussions with family, friends, etc.)"] <- 0
table(DataClean$tactical_politicalNewsSourceTelevision_bin)

DataClean$tactical_politicalNewsSourceYouTube_bin <- NA
DataClean$tactical_politicalNewsSourceYouTube_bin[DataRaw$tactical_political_news_source == "YouTube"] <- 1
DataClean$tactical_politicalNewsSourceYouTube_bin[DataRaw$tactical_political_news_source != "YouTube"] <- 0
table(DataClean$tactical_politicalNewsSourceYouTube_bin)

# tactical_foreignUSTies -----------------------------------------------
#### Do you think Canada's ties with the United States should be... 1) Much closer ; 2) Somewhat closer ; 3) About the same as now ; 4) Somewhat more distant ; 5) Much more distant

table(DataRaw$tactical_foreign_us_ties)
DataClean$tactical_foreignUSTies_numeric <- NA
DataClean$tactical_foreignUSTies_numeric[DataRaw$tactical_foreign_us_ties == "Much closer"] <- 1
DataClean$tactical_foreignUSTies_numeric[DataRaw$tactical_foreign_us_ties == "Somewhat closer"] <- 0.75
DataClean$tactical_foreignUSTies_numeric[DataRaw$tactical_foreign_us_ties == "About the same as now"] <- 0.5
DataClean$tactical_foreignUSTies_numeric[DataRaw$tactical_foreign_us_ties == "Somewhat more distant"] <- 0.25
DataClean$tactical_foreignUSTies_numeric[DataRaw$tactical_foreign_us_ties == "Much more distant"] <- 0
table(DataClean$tactical_foreignUSTies_numeric)

# bin 

DataClean$tactical_foreignUSTiesCloser_bin <- NA
DataClean$tactical_foreignUSTiesCloser_bin[DataRaw$tactical_foreign_us_ties %in% c("Somewhat closer", "Much closer")] <- 1
DataClean$tactical_foreignUSTiesCloser_bin[DataRaw$tactical_foreign_us_ties %in% c("About the same as now", "Somewhat more distant", "Much more distant")] <- 0
table(DataClean$tactical_foreignUSTiesCloser_bin)

DataClean$tactical_foreignUSTiesDistant_bin <- NA
DataClean$tactical_foreignUSTiesDistant_bin[DataRaw$tactical_foreign_us_ties %in% c("About the same as now","Somewhat closer", "Much closer")] <- 0
DataClean$tactical_foreignUSTiesDistant_bin[DataRaw$tactical_foreign_us_ties %in% c("Somewhat more distant", "Much more distant")] <- 1
table(DataClean$tactical_foreignUSTiesDistant_bin)


# tactical_foreignUSFeel (pondérée 3 wording) --------------------------------------------------
# How do you feel about the United States ? Set the slider to any number from 0 to 100, where 0 means you really dislike it and 100 means you really like it

table(DataRaw$tactical_foreign_us_feel.like)
DataClean$tactical_foreignUSFeelLike_numeric <- NA
DataClean$tactical_foreignUSFeelLike_numeric <- DataRaw$tactical_foreign_us_feel.like / 100
table(DataClean$tactical_foreignUSFeelLike_numeric)

# How do you feel about the United States ? Set the slider to any number from 0 to 100, where 0 means you have negative feelings and 100 means you have positive feelings.

table(DataRaw$tactical_foreign_us_feel.positive)
DataClean$tactical_foreignUSFeelPositive_numeric <- NA
DataClean$tactical_foreignUSFeelPositive_numeric <- DataRaw$tactical_foreign_us_feel.positive / 100
table(DataClean$tactical_foreignUSFeelPositive_numeric)

# How do you feel about Americans ? Set the slider to any number from 0 to 100, where 0 means you really dislike this group and 100 means you really like this group.

table(DataRaw$tactical_foreign_us_feel.americans)
DataClean$tactical_foreignUSFeelAmericans_numeric <- NA
DataClean$tactical_foreignUSFeelAmericans_numeric <- DataRaw$tactical_foreign_us_feel.americans / 100
table(DataClean$tactical_foreignUSFeelAmericans_numeric)


# tactical_proCarbonTax --------------------------------------------------
# The federal carbon tax should be maintained as an environmental measure

table(DataRaw$tactical_issue_carbon_tax)
DataClean$tactical_proCarbonTax_numeric <- NA
DataClean$tactical_proCarbonTax_numeric[DataRaw$tactical_issue_carbon_tax == "Strongly disagree"] <- 0
DataClean$tactical_proCarbonTax_numeric[DataRaw$tactical_issue_carbon_tax == "Somewhat disagree"] <- 0.33
DataClean$tactical_proCarbonTax_numeric[DataRaw$tactical_issue_carbon_tax == "Somewhat agree"] <- 0.66
DataClean$tactical_proCarbonTax_numeric[DataRaw$tactical_issue_carbon_tax == "Strongly agree"] <- 1
table(DataClean$tactical_proCarbonTax_numeric)

#bin

DataClean$tactical_proCarbonTax_bin <- NA
DataClean$tactical_proCarbonTax_bin[DataRaw$tactical_issue_carbon_tax %in% c("Strongly disagree", "Somewhat disagree")] <- 0
DataClean$tactical_proCarbonTax_bin[DataRaw$tactical_issue_carbon_tax %in% c("Somewhat agree", "Strongly agree")] <- 1
table(DataClean$tactical_proCarbonTax_bin)


# tactical_foreign_can_us_one ---------------------------------------------
# It would be a good thing if Canada and the United States became one country... 

table(DataRaw$tactical_foreign_can_us_one)
DataClean$tactical_proCanadaUSOneCountry_numeric <- NA
DataClean$tactical_proCanadaUSOneCountry_numeric[DataRaw$tactical_foreign_can_us_one == "Strongly disagree"] <- 0
DataClean$tactical_proCanadaUSOneCountry_numeric[DataRaw$tactical_foreign_can_us_one == "Somewhat disagree"] <- 0.33
DataClean$tactical_proCanadaUSOneCountry_numeric[DataRaw$tactical_foreign_can_us_one == "Somewhat agree"] <- 0.66
DataClean$tactical_proCanadaUSOneCountry_numeric[DataRaw$tactical_foreign_can_us_one == "Strongly agree"] <- 1
table(DataClean$tactical_proCanadaUSOneCountry_numeric)

#bin

DataClean$tactical_proCanadaUSOneCountry_bin <- NA
DataClean$tactical_proCanadaUSOneCountry_bin[DataRaw$tactical_foreign_can_us_one %in% c("Strongly disagree", "Somewhat disagree")] <- 0
DataClean$tactical_proCanadaUSOneCountry_bin[DataRaw$tactical_foreign_can_us_one %in% c("Somewhat agree", "Strongly agree")] <- 1
table(DataClean$tactical_proCanadaUSOneCountry_bin)


# tactical_iss_pro_immigration --------------------------------------------
# How many immigrants should the country admit?

table(DataRaw$tactical_iss_pro_immigration)
DataClean$tactical_proImmigration_numeric <- NA
DataClean$tactical_proImmigration_numeric[DataRaw$tactical_iss_pro_immigration == "Much less"] <- 0
DataClean$tactical_proImmigration_numeric[DataRaw$tactical_iss_pro_immigration == "Slightly less"] <- 0.25
DataClean$tactical_proImmigration_numeric[DataRaw$tactical_iss_pro_immigration == "No more, no less"] <- 0.5
DataClean$tactical_proImmigration_numeric[DataRaw$tactical_iss_pro_immigration == "Slightly more"] <- 0.75
DataClean$tactical_proImmigration_numeric[DataRaw$tactical_iss_pro_immigration == "Much more"] <- 1
table(DataClean$tactical_proImmigration_numeric)

# bin

DataClean$tactical_moreImmigrants_bin <- NA
DataClean$tactical_moreImmigrants_bin[DataRaw$tactical_iss_pro_immigration %in% c("Much less","Slightly less", "No more, no less")] <- 0
DataClean$tactical_moreImmigrants_bin[DataRaw$tactical_iss_pro_immigration %in% c("Slightly more", "Much more")] <- 1
table(DataClean$tactical_moreImmigrants_bin)

DataClean$tactical_lessImmigrants_bin <- NA
DataClean$tactical_lessImmigrants_bin[DataRaw$tactical_iss_pro_immigration %in% c("Much less", "Slightly less")] <- 1
DataClean$tactical_lessImmigrants_bin[DataRaw$tactical_iss_pro_immigration %in% c("Slightly more", "Much more", "No more, no less")] <- 0
table(DataClean$tactical_lessImmigrants_bin)


# tactical_iss_pro_bordercontrol ------------------------------------------
# Canada's border controls should be strengthened to limit the number of people applying for refugee status

table(DataRaw$tactical_iss_pro_bordercontrol)
DataClean$tactical_proBorderControl_numeric <- NA
DataClean$tactical_proBorderControl_numeric[DataRaw$tactical_iss_pro_bordercontrol == "Strongly disagree"] <- 0
DataClean$tactical_proBorderControl_numeric[DataRaw$tactical_iss_pro_bordercontrol == "Somewhat disagree"] <- 0.33
DataClean$tactical_proBorderControl_numeric[DataRaw$tactical_iss_pro_bordercontrol == "Somewhat agree"] <- 0.66
DataClean$tactical_proBorderControl_numeric[DataRaw$tactical_iss_pro_bordercontrol == "Strongly agree"] <- 1
table(DataClean$tactical_proBorderControl_numeric)

# bin

DataClean$tactical_proBorderControl_bin <- NA
DataClean$tactical_proBorderControl_bin[DataRaw$tactical_iss_pro_bordercontrol %in% c("Strongly disagree", "Somewhat disagree")] <- 0
DataClean$tactical_proBorderControl_bin[DataRaw$tactical_iss_pro_bordercontrol %in% c("Somewhat agree", "Strongly agree")] <- 1
table(DataClean$tactical_proBorderControl_bin)


# tactical_iss_reducedeficit ----------------------------------------------
# Reducing the deficit should be a priority, even if it means cutting services

# numeric

table(DataRaw$tactical_iss_reducedeficit)
DataClean$tactical_proReduceDeficit_numeric <- NA
DataClean$tactical_proReduceDeficit_numeric[DataRaw$tactical_iss_reducedeficit == "Strongly disagree"] <- 0
DataClean$tactical_proReduceDeficit_numeric[DataRaw$tactical_iss_reducedeficit == "Somewhat disagree"] <- 0.33
DataClean$tactical_proReduceDeficit_numeric[DataRaw$tactical_iss_reducedeficit == "Somewhat agree"] <- 0.66
DataClean$tactical_proReduceDeficit_numeric[DataRaw$tactical_iss_reducedeficit == "Strongly agree"] <- 1
table(DataClean$tactical_proReduceDeficit_numeric)

# bin

DataClean$tactical_proReduceDeficit_bin <- NA
DataClean$tactical_proReduceDeficit_bin[DataRaw$tactical_iss_reducedeficit %in% c("Strongly disagree", "Somewhat disagree")] <- 0
DataClean$tactical_proReduceDeficit_bin[DataRaw$tactical_iss_reducedeficit %in% c("Somewhat agree", "Strongly agree")] <- 1
table(DataClean$tactical_proReduceDeficit_bin)

# tactical_issue_lgbtq ----------------------------------------------------
# To what extent should the federal government defend the rights of LGBTQ+ people?

# numeric

table(DataRaw$tactical_issue_lgbtq)
DataClean$tactical_proDefenseLGBTQRights_numeric <- NA
DataClean$tactical_proDefenseLGBTQRights_numeric[DataRaw$tactical_issue_lgbtq == "Much less"] <- 0
DataClean$tactical_proDefenseLGBTQRights_numeric[DataRaw$tactical_issue_lgbtq == "Slightly less"] <- 0.25
DataClean$tactical_proDefenseLGBTQRights_numeric[DataRaw$tactical_issue_lgbtq == "No more, no less"] <- 0.5
DataClean$tactical_proDefenseLGBTQRights_numeric[DataRaw$tactical_issue_lgbtq == "Slightly more"] <- 0.75
DataClean$tactical_proDefenseLGBTQRights_numeric[DataRaw$tactical_issue_lgbtq == "Much more"] <- 1
table(DataClean$tactical_proDefenseLGBTQRights_numeric)

# bin

DataClean$tactical_moreDefenseLGBTQRights_bin <- NA
DataClean$tactical_moreDefenseLGBTQRights_bin[DataRaw$tactical_issue_lgbtq %in% c("Much less","Slightly less", "No more, no less")] <- 0
DataClean$tactical_moreDefenseLGBTQRights_bin[DataRaw$tactical_issue_lgbtq %in% c("Slightly more", "Much more")] <- 1
table(DataClean$tactical_moreDefenseLGBTQRights_bin)

DataClean$tactical_lessDefenseLGBTQRights_bin <- NA
DataClean$tactical_lessDefenseLGBTQRights_bin[DataRaw$tactical_issue_lgbtq %in% c("Much less", "Slightly less")] <- 1
DataClean$tactical_lessDefenseLGBTQRights_bin[DataRaw$tactical_issue_lgbtq %in% c("Slightly more", "Much more", "No more, no less")] <- 0
table(DataClean$tactical_lessDefenseLGBTQRights_bin)

# tactical_iss_bilinguisme -----------------------------------------------

table(DataRaw$tactical_iss_bilingual_prime_minister)
DataClean$tactical_iss_PMbilingue <- NA
DataClean$tactical_iss_PMbilingue[DataRaw$tactical_iss_bilingual_prime_minister == "Not at all important"] <- 0
DataClean$tactical_iss_PMbilingue[DataRaw$tactical_iss_bilingual_prime_minister == "Slightly important"] <- 0.25
DataClean$tactical_iss_PMbilingue[DataRaw$tactical_iss_bilingual_prime_minister == "Moderately important"] <- 0.5
DataClean$tactical_iss_PMbilingue[DataRaw$tactical_iss_bilingual_prime_minister == "Very important"] <- 0.75
DataClean$tactical_iss_PMbilingue[DataRaw$tactical_iss_bilingual_prime_minister == "Extremely important"] <- 1
table(DataClean$tactical_iss_PMbilingue)


# tactical_iss_dying_can --------------------------------------------------

table(DataRaw$tactical_iss_dying_can)
DataClean$tactical_iss_dyingForCan_numeric <- NA
DataClean$tactical_iss_dyingForCan_numeric[DataRaw$tactical_iss_dying_can == "Strongly disagree"] <-  0
DataClean$tactical_iss_dyingForCan_numeric[DataRaw$tactical_iss_dying_can == "Somewhat disagree"] <-  0.33
DataClean$tactical_iss_dyingForCan_numeric[DataRaw$tactical_iss_dying_can == "Somewhat agree"] <-  0.66
DataClean$tactical_iss_dyingForCan_numeric[DataRaw$tactical_iss_dying_can == "Strongly agree"] <-  1
table(DataClean$tactical_iss_dyingForCan_numeric)

DataClean$tactical_iss_dyingForCan_bin <- NA
DataClean$tactical_iss_dyingForCan_bin[DataRaw$tactical_iss_dying_can %in% c("Strongly disagree", "Somewhat disagree")] <- 0
DataClean$tactical_iss_dyingForCan_bin[DataRaw$tactical_iss_dying_can %in% c("Strongly agree", "Somewhat agree")] <- 1
table(DataClean$tactical_iss_dyingForCan_bin)


#### Partners 
# tactical_ restrictAbort -----------------------------------------------------------
## Do you support the adoption of a legislation to restrict abortion rights in Canada?
table(DataRaw$tactical_abort_restrict)
DataClean$tactical_restrictAbort <- NA
DataClean$tactical_restrictAbort[DataRaw$tactical_abort_restrict == "Strongly disagree"] <- 0
DataClean$tactical_restrictAbort[DataRaw$tactical_abort_restrict == "Somewhat disagree"] <- 0.33
DataClean$tactical_restrictAbort[DataRaw$tactical_abort_restrict == "Somewhat agree"] <- 0.66
DataClean$tactical_restrictAbort[DataRaw$tactical_abort_restrict == "Strongly agree"] <- 1
table(DataClean$tactical_restrictAbort)


# tactical_abortionThreatExists -------------------------------------------
## Access to abortion in Canada is under threat
table(DataRaw$tactical_abortion_threat)
DataClean$tactical_abortionThreatExists <- NA
DataClean$tactical_abortionThreatExists[DataRaw$tactical_abortion_threat == "Strongly disagree"] <- 0
DataClean$tactical_abortionThreatExists[DataRaw$tactical_abortion_threat == "Somewhat disagree"] <- 0.33
DataClean$tactical_abortionThreatExists[DataRaw$tactical_abortion_threat == "Somewhat agree"] <- 0.66
DataClean$tactical_abortionThreatExists[DataRaw$tactical_abortion_threat == "Strongly agree"] <- 1
table(DataClean$tactical_abortionThreatExists)


# tactical_home_situation -------------------------------------------------
table(DataRaw$tactical_home_situation)
DataClean$tactical_dwellingSituation <- NA
DataClean$tactical_dwellingSituation[DataRaw$tactical_home_situation == "I do not pay for my home"] <- "doNotPay"
DataClean$tactical_dwellingSituation[DataRaw$tactical_home_situation == "I own my home"] <- "landlord"
DataClean$tactical_dwellingSituation[DataRaw$tactical_home_situation == "I rent my home"] <- "tenant"
DataClean$tactical_dwellingSituation[DataRaw$tactical_home_situation == "Other"] <- "other"
DataClean$tactical_dwellingSituation <- factor(DataClean$tactical_dwellingSituation,
                                                        levels = c("doNotPay",
                                                                   "landlord",
                                                                   "tenant",
                                                                   "other"),
                                                        ordered = TRUE)
table(DataClean$tactical_dwellingSituation)


# tactical_immigration_type.immigrants.construction -----------------------
# Do you agree or disagree that Canada should allow more immigrants who work in home construction to come and live here?

table(DataRaw$tactical_immigration_type.immigrants.construction)
DataClean$tactical_moreImmigrantsConstruction <- NA
DataClean$tactical_moreImmigrantsConstruction[DataRaw$tactical_immigration_type.immigrants.construction == "Strongly disagree"] <- 0
DataClean$tactical_moreImmigrantsConstruction[DataRaw$tactical_immigration_type.immigrants.construction == "Somewhat disagree"] <- 0.25
DataClean$tactical_moreImmigrantsConstruction[DataRaw$tactical_immigration_type.immigrants.construction == "Neither agree nor disagree"] <- 0.5
DataClean$tactical_moreImmigrantsConstruction[DataRaw$tactical_immigration_type.immigrants.construction == "Somewhat agree"] <- 0.75
DataClean$tactical_moreImmigrantsConstruction[DataRaw$tactical_immigration_type.immigrants.construction == "Strongly agree"] <- 1
table(DataClean$tactical_moreImmigrantsConstruction)

# tactical_immigration_type.immigrants.agricultural -----------------------
# Do you agree or disagree that Canada should allow more immigrants who work in agricultural farming to come and live here?
table(DataRaw$tactical_immigration_type.immigrants.agricultural)
DataClean$tactical_moreImmigrantsAgriculture <- NA
DataClean$tactical_moreImmigrantsAgriculture[DataRaw$tactical_immigration_type.immigrants.agricultural == "Strongly disagree"] <- 0
DataClean$tactical_moreImmigrantsAgriculture[DataRaw$tactical_immigration_type.immigrants.agricultural == "Somewhat disagree"] <- 0.25
DataClean$tactical_moreImmigrantsAgriculture[DataRaw$tactical_immigration_type.immigrants.agricultural == "Neither agree nor disagree"] <- 0.5
DataClean$tactical_moreImmigrantsAgriculture[DataRaw$tactical_immigration_type.immigrants.agricultural == "Somewhat agree"] <- 0.75
DataClean$tactical_moreImmigrantsAgriculture[DataRaw$tactical_immigration_type.immigrants.agricultural == "Strongly agree"] <- 1
table(DataClean$tactical_moreImmigrantsAgriculture)

# tactical_immigration_type.immigrants.live -------------------------------
# Do you agree or disagree that Canada should allow more immigrants to come and live here?
table(DataRaw$tactical_immigration_type.immigrants.live)
DataClean$tactical_moreImmigrantsLive <- NA
DataClean$tactical_moreImmigrantsLive[DataRaw$tactical_immigration_type.immigrants.live == "Strongly disagree"] <- 0
DataClean$tactical_moreImmigrantsLive[DataRaw$tactical_immigration_type.immigrants.live == "Somewhat disagree"] <- 0.25
DataClean$tactical_moreImmigrantsLive[DataRaw$tactical_immigration_type.immigrants.live == "Neither agree nor disagree"] <- 0.5
DataClean$tactical_moreImmigrantsLive[DataRaw$tactical_immigration_type.immigrants.live == "Somewhat agree"] <- 0.75
DataClean$tactical_moreImmigrantsLive[DataRaw$tactical_immigration_type.immigrants.live == "Strongly agree"] <- 1
table(DataClean$tactical_moreImmigrantsLive)

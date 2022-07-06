#' Choice List
#'
#' An S3 vector class (subclass of list) containing the inputs,
#' i.e., column names, for the dashboard input sidebars. The column names
#' are grouped together.
#'
#' @importFrom vctrs new_list_of
#'
#' @return A `ElectProtect_choices` object.
#'
#' @export
choices <- function() {
  new_list_of(
    x = list(
      "Experiences & Impressions" = c(
        "Democracy Fairness" = "democracy_fairness",
        "Two-Party" = "view_two_party",
        "Joe Biden Victory" = "view_joe_biden_victory",
        "Election Trust Change" = "election_trust_change",
        "Presidential Candidate" = "pres_candidate",
        "Presidential Candidate Support" = "pres_candidate_excitement",
        "Democracy Representation" = "democracy_representation",
        "Identify with Political Party" = "party_identification",
        "Party Identification Change" = "party_identification_change"
      ),
      "Political Messaging" = c(
        "Jocelyn Benson (SecState of Michigan)" = "message_benson",
        "Donald Trump (Former POTUS)" = "message_trump",
        "Jena Griswold (SecState of Colorado)" = "message_griswold",
        "Susan Greenhalgh (Harvard Anthropologist)" = "message_greenhalgh",
        "Josh Hawley (Senator from Missouri)" = "message_hawley"
      ),
      # Only for two-way
      "Election Threat (Most Concerning)" = c(
        "Law and Regulation" = "most_concern_law_and_regulation",
        "Insufficient Voting" = "most_concern_insuf_early_or_by_mail_voting",
        "Cybersecurity" = "most_concern_cybersecurity",
        "Tampering" = "most_concern_tampering",
        "False Info on Security" = "most_concern_false_info_on_security",
        "Faulty Machine" = "most_concern_faulty_machine",
        "Long Line" = "most_concern_long_line",
        "Foreign Interference" = "most_concern_foreign_interference",
        "Illegal Voting" = "most_concern_illegal_voting",
        "Corrupt Workers" = "most_concern_corrupt_official_or_worker",
        "Harrassment of Workers" = "most_concern_harassment_of_official_or_worker",
        "Underfunding" = "most_concern_underfunding",
        "Baseless Dispute" = "most_concern_baseless_dispute",
        "None Above" = "most_concern_none_above"
      ),
      # Only for two-way
      "Election Threat (Least Concerning)" = c(
        "Law and Regulation" = "least_concern_law_and_regulation",
        "Insufficient Voting" = "least_concern_insuf_early_or_by_mail_voting",
        "Cybersecurity" = "least_concern_cybersecurity",
        "Tampering" = "least_concern_tampering",
        "False Info on Security" = "least_concern_false_info_on_security",
        "Faulty Machine" = "least_concern_faulty_machine",
        "Long Line" = "least_concern_long_line",
        "Foreign Interference" = "least_concern_foreign_interference",
        "Illegal Voting" = "least_concern_illegal_voting",
        "Corrupt Workers" = "least_concern_corrupt_official_or_worker",
        "Harrassment of Workers" = "least_concern_harassment_of_official_or_worker",
        "Underfunding" = "least_concern_underfunding",
        "Baseless Dispute" = "least_concern_baseless_dispute",
        "None Above" = "least_concern_none_above"
      ),
      # Only for two-way
      "Election Threat (Least Informed)" = c(
        "Law and Regulation" = "least_informed_law_and_regulation",
        "Insufficient Voting" = "least_informed_insuf_early_or_by_mail_voting",
        "Cybersecurity" = "least_informed_cybersecurity",
        "Tampering" = "least_informed_tampering",
        "False Info on Security" = "least_informed_false_info_on_security",
        "Faulty Machine" = "least_informed_faulty_machine",
        "Long Line" = "least_informed_long_line",
        "Foreign Interference" = "least_informed_foreign_interference",
        "Illegal Voting" = "least_informed_illegal_voting",
        "Corrupt Workers" = "least_informed_corrupt_official_or_worker",
        "Harrassment of Workers" = "least_informed_harassment_of_official_or_worker",
        "Underfunding" = "least_informed_underfunding",
        "Baseless Dispute" = "least_informed_baseless_dispute",
        "None Above" = "least_informed_none_above"
      ),
      "Demographics" = c(
        "Age Range" = "age",
        "Education Rollup" = "education_rollup",
        "Gender" = "gender",
        "Political Ideology" = "ideology",
        "Politcal Party" = "party",
        "Political Party (Democratic)" = "party_dem_identity",
        "Political Party (Republican)" = "party_rep_identity",
        "Race" = "race"
      ),
      # Only for two-way
      "News Source" = c(
        "News Source Visit Frequency" = "preferred_news_visit_freq",
        "Facebook" = "news_source_facebook",
        "Twitter" = "news_source_twitter",
        "Instagram" = "news_source_instagram",
        "Snapchat" = "news_source_snapchat",
        "TikTok" = "news_source_tiktok",
        "Parlor" = "news_source_parlor",
        "Other Social Media" = "news_source_other_social_media",
        "Radio" = "news_source_npr_or_local_affiliate_radio",
        "Good Morning America" = "news_source_good_morning_america_show",
        "Today Show" = "news_source_today_show",
        "CNN" = "news_source_cnn",
        "ABC" = "news_source_abc",
        "FOX" = "news_source_fox",
        "Local Broadcast" = "news_source_local_broadcast",
        "Other TV Show" = "news_source_other_tv_show",
        "New York Times" = "news_source_new_york_times",
        "Washington Post" = "news_source_washington_post",
        "Guardian" = "news_source_the_guardian",
        "Epoch Times" = "news_source_epoch_times",
        "Daily Wire" = "news_source_the_daily_wire",
        "Breitbar" = "news_source_breitbar",
        "International News" = "news_source_international_news_outlets",
        "Newpaper" = "news_source_newspaper_print_or_online",
        "I Dislike News" = "news_source_dislike_news",
        "None Above" = "news_source_none_above"
      ),
      # Only for topline multiple selection questions
      "Multiple-Selection Questions" = c(
        "News Source" = "news_source",
        "Election Threat (Most Concerning)" = "most_concern",
        "Election Threat (Least Concerning)" = "least_concern",
        "Election Threat (Least Informed)" = "least_informed"
      ),
      # Modeled columns
      "Modeled Segmentations" = c(
        "Climate Change Believer" = "climate_change_believer",
        "2024 Election Trust" = "election_trust_2024",
        "January 6 Justification" = "january_6_justification",
        "Feeling Towards Opposition Party" = "opposition_party_feeling",
        "Political Division Helper" = "political_division_helper"
      )
    ),
    ptype = character(),
    class = "ElectProtect_choices"
  )
}

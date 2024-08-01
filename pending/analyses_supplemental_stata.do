// First, set the working directory to the project, set you own path
** cd ""
cd "C:/Users/anama/Documents/Postdoc/Projects/dutch_media_profs_r"

// import the dataset and tidy it up
import delimited "panel_datasets/prof_panel_tidy_1_5.csv", clear

encode profile_id, generate(prof_id) label(profile_id)
encode inferred_gender, generate(gender) label(inferred_gender)

keep if year < 2024 
drop if missing(general_field)
drop if general_field == "NA"

gen coa_online_all_total = coa_online_news_total + coa_blogs_total
gen coa_online_all_total_l = coa_online_news_total_l + coa_blogs_total_l
gen coa_online_all = coa_online_news + coa_blogs
gen coa_online_all_l = coa_online_news_l + coa_blogs_l

gen coa_tot_online_all_total = coa_tot_online_news_total + coa_tot_blogs_total
gen coa_tot_online_all_total_l = coa_tot_online_news_total_l + coa_tot_blogs_total_l
gen coa_tot_online_all = coa_tot_online_news + coa_tot_blogs
gen coa_tot_online_all_l = coa_tot_online_news + coa_tot_blogs_l

destring news_all, replace ignore("NA") 
destring news_all_l, replace ignore("NA")
destring cited_by_total_all_l, replace ignore("NA")
destring alt_online_all_total_l, replace ignore("NA")
destring alt_twitter_total_l, replace ignore("NA")
destring coa_tot_cited_by_total_l, replace ignore("NA")
destring coa_online_all_total_l, replace ignore("NA") 
destring coa_twitter_total_l, replace ignore("NA")
destring news_all_total_l, replace ignore("NA")
destring alt_online_all, replace ignore("NA")
destring alt_online_all_l, replace ignore("NA")
destring alt_twitter, replace ignore("NA")
destring alt_twitter_l, replace ignore("NA")

// Robust regression

// Printed news

// with lag
robreg s news_all i.gender news_all_l cited_by_total_all_l alt_online_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "STEM", cluster(prof_id)
eststo model1

robreg s news_all i.gender news_all_l cited_by_total_all_l alt_online_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Medicine", cluster(prof_id)
eststo model2

robreg s news_all i.gender news_all_l cited_by_total_all_l alt_online_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Social sciences", cluster(prof_id)
eststo model3

robreg s news_all i.gender news_all_l cited_by_total_all_l alt_online_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Arts & Humanities", cluster(prof_id)
eststo model4

esttab using "results\supplement_tables\printed_news_rob_full.rtf", b(5) se(5) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) noconstant title(Printed news) stats(N r2_p) mlabels("STEM" "Medicine" "Social science" "Arts & Humanities") drop(*.year 1.gender) coeflabels(2.gender "Inferred gender (reference:man)" news_all_l "Printed news attention (t-1)" cited_by_total_all_l "Total citations (t-1)" alt_online_all_total_l "Total online attention (t-1)"  alt_online_all_l "Online attention (t-1)" alt_twitter_total_l "Total Twitter/X attention (t-1)" alt_twitter_l "Twitter/X attention (t-1)" coa_tot_cited_by_total_l " Coauthors' total citations (t-1)"coa_online_all_total_l "Coauthors' total online attention total (t-1)" coa_twitter_total_l "Coauthors' total Twitter/X attention (t-1)")

eststo clear 

// without lag
robreg s news_all i.gender cited_by_total_all_l alt_online_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "STEM", cluster(prof_id)
eststo model1

robreg s news_all i.gender cited_by_total_all_l alt_online_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Medicine", cluster(prof_id)
eststo model2

robreg s news_all i.gender cited_by_total_all_l alt_online_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Social sciences", cluster(prof_id)
eststo model3

robreg s news_all i.gender cited_by_total_all_l alt_online_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Arts & Humanities", cluster(prof_id)
eststo model4

esttab using "results\supplement_tables\printed_news_rob_nolag.rtf", b(5) se(5) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) noconstant title(Printed news) stats(N r2_p) mlabels("STEM" "Medicine" "Social science" "Arts & Humanities") drop(*.year 1.gender) coeflabels(2.gender "Inferred gender (reference:man)" news_all_l "Printed news attention (t-1)" cited_by_total_all_l "Total citations (t-1)" alt_online_all_total_l "Total online attention (t-1)"  alt_online_all_l "Online attention (t-1)" alt_twitter_total_l "Total Twitter/X attention (t-1)" alt_twitter_l "Twitter/X attention (t-1)" coa_tot_cited_by_total_l " Coauthors' total citations (t-1)"coa_online_all_total_l "Coauthors' total online attention total (t-1)" coa_twitter_total_l "Coauthors' total Twitter/X attention (t-1)")

eststo clear 

// Online news
// with lag
robreg s alt_online_all i.gender alt_online_all_l cited_by_total_all_l news_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "STEM", cluster(prof_id)
eststo model1

robreg s alt_online_all i.gender alt_online_all_l cited_by_total_all_l news_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Medicine", cluster(prof_id)
eststo model2

robreg s alt_online_all i.gender alt_online_all_l cited_by_total_all_l news_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Social sciences", cluster(prof_id)
eststo model3

robreg s alt_online_all i.gender alt_online_all_l cited_by_total_all_l news_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Arts & Humanities", cluster(prof_id)
eststo model4

esttab using "results\supplement_tables\online_news_rob_full.rtf", b(5) se(5) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) noconstant title(Printed news) stats(N r2_p) mlabels("STEM" "Medicine" "Social science" "Arts & Humanities") drop(*.year 1.gender) coeflabels(2.gender "Inferred gender (reference:man)" news_all_total_l "Total printed news attention (t-1)" news_all_l "Printed news attention (t-1)" cited_by_total_all_l "Total citations (t-1)" alt_online_all_total_l "Total online attention (t-1)"  alt_online_all_l "Online attention (t-1)" alt_twitter_total_l "Total Twitter/X attention (t-1)" alt_twitter_l "Twitter/X attention (t-1)" coa_tot_cited_by_total_l " Coauthors' total citations (t-1)"coa_online_all_total_l "Coauthors' total online attention total (t-1)" coa_twitter_total_l "Coauthors' total Twitter/X attention (t-1)")

eststo clear 

// without lag
robreg s alt_online_all i.gender cited_by_total_all_l news_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "STEM", cluster(prof_id)
eststo model1

robreg s alt_online_all i.gender cited_by_total_all_l news_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Medicine", cluster(prof_id)
eststo model2

robreg s alt_online_all i.gender cited_by_total_all_l news_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Social sciences", cluster(prof_id)
eststo model3

robreg s alt_online_all i.gender cited_by_total_all_l news_all_total_l alt_twitter_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Arts & Humanities", cluster(prof_id)
eststo model4

esttab using "results\supplement_tables\online_news_rob_nolag.rtf", b(5) se(5) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) noconstant title(Printed news) stats(N r2_p) mlabels("STEM" "Medicine" "Social science" "Arts & Humanities") drop(*.year 1.gender) coeflabels(2.gender "Inferred gender (reference:man)" news_all_total_l "Total printed news attention (t-1)" news_all_l "Printed news attention (t-1)" cited_by_total_all_l "Total citations (t-1)" alt_online_all_total_l "Total online attention (t-1)"  alt_online_all_l "Online attention (t-1)" alt_twitter_total_l "Total Twitter/X attention (t-1)" alt_twitter_l "Twitter/X attention (t-1)" coa_tot_cited_by_total_l " Coauthors' total citations (t-1)"coa_online_all_total_l "Coauthors' total online attention total (t-1)" coa_twitter_total_l "Coauthors' total Twitter/X attention (t-1)")

eststo clear 

// Twitter/X attention
// with lag
robreg s alt_twitter i.gender alt_twitter_l cited_by_total_all_l news_all_total_l alt_online_all_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "STEM", cluster(prof_id)
eststo model1

robreg s alt_twitter i.gender alt_twitter_l cited_by_total_all_l news_all_total_l alt_online_all_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Medicine", cluster(prof_id)
eststo model2

robreg s alt_twitter i.gender alt_twitter_l cited_by_total_all_l news_all_total_l alt_online_all_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Social sciences", cluster(prof_id)
eststo model3

robreg s alt_twitter i.gender alt_twitter_l cited_by_total_all_l news_all_total_l alt_online_all_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Arts & Humanities", cluster(prof_id)
eststo model4

esttab using "results\supplement_tables\twitter_news_rob_full.rtf", b(5) se(5) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) noconstant title(Printed news) stats(N r2_p) mlabels("STEM" "Medicine" "Social science" "Arts & Humanities") drop(*.year 1.gender) coeflabels(2.gender "Inferred gender (reference:man)" news_all_total_l "Total printed news attention (t-1)" news_all_l "Printed news attention (t-1)" cited_by_total_all_l "Total citations (t-1)" alt_online_all_total_l "Total online attention (t-1)"  alt_online_all_l "Online attention (t-1)" alt_twitter_total_l "Total Twitter/X attention (t-1)" alt_twitter_l "Twitter/X attention (t-1)" coa_tot_cited_by_total_l " Coauthors' total citations (t-1)"coa_online_all_total_l "Coauthors' total online attention total (t-1)" coa_twitter_total_l "Coauthors' total Twitter/X attention (t-1)")

eststo clear 

// without lag

robreg s alt_twitter i.gender cited_by_total_all_l news_all_total_l alt_online_all_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "STEM", cluster(prof_id)
eststo model1

robreg s alt_twitter i.gender cited_by_total_all_l news_all_total_l alt_online_all_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Medicine", cluster(prof_id)
eststo model2

robreg s alt_twitter i.gender cited_by_total_all_l news_all_total_l alt_online_all_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Social sciences", cluster(prof_id)
eststo model3

robreg s alt_twitter i.gender cited_by_total_all_l news_all_total_l alt_online_all_total_l coa_tot_cited_by_total_l coa_online_all_total_l coa_twitter_total_l i.year if general_field == "Arts & Humanities", cluster(prof_id)
eststo model4

esttab using "results\supplement_tables\twitter_news_rob_nolag.rtf", b(5) se(5) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) noconstant title(Printed news) stats(N r2_p) mlabels("STEM" "Medicine" "Social science" "Arts & Humanities") drop(*.year 1.gender) coeflabels(2.gender "Inferred gender (reference:man)" news_all_total_l "Total printed news attention (t-1)" news_all_l "Printed news attention (t-1)" cited_by_total_all_l "Total citations (t-1)" alt_online_all_total_l "Total online attention (t-1)"  alt_online_all_l "Online attention (t-1)" alt_twitter_total_l "Total Twitter/X attention (t-1)" alt_twitter_l "Twitter/X attention (t-1)" coa_tot_cited_by_total_l " Coauthors' total citations (t-1)"coa_online_all_total_l "Coauthors' total online attention total (t-1)" coa_twitter_total_l "Coauthors' total Twitter/X attention (t-1)")

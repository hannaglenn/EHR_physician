# Meeting Minutes 
### This document serves as a log to record what is discussed in meetings with Ian regarding my third year paper.

## 09-30-2021
Questions to ask at the meeting:
- Should I limit the sample to AHA hospitals from the beginning? Capturing only physicians relationships with AHA hospitals?

Major Points Discussed:
- First, we discussed the issues with mergers and how I can potentially deal with them. The issue is: hospitals may adopt EHRs in order to forclose on smaller hospitals, which will affect share of patients at the target firm. For now, I will aggregate the data to the physician level (to avoid spillover effects) and this should mostly take care of the merger issue. There is a quick sensativity check I can do where I drop all physicians that work with a target hospital. In the future, I can bring in merger data and try to trak these things more carefully over time. 
- Next, we discussed some big picture things for this paper. I'm going to back up a little bit and focus more on my original questions, which were at the physician level. I'm going to create dependent variables for working (using variables separate from the shared patients with hospitals: bring in labs and such) and whether physicians are switching to private practices (using SK&A data).
- Also, instead of jumping straight to Sant'Anna and Callaway, start with just basic event study setup. Then work towards more complicated. 

Action Items:
- Bring in shared patient data for total working levels of physicians
- Bring in SK&A data
- Lay out in words exactly what I'm doing in each analysis to make it clear in my own mind

Points of Confusion:
- none 


## 10-7-2021 (I cancelled this meeting to have more time to make progress)
Questions to ask at the meeting:

Major Points Discussed:


Action Items:


Points of Confusion:

## 10-21-2021
Questions to ask:
- Show the tables with diff in diffs for each year 

Major Points Discussed:
- Before I present results on the extensive margin, I need to think a lot more about the billing relationship between physicians and hospitals. Since I'm seeing younger physicians drop out of the data, it may be that once they become employees of the hospital they no longer bill individually. I need to do some detective work to see if physicians who drop out are truly dropping out or not (do they leave for one year? look up their NPI and see if they are still practicing, etc. )
- Instead of year diff in diffs, create event studies for each year. That way I can see the effect in the years after treatment. Think about manually deciding the control groups: it may be that those who are never exposed to EHRs are just totally different. Think about making the control group those who adopt at least a couple years after. Also use Callaway and Sat'Anna's package and compare. 
- For the intensive margin:
-   Be very clear about the population of physicians. 
-   Limit to just hospital patients so I don't have double counts
-   Also use part B 
- Another potential outcome (instead of waiting for the SK&A data) is to look at hospital patients vs. nonhospital patients and see whether doctors are switching out of hospital settings


## 1-14-2022
Questions to ask:
- MDPPAS data?

Major Points Discussed:
- First we discussed the progress I've made on my third year paper. In particular, the general motivation that would make anyone interested in my question. Along with the efficiency and quality arguments I've already been thinking of, Ian also brought up the possibility that access to care may be improving, which is why we don't see a decrease in costs but that is an improvement in a different capacity. The efficiency question is POLICY RELEVENT, the access question is PATIENT RELEVENT
- The MDPPAS data should be coming in the next week or so, they approved exporting it as long as certain things are masked. Ian is working on the masking and then will get approval. 
- It may be worth looking into whether there is physician level info regarding MACRA (changes in how physicians are paid that has meaningful EHR use in the requirements)
- At the top of my to-do list should be speaking to a hospitalist about some of these institutional details to make sure I'm getting the story right

Action Steps:
- Talk to hospitalist
- Write aspirational introduction and send it to Ian by the 20th


## 1-28-2022
Major Points Discussed:
- I received MDPPAS on 1-23 and gave an update to what I have done so far. I incorporated MDPPAS into the data I've been using by filtering physicians using the specialty coding and using the MDPPAS information for the dependent variables in my analysis. I had very preliminary results to show.
- We first talked about my data process. The major notes here were to try to expand outcomes to 2016-2017 (which is in MDPPAS but not in shared patient) by assuming a control group. That is, there won't be any new treatment groups after 2015 but I can still analyze the previous control and treatments following 2015. Another note was to change the retirement variable to not considering the shared patient future claims, since MDPPAS is the more reliable data. 
- Then we went over the preliminary results I have so far. Reitrement shows nothing meaningful. I see an increase in the probability of wokring in an office as a result of EHR implementation in hospitals. I also see an increase in total claims as a result of EHR implementation in hospitals. I need to dig deeper into how the last two results fit together. Specifically, I need to limit the sample of physicians for the productivity argument to see who is driving this result. (People who stay in hospitals vs. move to offices; quantile effects)
- Other tips: 
1. Look into Jonathen Roth's recent work on pretrends
2. Use patient count instead of claim count (Ian will try to pull this from MDPPAS for me)
3. Look into who has "data assistant" npi, like what their actual job is
4. Potential other controls could be physician quality, hospital FTEs, physician network size (although physician network size could also be an interesting outcome to look at)

Action steps: 
- Clarifying what my results mean as a story. More specifically, talking to a physician
- Expand outcomes to 2016-2017 if possible
- look up npi with data assistant tax codes online
- consider network size from shared patient data

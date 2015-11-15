# lexicase-hyper-selection-GECCO16
Paper describing hyper selection in lexicase and @thelmuth's dissertations experiments suggesting *who* is selected is more important than *how often*.

The basic question:
 * We know that lexicase is good (at least in many circumstances, such as software synthesis).
 * We know that lexicase leads to hyperselection, where it's fairly common for an individual to receive over 10% of the selections in a given generation. Sometimes an single individual can receive well over 90% of the selections in a given generation.
 * How important is hyperselection to the success of lexicase?

@thelmuth (as part of his dissertation) developed a set of experiments to address this question, essentially determining which individuals would likely be selected via lexicase, but smoothing out the distribution of selections so there isn't so much emphasis on a very small number of individuals. These variations performed as well as (or even a hair better, but not statistically significant) "standard" lexicase, which suggests that *which* individuals are selected is more important than *how often* they're selected.


Maybe start with a lower prevalence (like 1 per thousand)

Delete old model, we like the new one

Work on visualizing R0; the current lines don't illustrate the idea that the curves are the same. Maybe use color, transparency????

Delete the boring graphs

Compare incidence curves or rate of early growth
* Make pictures of different simulations and show incidence only
	* (incidence is what we call trans, I don't know if you're saving it)
	* Alternatively could use prevalence; discuss with each other, and be clear when presenting
* Could also try to estimate initial value of little r

Check finishing:
* e.g., give a warning if I_final/N > thresh

The direct labels package?

For the data plots: try to make them more user-friendly. We should be able to see quickly that confirmed goes with confirmed, death with death etc. This is an interesting aesthetic question. Maybe make a long data frame? If so, maybe make separate columns for these aesthetics (e.g., surveillanceType, eventType).

Estimate initial value of r0 from the data? One suggestion from lecture would be to use a glm.nb in R.

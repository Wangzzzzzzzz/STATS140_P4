## STATS 140SL Project 4

Census at school data cleaning
***question:
for footlength_cm, in pdf it says:"4. Since the ratio of right foot length to left foot length should be around 1 for
human. For the NAs in the modified Right Footlength cm variable, we replace them
with their corresponding left foot length. Similarly, we use the right footlength value
to replace the NAs in the variable Left Footlength cm.
5. Modify the Longer foot based on the ratio of right footlength to left footlength.(e.g.ratio>1—Longer foot = Right foot)"
which i didn't do as i don't have other two's infos, let's see if we need to do that.

## Variables Left:

| Name | Type | Possible ways to clean |
| ---- | ---- | -------- |
| Importance_reducing_pollution | Numeric range | set all value out of range (0-1000) to have flag 1 |
| Importance_recycling_rubbish | Numeric range | set all value out of range (0-1000) to have flag 1 |
| Importance_conserving_water | Numeric range | set all value out of range (0-1000) to have flag 1 |
| Importance_saving_energy | Numeric range | set all value out of range (0-1000) to have flag 1 |
| Importance_owning_computer | Numeric range | set all value out of range (0-1000) to have flag 1 |
| Importance_internet_access | Numeric range | set all value out of range (0-1000) to have flag 1 |

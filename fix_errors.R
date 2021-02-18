
# 20210217

# 1. function expected_child_death() gives error -------
# Diagnosis: 
# When the loop processes the array lx.kids.arr, it fails because
# the array has dim(lx.kids.arr)
# [1] 35 86 51,
# where xs = 15:49; length(xs) is 35
# However, the fertility vector(derived from ASFC) has length 36
# because it uses ages 15:50!
# History:
# This confusion probably originate beause the original code
# from Emilio used Swedish data that had different reproductive ages.
# From commit 2c150d6 on Jan 24, 2020 (changed ages to 15)
# "when creating matrix of survival probs, the x's were 13:54, when they should be 15:49"

# Solution:
# When creating the 1x1 ASFRC data make sure that fertility only goes up to age 49!


# 2. Robustness check does not reproduce -----------

# Diagnosis:
# I cannot produce the same robustness plot for Guatemala. 
# values I create now are too high.
# 

# Solution:
# The function that created the ASFRC was saving the object as a data.table
# which produced an error when runnin the loop to get the child death estimates
# Force ASFRC to be a data.frame
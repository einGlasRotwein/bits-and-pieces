
# Cake experiment: 40 participants taste 20 different cakes, two conditions: 
# blindfolded or not. Between subjects: Participants get to try 10 cakes (set A) 
# blindfolded and 10 (set B) without a blindfold. Which set (A or B) is 
# blindfolded is balanced across participants. Of interest: Does the number of 
# dropouts (i.e. people who get sick and break up the experiment) differ?
# a) Most importantly: Across cakes? I.e. do some cakes cause people to get 
#    sicker?
# b) Also: Is there an interaction between cake and condition, i.e. does the 
#    red velvet cause people to get sick in the blindfolded condition, but not 
#    without a blindfold - while it is the other way around for the chocolate 
#    cake?

# Question 1: Is dependency an issue? It is the same stimuli (cakes) used in 
#             both conditions, but WITHOUT repeated measures.

# Question 2: Is a log linear model the right choice here? I would set up a 
#             saturated model with cake x blindfold x dropout. Then I'd exclude 
#             the three-way-interaction and see if the model still works. If 
#             that is the case, I'd eliminate the two-way-interactions and so 
#             forth, until I am either left if main effects (if there are any) 
#             or find that at least one of the interactions is needed for an 
#             appropriate model fit.

# Toy data
cake <- c("red velvet", "chocolate chip", "chocolate", "lemon", "strawberry",
         "carrot", "walnut", "oreo crumble", "apple pie", "scone", 
         "victoria sponge", "shortbread", "peanut butter", "black forest",
         "chocolate souffle", "gingerbread", "banoffee", "yeast rolls",
         "buttercream cupcake", "marble cake")

# Dropout is rather uncommon - for each cake, no more than 10 participants quit
set.seed(11)
dropout <- sample(0:10, 40, replace = TRUE)

cakes <- data.frame(
  cake = rep(cake, 2),
  set = rep(c("A", "B"), each = 10, times = 2),
  blindfold = rep(c("yes", "no"), each = 20),
  dropout = dropout
)

# Let's model some effects: Huge dropout for banoffee, because banana sucks. And 
# also for the buttercream cupcakes, because the buttercream went stale.
cakes$dropout[cakes$cake %in% c("banoffee", "buttercream cupcake")] <- c(23, 39)

cakes$nodrop <- 40 - cakes$dropout

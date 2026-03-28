# model.R

# RUN MODEL
model_fd <- plm(lwage ~ middleSchool + highSchool +  undergrad + higherEd + expyrs + NLW, 
                data = usoc_final, 
                index = c("pidp", "year"), 
                model = "fd")

summary(model_fd)

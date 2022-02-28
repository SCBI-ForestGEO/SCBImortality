# How to editing records with errors

### Finding records of errors

The lists of records with errors and missed trees are [here](https://github.com/SCBI-ForestGEO/SCBImortality/tree/main/testthat/reports/requires_field_fix).

A summary of n errors by quadrat is [here](https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/testthat/reports/quadrat_n_errors_summary.csv).

All fixes need to be done in FastField (unless this proves awful, in which case we can come up with an alternative). To send records back to FastField for editing, follow these steps:

### 1. Login to the [FFF's account](https://manage.fastfieldforms.com/Account/Login) (username ecoclimlab@gmail.com) and choose 'Legacy Portal' at bottom of left sidebar.

<img src="https://user-images.githubusercontent.com/6355854/124400940-2bce3f80-dcf4-11eb-8660-ec6004f38968.png" alt="drawing" width="300"/>


### 2. Go to 'Submissions' and in the 'Filters' section on the right select 'SCBI Mortality'
 <img width="1488" alt="image" src="https://user-images.githubusercontent.com/6355854/124400980-764fbc00-dcf4-11eb-9be2-d636b2082b87.png">
 
 Be sure you select "all time" for submissions:
 
 <img width="309" alt="image" src="https://user-images.githubusercontent.com/6355854/124512880-7bba0e80-dda7-11eb-8116-fda0a7fdeff5.png">


### 3. Add a filter - this can be the quadrat name from the drop down list: Quad/lookuplistpicker_1

<img width="540" alt="image" src="https://user-images.githubusercontent.com/6355854/124401087-79977780-dcf5-11eb-97c0-3c22ed179a93.png">

### 4. Use [SCBImortality/testthat/reports/quadrat_n_errors_summary.csv](https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/testthat/reports/quadrat_n_errors_summary.csv) to identify quadrats with errors.  Apply the filter and you should have a list of the submissions for the quadrat (likely only 1 per quadrat)

<img width="328" alt="image" src="https://user-images.githubusercontent.com/6355854/124401532-fcb9cd00-dcf7-11eb-8bf4-00709d7665a9.png">

You can do several quadrats at once.

<img width="328" alt="image" src="https://user-images.githubusercontent.com/6355854/124401649-82d61380-dcf8-11eb-8a55-c97140df5c1b.png">


### 5. To the left of the submission there is a 'View' button with an arrow. Click on the arrow and 'Dispatch'

<img width="339" alt="image" src="https://user-images.githubusercontent.com/6355854/124401823-bd8c7b80-dcf9-11eb-98f0-c86f55cbfb25.png">

Unfortunately, this has to be done one quad at a time. 

### 6. Type ecoclimlab@gmail.com in the 'Users' box, then click 'Dispatch'
<img width="599" alt="image" src="https://user-images.githubusercontent.com/6355854/124513914-d6ed0080-dda9-11eb-89fe-d09504e388f6.png">

You can name the dispatch, as above. I think it will be worth it to rename all with the quad numbers. It may also be helpful to list the n errors, but that slows down the process.  

### 7. Repeat steps 5-6 for all quadrats.

### 8. Open the FFF's app on the iPad and open the Inbox to view the dispatched file. 
They show up in the Inbox.
From there you can edit and resubmit and the submission should replace the original.

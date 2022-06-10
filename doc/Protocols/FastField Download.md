# Instructions for processing FastField Data

## 1. Log into FastFieldForms
URL: https://manage.fastfieldforms.com/Account/Login
see [this](https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/doc/Protocols/iPad%20%26%20FastField%20setup.md) if you need help to setup. 

## 2.Go to `Submissions`
Make sure you are in the NEW portal. 

## 3. Highlight ALL the forms for the current year

Filter by "this year", then check the select-all box at the top.
<img width="1119" alt="image" src="https://user-images.githubusercontent.com/6355854/171674461-17a63512-a8d5-4767-9c26-7b54df6495e2.png">

## 4. click on `Download` and chose `Excel`

## 5. save Excel file [here](https://github.com/SCBI-ForestGEO/SCBImortality/tree/main/raw_data/FFF_excel)
Unzip first if needed.

There should be only one form for the current year. The name should include the year and not be too long. Current example: "SCBI Mortality 2022.xlsx"

You will replace any existing file with the updated version.

## 6. push to GitHub. 
Preferably, flag everyone who helped collect the data as coauthors in the commit.

## 7. wait several minutes for GitHub Actions continuous integration system to complete.
Once complete, the error reports and map should update. If CI fails, notify Valentine. 

## 8. Log into Fast Field Forms and re-dispatch quadrats with errors
Dispatch by logging in to FFF's via this link: https://portal.fastfieldforms.com/SubmissionActivity.
The only field you have to fill in is the email address (ecoclimlab@gmail.com). It would probably be helpful if you edit the dispatch name to include the quadrat (I forgot about that until after I sent the dispatches).

![image](https://user-images.githubusercontent.com/6355854/171676467-cc087191-9d7f-48ff-b5c4-8ef3c94da5aa.png)

## 9. Sync FastField app to receive new dispatches before returnnign to field.

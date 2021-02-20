<div dir="rtl">
 
# מודלים סטוכסטיים

זהו אתר הגיטהאב עבור הקוד מהקורס "מודלים סטוכסטיים".
לכאן אעלה לפני או אחרי כל שיעור את הקוד שבו אשתמש בשביל לעשות סימולציות שונות.
לרוב, נכתוב פונקציות עזר לסימולציה ואז נבצע אותה באמצעותן.
נשתדל לרכז את התוצאות [באפליקציית שייני של הקורס](https://zztop.shinyapps.io/Stochastic/).
אפשר להפעיל את האפליקציה דרך Rstudio. לשם כך יש להריץ את הפקודה :
`shiny::runGitHub("Stochastic","blebedenko")` (שימו לב שצריך להתקין את הספריה `shiny` קודם באמצעות הרצת הפקודה `install.packages("shiny")`.

בהצלחה,
בוריס


## שיעור 1 - סימולציות בני הדייג
$$X_t$$
בשיעור 1 פתרנו תרגיל על דייג שמוציא דגים בקצב פואסוני ומשחרר אחוז מסוים מתוכם חזרה לים, באקראי.
נראה באמצעות סימולציה מהי התפלגות הדגים שהוא לוקח איתו.
כמו כן, נחקור את התפלגות המחסור היומי בדגים ונבין כמה זה עולה לנו.


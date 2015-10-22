
songlog.db tables structure

sqlite> select count(*) from users;
count(*)  
----------
41355 

sqlite> select * from users limit 1;
ID          NAME                                    
----------  ----------------------------------------
1           0000f88f8d76a238c251450913b0d070e4a77d19


sqlite> select count(*) from songs;
count(*)  
----------
3675      

sqlite> select * from songs limit 1;
ID          NAME                DUR         KEY         LOUD        MODE        TEMPO       TIMESIG   
----------  ------------------  ----------  ----------  ----------  ----------  ----------  ----------
1           SOWEZSI12A81C21CE6  194.87302   5           -8.403      0           165.006     1         


sqlite> select count(*) from learnlog;
count(*)  
----------
177992    

sqlite> select * from learnlog limit 1;
ID          USER_ID     SONG_ID     AMOUNT    
----------  ----------  ----------  ----------
1           14651       9           5         


sqlite> select count(*) from testlog;
count(*)  
----------
47125     

sqlite> select * from testlog limit 1;
ID          USER_ID     SONG_ID     AMOUNT    
----------  ----------  ----------  ----------
1           14651       10          1         


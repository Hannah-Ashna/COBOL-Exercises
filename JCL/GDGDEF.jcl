//USERID      JOB USERID,'HJ',
//            CLASS=S,
//            MSGCLASS=R,
//            NOTIFY=&SYSUID,
//            TIME=1440,
//            REGION=0M,
//            LINES=(9999,WARNING)
//*------------------------------------------------------------------**
//* YOU'LL NEED ACCESS TO IDCAMS TO BE ABLE TO DO THIS
//GDGDEF      EXEC PGM=IDCAMS
//SYSPRINT    DD SYSOUT=*
//SYSIN       DD *
 DEFINE GDG -
        (NAME(Insert your GDG Name here)-
        LIMIT(Insert max number of gens here) -
        NOEMPTY -
        SCRATCH -
       )

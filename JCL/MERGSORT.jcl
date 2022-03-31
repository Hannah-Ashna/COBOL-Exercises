//USERID      JOB USERID,'HJ',
//            CLASS=S,
//            MSGCLASS=R,
//            NOTIFY=&SYSUID,
//            TIME=1440,
//            REGION=0M,
//            LINES=(9999,WARNING)
//*------------------------------------------------------------------**
//* THIS IS FOR WHEN YOU WISH TO MERGE FILE A AND FILE B TO CREATE A NEW FILE
//SORT        EXEC PGM=SORT,REGION=6144K,  
//*
//* #INPUT FILES#                                                  
//SORTIN      DD DSN=(Insert file name here - FILE A),              
//            DISP=SHR                                     
//            DD DSN=(Insert file name here - FILE B),
//            DISP=SHR
//* THIS IS WHERE YOU SPECIFY THE DETAILS FOR HOW YOU WANT THE RECORDS TO BE SORTED
//SYSIN       DD   *                                       
         SORT FIELDS=(5,26,CH,A)
//* #OUTPUT FILES#                                         
//SORTOUT     DD  DSN=(Insert file name here - FILE C),  
//            DISP=(NEW,CATLG,DELETE),                     
//            SPACE=(CYL,(100,50),RLSE)                    
//SYSOUT      DD   SYSOUT=*                                                             

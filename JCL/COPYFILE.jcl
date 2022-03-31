//USERID      JOB USERID,'HJ',
//            CLASS=S,
//            MSGCLASS=R,
//            NOTIFY=&SYSUID,
//            TIME=1440,
//            REGION=0M,
//            LINES=(9999,WARNING)
//*------------------------------------------------------------------**
//* THIS IS FOR WHEN YOU WANT TO OVERWRITE FILE B'S CONTENTS WITH FILE A
//COPYFILE EXEC PGM=SORT,PARM='BMSG',                   
//              REGION=0M
//*
//SORTIN   DD DSN=(Insert file name here - FILE A),         
//            DISP=SHR                                 
//SORTOUT  DD DSN=(Insert file name here - FILE B),
//            DISP=(NEW,CATLG,DELETE),                 
//            UNIT=PERM,                               
//            SPACE=(CYL,(100,25),RLSE),               
//            DCB=(LRECL=35,RECFM=FB,BLKSIZE=0)
//* IF IT'S AN EXISTING FILE, COMMENT OUT THESE VALUES ABOVE AND JUST UNCOMMENT THE LINE BELOW
//*            DISP=SHR
//*
//SYSIN DD *                                           
  OPTION COPY                                           
//SYSPRINT DD SYSOUT=*                                 
//SYSOUT   DD SYSOUT=*                                 

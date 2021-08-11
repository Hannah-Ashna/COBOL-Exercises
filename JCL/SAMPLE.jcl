//USERID      JOB USERID,'HJ',
//            CLASS=S,
//            MSGCLASS=R,
//            NOTIFY=&SYSUID,
//            TIME=1440,
//            REGION=0M,
//            LINES=(9999,WARNING)
//*------------------------------------------------------------------**
//P10      EXEC PGM=EXPPROG3,                                           
//              REGION=8499K                                              
//*** Input files:
//FILEIN   DD DSN=USERID.CBLPROG1.INPUT1,                        
//            DISP=SHR
//*** Output files:
//FILEOUT  DD DSN=USERID.CBLPROG1.OUTPUT1,
//*           DISP=SHR
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=PERM,
//            SPACE=(TRK,(50,25),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//*
//IOMODIN  DD DSN=,                          
//            DISP=SHR                                                  
//IOSTBIN  DD DSN=,                          
//            DISP=SHR                                                  
//IOMODOUT DD SYSOUT=*                                                  
//IOSTBOUT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=*                                                  
//ABENDAID DD SYSOUT=*                                                  
//*

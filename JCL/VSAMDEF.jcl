//USERID      JOB USERID,'HJ',
//            CLASS=S,
//            MSGCLASS=R,
//            NOTIFY=&SYSUID,
//            TIME=1440,
//            REGION=0M,
//            LINES=(9999,WARNING)
//*------------------------------------------------------------------**
//DELDEF   EXEC PGM=IDCAMS,REGION=8M
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
    DELETE               USERID.FILE.VSAM
    DEFINE CLUSTER (NAME(USERID.FILE.VSAM                            )-
                   INDEXED                                            -
                   KEYS   (5,0)                                       -
                   RECSZ  (80,80)                                     -
                   SHR    (2,3))                                      -
                   DATA(CISZ   (26624)                                -
                        CYL    (10,10))
//*-------------------------------------------------------------------*
//*SORT FILE RECORDS INTO ASCENDING ORDER*
//SORT        EXEC PGM=SORT,REGION=0M
//SORTIN      DD DSN=[FILE TO SORT HERE],
//            DISP=SHR
//SYSIN       DD *
 SORT FIELDS=(1,5,CH,A)
 SUM FIELDS=NONE
//SORTOUT     DD DSN=[SORTED FILE OUTPUT HERE],
//*          DISP=SHR
//           DISP=(NEW,CATLG,CATLG),
//           UNIT=PERM,
//           SPACE=(TRK,(100,50),RLSE),
//           DCB=*.SORTIN
//SYSPRINT DD SYSOUT=*
//*-------------------------------------------------------------------*
//*COPY SORTED FILE RECORDS TO VSAM*
//LOADVSAM    EXEC PGM=IDCAMS
//*
//INDD        DD   DSN=[SORTED FILE OUTPUT HERE],
//             DISP=SHR
//OUTDD       DD   DSN= USERID.FILE.VSAM,
//             DISP=SHR
//SYSIN       DD *
   REPRO INFILE(INDD) -
         OUTFILE(OUTDD) -
   REPLACE
//SYSPRINT    DD   SYSOUT=*
//*-------------------------------------------------------------------*

//* =============================================
//*  JCL Comprehensive Demo - Time Warp Studio
//* =============================================
//DEMO     JOB (ACCT001),'TIME WARP DEMO',CLASS=A,
//             MSGCLASS=X,NOTIFY=&SYSUID
//*
//* --- Simple No-Op Step ---
//STEP1    EXEC PGM=IEFBR14
//*
//* --- Copy Step with Inline Data ---
//STEP2    EXEC PGM=IEBGENER
//SYSUT1   DD *
Hello World from JCL!
This is inline data being processed.
Time Warp Studio JCL Demo.
/*
//SYSUT2   DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSPRINT DD SYSOUT=*
//*
//* --- Dataset Allocation ---
//STEP3    EXEC PGM=IEFBR14
//NEWDATA  DD DSN=TIME.WARP.DEMO,DISP=(NEW,CATLG),
//            SPACE=(TRK,(5,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//*
//* --- Sort Step ---
//STEP4    EXEC PGM=SORT
//SORTIN   DD *
CHARLIE
ALICE
EVE
BOB
DIANA
/*
//SORTOUT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
/*
//*
//* --- Program with Parameters ---
//STEP5    EXEC PGM=MYPROG,PARM='HELLO WORLD'
//STEPLIB  DD DSN=MY.LOAD.LIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//*
//* --- JES2 Control ---
/*JOBPARM SYSAFF=*
//*
//* --- Conditional Execution ---
//STEP6    EXEC PGM=IEFBR14,COND=(0,NE,STEP1)
//*
//* --- Done Marker ---
//STEP7    EXEC PGM=MYPROG,PARM='===== DONE ====='
//STEPLIB  DD DSN=MY.LOAD.LIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//*
//

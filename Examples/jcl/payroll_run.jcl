//PAYRJOB  JOB (ACCT-PAY01,PAYROLL),'PAYROLL RUN',
//         CLASS=A,MSGCLASS=X,
//         NOTIFY=&SYSUID,
//         MSGLEVEL=(1,1),
//         TIME=(0,30)
//*
//*  ================================================================
//*  ACME CORPORATION PAYROLL PROCESSING JOB STREAM
//*  JOB:      PAYRJOB
//*  SCHEDULE: Every other Friday (bi-weekly)
//*  PURPOSE:  Full payroll: timecard validation, gross/net calc,
//*            check printing, GL posting, and payroll reports
//*  ================================================================
//*
//* =================================================================
//* STEP 1: DELETE PRIOR RUN WORK DATASETS
//* =================================================================
//CLEANUP  EXEC PGM=IEFBR14
//PRNTFILE  DD DSN=ACME.PAYROLL.REGISTER,
//             DISP=(MOD,DELETE,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(1,1))
//ERRFILE   DD DSN=ACME.PAYROLL.ERRORS,
//             DISP=(MOD,DELETE,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(1,1))
//CHKFILE   DD DSN=ACME.PAYROLL.CHECKS,
//             DISP=(MOD,DELETE,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(5,1))
//*
//* =================================================================
//* STEP 2: VALIDATE TIMECARDS — TIMVLD01
//*         Checks: hours within limits, valid job codes, dept codes
//*         Output: validated records to TIMWRK, errors to TIMERR
//* =================================================================
//TIMVLD   EXEC PGM=TIMVLD01,
//         COND=(0,NE)
//STEPLIB  DD DSN=ACME.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//DB2PLAN  DD DSN=ACME.DB2.PLANS(TIMVLD01),DISP=SHR
//PARMFILE DD DSN=ACME.PAYROLL.PARMS(PERIOD),DISP=SHR
//TIMERR   DD DSN=ACME.PAYROLL.TIMERR,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=20000)
//TIMWRK   DD DSN=ACME.PAYROLL.TIMWRK,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=20000)
//*
//* =================================================================
//* STEP 3: TIMECARD EDIT REPORT — IF ERRORS NOTIFY OPERATOR
//* =================================================================
//TMERPT   EXEC PGM=IEBGENER,
//         COND=(0,NE,TIMVLD)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=ACME.PAYROLL.TIMERR,DISP=SHR
//SYSUT2   DD SYSOUT=(A,INTRDR)
//*
//* =================================================================
//* STEP 4: RUN PAYROLL CALCULATION — PAYPRO01
//*         Reads EMPLOYEE + TIMECARD from DB2
//*         Writes PAYCHECK records to DB2
//*         Produces: PRNTFILE (register), ERRFILE (exceptions)
//* =================================================================
//PAYCALC  EXEC PGM=PAYPRO01,
//         COND=(0,NE)
//STEPLIB  DD DSN=ACME.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSABEND DD SYSOUT=*
//CEEOPTS  DD DSN=ACME.CEE.OPTIONS,DISP=SHR
//DB2PLAN  DD DSN=ACME.DB2.PLANS(PAYPRO01),DISP=SHR
//PRNTFILE DD DSN=ACME.PAYROLL.REGISTER,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(3,1),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//ERRFILE  DD DSN=ACME.PAYROLL.ERRORS,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=20000)
//SYSIN    DD *
PERIOD=2025122BI
/*
//*
//* =================================================================
//* STEP 5: CHECK — ABORT IF PAYROLL CALC FAILED
//* =================================================================
//PAYCHK   IF (PAYCALC.RC > 4) THEN
//NOTIFOP  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//NOTIFY   DD DSN=ACME.PAYROLL.FAILURE.NOTIFY,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(1,1))
//         ENDIF
//*
//* =================================================================
//* STEP 6: PRINT PAYROLL REGISTER — PAYRPRT
//* =================================================================
//PAYRPRT  EXEC PGM=IEBGENER,
//         COND=(4,LT,PAYCALC)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=ACME.PAYROLL.REGISTER,DISP=SHR
//SYSUT2   DD SYSOUT=(A,,(PAYROLL,REGISTER))
//*
//* =================================================================
//* STEP 7: GENERATE SQR PAYROLL REPORT — SQRRPAY
//* =================================================================
//SQRRPAY  EXEC PGM=SQRRUN,
//         COND=(4,LT,PAYCALC)
//STEPLIB  DD DSN=ACME.SQRLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SQRIN    DD DSN=ACME.SQR.SOURCE(PAYROLL),DISP=SHR
//SQROUT   DD DSN=ACME.REPORTS.PAYRPRT.&&TODAY,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//SQRPARM  DD *
-PERIOD_ID=2025122BI
-COMPANY_ID=ACME
-OUTPUT_TYPE=PDF
/*
//*
//* =================================================================
//* STEP 8: POST GL JOURNAL — GLPOST01
//*         Makes GL entries from PAYCHECK permanent (Posted_Flag=Y)
//* =================================================================
//GLPOST   EXEC PGM=GLPOST01,
//         COND=(4,LT,PAYCALC)
//STEPLIB  DD DSN=ACME.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//DB2PLAN  DD DSN=ACME.DB2.PLANS(GLPOST01),DISP=SHR
//JRNLRPT  DD DSN=ACME.GL.JOURNAL.RPTFILE,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FBA,LRECL=133)
//SYSIN    DD *
JOURNAL_ID=202512PAYP01
POST_DATE=2025-12-26
/*
//*
//* =================================================================
//* STEP 9: BACKUP PAYROLL DATASETS TO TAPE
//* =================================================================
//TAPEBKUP EXEC PGM=IEBCOPY,
//         COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  COPY OUTDD=TAPOUT,INDD=((REGIN,R))
/*
//TAPOUT   DD DSN=ACME.PAYROLL.ARCHIVE.&SYSDATE,
//            DISP=(NEW,CATLG,KEEP),
//            UNIT=CART,VOL=(,RETAIN,SER=PAYTAP),
//            LABEL=(,SL)
//REGIN    DD DSN=ACME.PAYROLL.REGISTER,DISP=SHR
//*
//* =================================================================
//* FINAL STEP: WRITE COMPLETION TRIGGER FOR DOWNSTREAM JOBS
//* =================================================================
//DONE     EXEC PGM=IEFBR14,
//         COND=(0,NE)
//TRIGGER  DD DSN=ACME.PAYROLL.COMPLETE.FLAG,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(1,1)),
//            DCB=(RECFM=FB,LRECL=80)
//*
//         IF (DONE.RC = 0) THEN
//NOTIFY   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
PAYROLL RUN 2025122BI COMPLETED SUCCESSFULLY
/*
//SYSUT2   DD SYSOUT=(A,INTRDR)
//         ENDIF
//*
//  END OF JOB STREAM - PAYRJOB

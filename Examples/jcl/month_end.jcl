//MNDENDJB JOB (ACME,MEND),'MONTH END CLOSE',CLASS=A,
//         MSGCLASS=X,NOTIFY=&SYSUID,MSGLEVEL=(1,1),
//         TIME=1440,REGION=0M
//*=================================================================
//* JOB:    MNDENDJB
//* DESC:   ACME CORPORATION MONTH-END CLOSING JOB STREAM
//* RUNS:   Last business day of each month
//* STEPS:  01-LOCKGL  Lock GL for period (prevent new postings)
//*         02-ACCRUAL Post month-end accruals
//*         03-DEPRCTN Post depreciation entries
//*         04-INVADJ  Inventory cost adjustments
//*         05-INTCALC Calculate interest on AR/AP
//*         06-CLOSE   Close fiscal period
//*         07-TRLBAL  Generate trial balance
//*         08-GLRPT   GL activity report
//*         09-ARAGING AR aging analysis
//*         10-BALSHT  Balance sheet
//*         11-INCSTMT Income statement
//*         12-DISTRIB Distribute reports
//*         13-ARCHIVE Archive period data
//*         14-NOTIFY  Completion notification
//*=================================================================
//*
//*─── STEP 01: LOCK GENERAL LEDGER PERIOD ─────────────────────────
//LOCKGL   EXEC PGM=LOCKPER1,
//         PARM='LOCK,CURRENT'
//STEPLIB  DD DSN=ACME.PROD.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=X
//SYSPRINT DD SYSOUT=X
//LOCKRPT  DD DSN=ACME.MEND.LOCKREPT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//*
//*─── STEP 02: POST MONTH-END ACCRUALS ────────────────────────────
//ACCRUAL  EXEC PGM=ACCRPST1,
//         COND=(0,NE,LOCKGL)
//STEPLIB  DD DSN=ACME.PROD.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=X
//ACCRLIN  DD DSN=ACME.MEND.ACCRUALS,DISP=SHR
//GLJRNL   DD DSN=ACME.PROD.GLJRNL,DISP=SHR
//SYSPRINT DD DSN=ACME.MEND.ACCRRPT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//*
//*─── STEP 03: POST DEPRECIATION ──────────────────────────────────
//DEPRCTN  EXEC PGM=DEPRPST1,
//         COND=(0,NE,LOCKGL)
//STEPLIB  DD DSN=ACME.PROD.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=X
//ASSETMST DD DSN=ACME.PROD.ASSETS,DISP=SHR
//SYSPRINT DD DSN=ACME.MEND.DEPRRPT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//         IF (DEPRCTN.RC > 4) THEN
//*─── ABORT: DEPRECIATION FAILED ─────────────────────────────────
//DEPFAIL  EXEC PGM=IEBGENER
//SYSUT1   DD *
DEPRECIATION POST FAILED — MONTH END ABORTED
/*
//SYSUT2   DD SYSOUT=(A,INTRDR)
//SYSIN    DD DUMMY
//         ENDIF
//*
//*─── STEP 04: INVENTORY COST ADJUSTMENTS ─────────────────────────
//INVADJ   EXEC PGM=INVADJ01,
//         COND=((0,NE,LOCKGL),(8,LE,DEPRCTN))
//STEPLIB  DD DSN=ACME.PROD.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=X
//INVADJIN DD DSN=ACME.MEND.INVADJ,DISP=SHR
//SYSPRINT DD DSN=ACME.MEND.INVADJ.RPT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//*
//*─── STEP 05: INTEREST CALCULATION ──────────────────────────────
//INTCALC  EXEC PGM=INTCAL01,
//         COND=((0,NE,LOCKGL),(8,LE,DEPRCTN))
//STEPLIB  DD DSN=ACME.PROD.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=X
//CUSTMST  DD DSN=ACME.PROD.CUSTOMER,DISP=SHR
//VENRMST  DD DSN=ACME.PROD.VENDOR,DISP=SHR
//SYSPRINT DD DSN=ACME.MEND.INTRPT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//*
//*─── STEP 06: CLOSE FISCAL PERIOD ────────────────────────────────
//CLOSE    EXEC PGM=PERCLOSE,
//         COND=((0,NE,LOCKGL),(8,LE,ACCRUAL),(8,LE,DEPRCTN))
//STEPLIB  DD DSN=ACME.PROD.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=X
//SYSPRINT DD DSN=ACME.MEND.CLOSERPT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//*
//*─── STEP 07: TRIAL BALANCE ───────────────────────────────────────
//TRLBAL   EXEC PGM=GLPOST01,
//         COND=(0,NE,CLOSE)
//STEPLIB  DD DSN=ACME.PROD.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=X
//RPTOUT   DD DSN=ACME.MEND.TRLBAL,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(1,1)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//*
//*─── STEP 08: GL ACTIVITY REPORT (SQR) ───────────────────────────
//GLRPT    EXEC PGM=SQRRUN,
//         PARM='gl_report ACME &PERIOD TYPE=MEND',
//         COND=(4,LT,TRLBAL)
//STEPLIB  DD DSN=ACME.PROD.SQRLIB,DISP=SHR
//SQRIN    DD DSN=ACME.SQR.GLRPT,DISP=SHR
//SQROUT   DD DSN=ACME.MEND.GLRPT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//SYSPRINT DD SYSOUT=X
//*
//*─── STEP 09: AR AGING ANALYSIS ──────────────────────────────────
//ARAGING  EXEC PGM=SQRRUN,
//         PARM='ar_aging ACME &PERIOD',
//         COND=(4,LT,CLOSE)
//STEPLIB  DD DSN=ACME.PROD.SQRLIB,DISP=SHR
//SQRIN    DD DSN=ACME.SQR.ARAGING,DISP=SHR
//SQROUT   DD DSN=ACME.MEND.ARAGING,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//SYSPRINT DD SYSOUT=X
//*
//*─── STEP 10: BALANCE SHEET ───────────────────────────────────────
//BALSHT   EXEC PGM=SQRRUN,
//         PARM='balance_sheet ACME &PERIOD',
//         COND=(4,LT,CLOSE)
//STEPLIB  DD DSN=ACME.PROD.SQRLIB,DISP=SHR
//SQRIN    DD DSN=ACME.SQR.BALSHT,DISP=SHR
//SQROUT   DD DSN=ACME.MEND.BALSHT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//SYSPRINT DD SYSOUT=X
//*
//*─── STEP 11: INCOME STATEMENT ────────────────────────────────────
//INCSTMT  EXEC PGM=SQRRUN,
//         PARM='income_stmt ACME &PERIOD',
//         COND=(4,LT,CLOSE)
//STEPLIB  DD DSN=ACME.PROD.SQRLIB,DISP=SHR
//SQRIN    DD DSN=ACME.SQR.INCSTMT,DISP=SHR
//SQROUT   DD DSN=ACME.MEND.INCSTMT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//SYSPRINT DD SYSOUT=X
//*
//*─── STEP 12: DISTRIBUTE REPORTS ─────────────────────────────────
//DISTRIB  EXEC PGM=RPTDIST1,
//         PARM='MONTHLY,EMAIL,PRINT',
//         COND=((4,LT,TRLBAL),(4,LT,ARAGING))
//STEPLIB  DD DSN=ACME.PROD.LOADLIB,DISP=SHR
//RPTLIST  DD DSN=ACME.MEND.TRLBAL,DISP=SHR
//         DD DSN=ACME.MEND.GLRPT,DISP=SHR
//         DD DSN=ACME.MEND.ARAGING,DISP=SHR
//         DD DSN=ACME.MEND.BALSHT,DISP=SHR
//         DD DSN=ACME.MEND.INCSTMT,DISP=SHR
//DISTCFG  DD DSN=ACME.CONFIG.DISTLIST,DISP=SHR
//SYSPRINT DD SYSOUT=X
//SYSOUT   DD SYSOUT=*
//*
//*─── STEP 13: ARCHIVE PERIOD DATA ────────────────────────────────
//ARCHIVE  EXEC PGM=IEBCOPY,
//         COND=(4,LT,CLOSE)
//SYSUT1   DD DSN=ACME.PROD.GLJRNL,DISP=SHR
//SYSUT2   DD DSN=ACME.ARCHIVE.GL.&PERIOD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=TAPE,VOL=SER=ARCH01,
//            LABEL=(1,SL),
//            DCB=(ACME.PROD.GLJRNL)
//SYSIN    DD DUMMY
//SYSPRINT DD SYSOUT=X
//*
//*─── STEP 14: COMPLETION NOTIFICATION ────────────────────────────
//NOTIFY   EXEC PGM=IEBGENER,
//         COND=(0,NE,CLOSE)
//SYSUT1   DD *
//JOBNAME JOB (ACME,NOTIFY),'MONTH END NOTIFY',CLASS=A
//STEP1   EXEC PGM=SENDMAIL
//MAILTO  DD *
TO: finance@acme.com
TO: controller@acme.com
TO: cfo@acme.com
SUBJECT: Month-End Close Complete
BODY: Month-end processing for period &PERIOD completed successfully.
      Reports have been distributed to all recipients.
      Total runtime: See job log MNDENDJB.
/*
//SYSUT2   DD SYSOUT=(A,INTRDR)
//SYSIN    DD DUMMY
//SYSPRINT DD SYSOUT=X
//*
//*─── END OF JOB MNDENDJB ─────────────────────────────────────────

//DLYBTCH  JOB (ACCT-OPS01,BATCH),'DAILY BATCH RUN',
//         CLASS=B,MSGCLASS=X,
//         NOTIFY=&SYSUID,
//         MSGLEVEL=(1,1),
//         TIME=(1,0)
//*
//*  ================================================================
//*  ACME CORPORATION — DAILY BATCH PROCESSING JOB STREAM
//*  JOB:     DLYBTCH
//*  SCHEDULE: Weekdays 01:00 AM (scheduled via OPC/TWS)
//*  PURPOSE: Nightly processing including order fulfillment,
//*           inventory updates, AR aging, and daily GL summaries
//*  ================================================================
//*
//* =================================================================
//* STEP 01: HOUSEKEEPING — DELETE YESTERDAY'S WORK FILES
//* =================================================================
//HSKP01   EXEC PGM=IEFBR14
//ORDWRK   DD DSN=ACME.DAILY.ORDWRK,
//            DISP=(MOD,DELETE,DELETE),UNIT=SYSDA,SPACE=(TRK,(1,1))
//INVWRK   DD DSN=ACME.DAILY.INVWRK,
//            DISP=(MOD,DELETE,DELETE),UNIT=SYSDA,SPACE=(TRK,(1,1))
//ARWRK    DD DSN=ACME.DAILY.ARWRK,
//            DISP=(MOD,DELETE,DELETE),UNIT=SYSDA,SPACE=(TRK,(1,1))
//*
//* =================================================================
//* STEP 02: ORDER FULFILLMENT — ORDFILL1
//*          Check inventory, allocate stock for open orders.
//*          Ships orders with full stock. Partial ships partial.
//* =================================================================
//ORDFILL  EXEC PGM=ORDFILL1,COND=(0,NE)
//STEPLIB  DD DSN=ACME.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//DB2PLAN  DD DSN=ACME.DB2.PLANS(ORDFILL1),DISP=SHR
//PRNTFILE DD DSN=ACME.DAILY.ORDSHIP.RPT,
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//SYSIN    DD *
COMPANY=ACME
WAREHOUSE=WH01 WH02
PRIORITY=Y
/*
//*
//* =================================================================
//* STEP 03: INVENTORY TRANSACTION POSTING — INVPST01
//*          Post all inventory receipts/issues queued from the day.
//*          Updates QTY_ON_HAND, creates transaction history.
//* =================================================================
//INVPOST  EXEC PGM=INVPST01,COND=(4,LT,ORDFILL)
//STEPLIB  DD DSN=ACME.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//DB2PLAN  DD DSN=ACME.DB2.PLANS(INVPST01),DISP=SHR
//INVTRANS DD DSN=ACME.INV.PENDING.TRANS,DISP=SHR
//INVAUDIT DD DSN=ACME.DAILY.INVAUDIT,
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200)
//*
//* =================================================================
//* STEP 04: REORDER ALERT REPORT — REORDPT1
//*          Find items below reorder point. Generate purchase
//*          order suggestions. Print reorder report.
//* =================================================================
//REORDER  EXEC PGM=REORDPT1,COND=(4,LT,INVPOST)
//STEPLIB  DD DSN=ACME.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//DB2PLAN  DD DSN=ACME.DB2.PLANS(REORDPT1),DISP=SHR
//PRNTFILE DD DSN=ACME.DAILY.REORDER.RPT,
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FBA,LRECL=133)
//SYSOUT   DD SYSOUT=*
//*
//* =================================================================
//* STEP 05: ACCOUNTS RECEIVABLE AGING — ARAGING1
//*          Compute AR aging buckets: current, 30, 60, 90, 90+
//*          Update AR_AGE_BALANCE table. Generate aging report.
//* =================================================================
//ARAGING  EXEC PGM=ARAGING1,COND=(0,NE)
//STEPLIB  DD DSN=ACME.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//DB2PLAN  DD DSN=ACME.DB2.PLANS(ARAGING1),DISP=SHR
//PRNTFILE DD DSN=ACME.DAILY.ARAGE.RPT,
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FBA,LRECL=133)
//SYSIN    DD *
AS_OF_DATE=TODAY
COMPANY=ACME
INCLUDE_CREDIT=Y
/*
//*
//* =================================================================
//* STEP 06: DAILY GL SUMMARY — GLSUM01
//*          Summarize today's journal activity by account.
//*          Update nightly GL period balances.
//* =================================================================
//GLSUMRY  EXEC PGM=GLSUM01,COND=(0,NE)
//STEPLIB  DD DSN=ACME.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//DB2PLAN  DD DSN=ACME.DB2.PLANS(GLSUM01),DISP=SHR
//GLJRNAL  DD DSN=ACME.GL.JOURNAL.DAILY,DISP=SHR
//GLRPT    DD DSN=ACME.DAILY.GL.RPT,
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FBA,LRECL=133)
//*
//* =================================================================
//* STEP 07: DAILY MANAGEMENT REPORTS — SQR BATCH
//*          Run three SQR reports: Order Status, AR Summary, Inv Value
//* =================================================================
//SQRRPTS  EXEC PGM=SQRRUN,COND=(4,LT,GLSUMRY)
//STEPLIB  DD DSN=ACME.SQRLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SQRIN    DD *
RUNCTL_REPORT=ORDRPT
RUNCTL_REPORT=ARSUMRY
RUNCTL_REPORT=INVVAL
/*
//SQROUT   DD SYSOUT=(A,,(DAILY,MGMT-REPORTS))
//*
//* =================================================================
//* STEP 08: EMAIL DISTRIBUTION — Send reports to managers
//* =================================================================
//EMAILRPT EXEC PGM=MAILDIST,COND=(4,LT,SQRRPTS)
//STEPLIB  DD DSN=ACME.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//MAILCFG  DD DSN=ACME.MAIL.CONFIG,DISP=SHR
//MAILIST  DD DSN=ACME.RPT.DISTRIB.LIST(DAILY),DISP=SHR
//RPTFILES DD DSN=ACME.DAILY.REORDER.RPT,DISP=SHR
//         DD DSN=ACME.DAILY.ARAGE.RPT,DISP=SHR
//         DD DSN=ACME.DAILY.GL.RPT,DISP=SHR
//*
//* =================================================================
//* STEP 09: UPDATE AUDIT LOG — Record successful run
//* =================================================================
//AUDITLOG EXEC PGM=AUDTLOG1,COND=(0,NE)
//STEPLIB  DD DSN=ACME.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//DB2PLAN  DD DSN=ACME.DB2.PLANS(AUDTLOG1),DISP=SHR
//SYSIN    DD *
JOB_NAME=DLYBTCH
JOB_TYPE=DAILY_BATCH
STATUS=COMPLETE
/*
//*
//* =================================================================
//* STEP 10: IF ANY STEP FAILED, SEND ALERT
//* =================================================================
//         IF (ORDFILL.RC   > 4 OR
//             INVPOST.RC   > 4 OR
//             ARAGING.RC   > 4 OR
//             GLSUMRY.RC   > 4) THEN
//ALERTOPS EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
ALERT: ACME DAILY BATCH FAILED - CHECK JES2 SYSLOG FOR DETAILS
JOBS: DLYBTCH
TIME: CHECK SPOOL
ACTION: CONTACT ON-CALL DBA AND OPERATIONS MANAGER
/*
//SYSUT2   DD SYSOUT=(A,INTRDR)
//         ENDIF
//*
// END OF DAILY BATCH JOB STREAM

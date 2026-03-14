//DATA-MIGRATION  JOB (ACCT001),'QUARTERLY MIGRATION',
//                CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID,
//                MSGLEVEL=(1,1)
//*
//* ══════════════════════════════════════
//*   📦 Quarterly Data Migration Job
//* ══════════════════════════════════════
//* Demonstrates: SET, IF/THEN/ELSE/ENDIF, PROC/PEND,
//*               multi-step job with conditional execution
//*
//* ── Set symbolic parameters ──
// SET QUARTER='Q1'
// SET YEAR='2025'
// SET ENV='PROD'
// SET MAXRC=0
//*
//* ══════════════════════════════════════
//*   Step 1: Validate source datasets
//* ══════════════════════════════════════
//VALIDATE EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES('SRC.DATA.&YEAR..&QUARTER..MASTER') -
          ALL
  LISTCAT ENTRIES('SRC.DATA.&YEAR..&QUARTER..DETAIL') -
          ALL
/*
//*
//* ══════════════════════════════════════
//*   Step 2: Backup existing target data
//* ══════════════════════════════════════
//BACKUP   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=TGT.DATA.&YEAR..MASTER,
//            DISP=SHR
//SYSUT2   DD DSN=TGT.DATA.&YEAR..MASTER.BACKUP,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//SYSIN    DD DUMMY
//*
//* ── Conditional: Check environment ──
// IF (&ENV EQ 'PROD') THEN
//*
//* ══════════════════════════════════════
//*   Step 3: Sort source data (PROD only)
//* ══════════════════════════════════════
//SORTDATA EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=SRC.DATA.&YEAR..&QUARTER..MASTER,
//            DISP=SHR
//SORTOUT  DD DSN=&&SORTED,
//            DISP=(NEW,PASS),
//            UNIT=SYSDA,
//            SPACE=(CYL,(100,20),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A,11,8,CH,A)
  SUM FIELDS=NONE
/*
//*
//* ══════════════════════════════════════
//*   Step 4: Transform and load data
//* ══════════════════════════════════════
//LOADDATA EXEC PGM=MIGUTIL
//STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//INPUT    DD DSN=&&SORTED,DISP=(OLD,DELETE)
//OUTPUT   DD DSN=TGT.DATA.&YEAR..MASTER,
//            DISP=OLD
//PARMS    DD *
  MIGRATE QUARTER=&QUARTER YEAR=&YEAR
  VALIDATE CHECKSUMS=YES
  COMMIT INTERVAL=1000
/*
//*
// ELSE
//*   ── Dev/Test: Simple copy without sort ──
//DEVCOPY  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=SRC.DATA.&YEAR..&QUARTER..MASTER,
//            DISP=SHR
//SYSUT2   DD DSN=TGT.DATA.&YEAR..MASTER,
//            DISP=OLD
//SYSIN    DD DUMMY
//*
// ENDIF
//*
//* ══════════════════════════════════════
//*   Step 5: Verify row counts
//* ══════════════════════════════════════
//VERIFY   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=TGT.DATA.&YEAR..MASTER,
//            DISP=SHR
//SYSIN    DD *
  PRINT INFILE(INFILE) -
        COUNT(5) -
        CHARACTER
/*
//*
//* ── Inline Proc: Cleanup temporary datasets ──
//CLEANUP  PROC DSN=
//DELSTEP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE '&DSN' PURGE
  SET MAXCC = 0
/*
//         PEND
//*
//* ── Execute cleanup proc ──
//CLEAN1   EXEC CLEANUP,DSN='TGT.DATA.&YEAR..MASTER.BACKUP'
//*
//* ══════════════════════════════════════
//*   Step 6: Generate completion report
//* ══════════════════════════════════════
//REPORT   EXEC PGM=IKJEFT01
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  SEND 'DATA MIGRATION &YEAR &QUARTER COMPLETE - RC=&MAXRC' +
       USER(ADMIN)
/*
//*
// IF (RC GT 4) THEN
//NOTIFY   EXEC PGM=IKJEFT01
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  SEND 'WARNING: MIGRATION HAD ERRORS - CHECK JOB LOG' +
       USER(ONCALL)
/*
// ENDIF
//*
//* ═══════════════════════════════
//*   ✅ End of Migration Job
//* ═══════════════════════════════
//

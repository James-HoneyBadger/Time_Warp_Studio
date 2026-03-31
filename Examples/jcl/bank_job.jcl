//BANKJOB  JOB (ACCT),'BANK DEMO',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID
//STEP1    EXEC DSNUPROC
//SYSIN    DD *
  -- Run SQL schema setup
  @/home/james/Time_Warp_Studio/Examples/sql/bank_schema.sql
/*
//STEP2    EXEC COBRUN
//COBOLPGM DD DSN=/home/james/Time_Warp_Studio/Examples/cobol/bank_account_manager.cob
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
  * CICS screen simulation handled by /Examples/cics/bank_transaction.cics
/*

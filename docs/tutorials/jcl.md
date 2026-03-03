# JCL Programming Tutorial

JCL (Job Control Language) is IBM's language for submitting batch jobs on z/OS mainframes. Created in the 1960s, it defines what programs run, what data they use, and how to allocate resources.

## Overview

JCL tells the mainframe's **Job Entry Subsystem (JES)**:
1. Who you are and billing info (JOB card)
2. What program to execute (EXEC card)
3. What files/data sets to use (DD cards)

## Basic Structure

```jcl
//MYJOB   JOB (ACCT),'MY NAME',CLASS=A,MSGCLASS=X
//STEP1   EXEC PGM=IEFBR14
//SYSOUT  DD  SYSOUT=*
```

Every JCL statement begins with `//`. Comments start with `//*`.

## The JOB Card

```jcl
//WARPJOB JOB (DEPT01,PROJ001),
//             'TIME WARP STUDIO',
//             CLASS=A,
//             MSGCLASS=X,
//             NOTIFY=&SYSUID
```

| Parameter | Meaning |
|-----------|---------|
| `CLASS=A` | Job class / priority |
| `MSGCLASS=X` | Output destination |
| `NOTIFY=` | Notify when complete |
| `REGION=` | Memory limit |
| `TIME=` | CPU time limit |

## The EXEC Card

```jcl
//STEP1   EXEC PGM=IEFBR14          ← run a program
//STEP2   EXEC PROC=MYBATCH         ← call a procedure
//STEP3   EXEC PGM=IKJEFT01,        ← TSO with parms
//             PARM='%MYCLG'
```

## The DD Card (Data Definition)

```jcl
//         DD statements define files / data sets

//INPUT   DD  DSN=MY.DATA.FILE,
//            DISP=SHR
//OUTPUT  DD  DSN=MY.OUT.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSOUT  DD  SYSOUT=*              ← print to JES spool
//SYSIN   DD  *                     ← inline data follows
    SORT FIELDS=(1,10,CH,A)
/*                                  ← end of inline data
```

### DISP Options

| DISP | Create | Normal | Abnormal |
|------|--------|--------|----------|
| `(NEW,CATLG,DELETE)` | Create | Catalog | Delete |
| `(OLD,KEEP)` | — | Keep | Keep |
| `SHR` | — | Shared read | Keep |
| `MOD` | Append | Keep | Keep |

## Hello World Job

```jcl
//*  PRINT A MESSAGE USING IEBGENER
//HELLOJOB JOB (0000),'HELLO WORLD',CLASS=A
//STEP1    EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DUMMY
//SYSUT2   DD  SYSOUT=*
//SYSUT1   DD  *
Hello from JCL - Time Warp Studio!
This is a batch job running on the mainframe.
/*
```

## Sorting with DFSORT

```jcl
//SORTJOB  JOB (ACCT),'SORT DEMO',CLASS=A
//SORT1    EXEC PGM=SORT
//SYSOUT   DD  SYSOUT=*
//SORTIN   DD  DSN=INPUT.DATA,DISP=SHR
//SORTOUT  DD  DSN=OUTPUT.SORTED,
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(CYL,(2,1))
//SYSIN    DD  *
  SORT FIELDS=(1,20,CH,A,21,8,ZD,D)
  OMIT  COND=(30,1,CH,EQ,C'X')
  SUM   FIELDS=(40,8,ZD)
/*
```

## Compile and Link (COBOL)

```jcl
//COBLJOB  JOB (ACCT),'COBOL BUILD',CLASS=A
//*
//COMPILE  EXEC PGM=IGYCRCTL,
//             PARM='OBJECT,NODECK'
//STEPLIB  DD  DSN=IGY.V6R1M0.SIGYCOMP,DISP=SHR
//SYSIN    DD  DSN=MY.COBOL.SOURCE(MYPGM),DISP=SHR
//SYSLIN   DD  DSN=&&OBJ,DISP=(,PASS),
//             SPACE=(TRK,(5,5))
//SYSPRINT DD  SYSOUT=*
//*
//LKED     EXEC PGM=HEWL,
//             PARM='XREF,LIST,LET'
//SYSLIN   DD  DSN=&&OBJ,DISP=(OLD,DELETE)
//SYSLMOD  DD  DSN=MY.LOAD.LIB(MYPGM),
//             DISP=(SHR)
//SYSPRINT DD  SYSOUT=*
```

## Conditional Execution

```jcl
//STEP1   EXEC PGM=MYPGM1
//STEP2   EXEC PGM=MYPGM2,
//             COND=(0,NE,STEP1)    ← only if STEP1 RC = 0
//STEP3   EXEC PGM=MYPGM3,
//             COND=EVEN            ← always run
//STEP4   EXEC PGM=CLEANUP,
//             COND=ONLY            ← only if job abended
```

## Further Reading

- [Examples/jcl/](../Examples/jcl/) — 5 JCL example programs
- [Language Guide: JCL](LANGUAGE_GUIDE.md#jcl)

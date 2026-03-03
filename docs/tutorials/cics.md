# CICS Programming Tutorial

CICS (Customer Information Control System) is IBM's transaction processing middleware for z/OS. Created in 1968, CICS processes billions of transactions daily — ATMs, airline bookings, banking, and more.

## What is CICS?

CICS provides:
- **Transaction management**: Start/stop transactions by name (4-char TRANSIDs)
- **Terminal I/O**: Send/receive 3270 terminal screens
- **File services**: Read/write VSAM datasets
- **Queue management**: Temporary storage, transient data
- **Inter-system communication**: Connect multiple CICS regions

## Hello World

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOWLD.

       PROCEDURE DIVISION.
           EXEC CICS
               SEND TEXT FROM('Hello from CICS!')
               LENGTH(17)
               ERASE
               FREEKB
           END-EXEC

           EXEC CICS RETURN END-EXEC.
```

## Basic EXEC CICS Commands

```cobol
*  Send a map (BMS screen) to the terminal
           EXEC CICS
               SEND MAP('MAINMAP')
               MAPSET('MYMAPSET')
               ERASE
               FREEKB
           END-EXEC

*  Receive input from the map
           EXEC CICS
               RECEIVE MAP('MAINMAP')
               MAPSET('MYMAPSET')
               INTO(WS-COMMAREA)
           END-EXEC

*  Send and then return (end transaction)
           EXEC CICS RETURN END-EXEC
```

## Handling Conditions

```cobol
       PROCEDURE DIVISION.
           EXEC CICS
               READ FILE('CUSTFILE')
               INTO(WS-CUSTOMER-RECORD)
               RIDFLD(WS-CUST-ID)
               RESP(WS-RESP)
               RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM DISPLAY-RECORD
               WHEN DFHRESP(NOTFND)
                   EXEC CICS
                       SEND TEXT FROM('Record not found')
                       LENGTH(16)
                   END-EXEC
               WHEN OTHER
                   EXEC CICS ABEND ABCODE('MERR') END-EXEC
           END-EVALUATE
```

## VSAM File Operations

```cobol
*  READ  — retrieve by key
           EXEC CICS
               READ FILE('ACCOUNTS')
               INTO(WS-ACCT-REC)
               RIDFLD(WS-ACCT-NUM)
               UPDATE
           END-EXEC

*  REWRITE — update after READ UPDATE
           EXEC CICS
               REWRITE FILE('ACCOUNTS')
               FROM(WS-ACCT-REC)
               LENGTH(LENGTH OF WS-ACCT-REC)
           END-EXEC

*  WRITE — add new record
           EXEC CICS
               WRITE FILE('ACCOUNTS')
               FROM(WS-ACCT-REC)
               RIDFLD(WS-ACCT-NUM)
           END-EXEC

*  DELETE — remove a record
           EXEC CICS
               DELETE FILE('ACCOUNTS')
               RIDFLD(WS-ACCT-NUM)
           END-EXEC

*  STARTBR + READNEXT + ENDBR — browse (scan)
           EXEC CICS
               STARTBR FILE('ACCOUNTS')
               RIDFLD(WS-START-KEY)
           END-EXEC
           PERFORM UNTIL WS-DONE = 'Y'
               EXEC CICS
                   READNEXT FILE('ACCOUNTS')
                   INTO(WS-ACCT-REC)
                   RIDFLD(WS-ACCT-NUM)
                   RESP(WS-RESP)
               END-EXEC
               IF WS-RESP = DFHRESP(ENDFILE)
                   MOVE 'Y' TO WS-DONE
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('ACCOUNTS') END-EXEC
```

## Temporary Storage (TSQUEUE)

```cobol
*  Write to TS queue
           EXEC CICS
               WRITEQ TS QUEUE('MYQUEUE')
               FROM(WS-DATA)
               LENGTH(WS-LEN)
               ITEM(WS-ITEM-NUM)
           END-EXEC

*  Read from TS queue
           EXEC CICS
               READQ TS QUEUE('MYQUEUE')
               INTO(WS-DATA)
               LENGTH(WS-LEN)
               ITEM(1)
           END-EXEC

*  Delete TS queue
           EXEC CICS DELETEQ TS QUEUE('MYQUEUE') END-EXEC
```

## Program Linking

```cobol
*  LINK — call another program (synchronous)
           EXEC CICS
               LINK PROGRAM('MYPGM2')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC

*  XCTL — transfer control (like a jump, no return)
           EXEC CICS
               XCTL PROGRAM('NEXTPGM')
               COMMAREA(WS-COMMAREA)
           END-EXEC

*  START — start a new transaction
           EXEC CICS
               START TRANSID('TRN1')
               FROM(WS-DATA)
               LENGTH(WS-LEN)
               AFTER SECONDS(5)
           END-EXEC
```

## The COMMAREA

```cobol
       WORKING-STORAGE SECTION.
       01 WS-COMMAREA.
          05 WS-TRAN-COUNT  PIC 9(5).
          05 WS-USER-ID     PIC X(8).

       LINKAGE SECTION.
       01 DFHCOMMAREA       PIC X(13).

       PROCEDURE DIVISION.
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
           END-IF

           ADD 1 TO WS-TRAN-COUNT

           EXEC CICS
               RETURN TRANSID(EIBTRNID)
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
```

## EIB — Execute Interface Block

| Field | Contents |
|-------|----------|
| `EIBTRNID` | Current transaction ID |
| `EIBTIME` | Time (HHMMSS integer) |
| `EIBDATE` | Date (CYYDDD format) |
| `EIBTERMID` | Terminal ID |
| `EIBCALEN` | COMMAREA length |
| `EIBRESP` | Last response code |

## Further Reading

- [Examples/cics/](../Examples/cics/) — 5 CICS example programs
- [Language Guide: CICS](LANGUAGE_GUIDE.md#cics)

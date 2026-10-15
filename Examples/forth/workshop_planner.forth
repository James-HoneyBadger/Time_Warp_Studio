\ COMMUNITY WORKSHOP RESOURCE PLANNER
\ Budget model, materials ledger, target check, and reserve plan

VARIABLE BUDGET
VARIABLE TABLES
VARIABLE CHAIRS
VARIABLE KITS
VARIABLE SIGNAGE
VARIABLE TOTAL
VARIABLE RESERVE

: LOAD-PLAN
  2400 BUDGET !
  12 TABLES !
  48 CHAIRS !
  36 KITS !
  8 SIGNAGE ! ;

: CALCULATE-TOTAL
  TABLES @ 45 *
  CHAIRS @ 12 * +
  KITS @ 18 * +
  SIGNAGE @ 25 * +
  TOTAL ! ;

: REPORT-ITEMS
  CR ." MATERIAL ESTIMATE" CR
  ." Tables: " TABLES @ . ." x 45 credits = " TABLES @ 45 * . CR
  ." Chairs: " CHAIRS @ . ." x 12 credits = " CHAIRS @ 12 * . CR
  ." Learning kits: " KITS @ . ." x 18 credits = " KITS @ 18 * . CR
  ." Sign boards: " SIGNAGE @ . ." x 25 credits = " SIGNAGE @ 25 * . CR ;

: REPORT-BUDGET
  CR ." PROJECT SUMMARY" CR
  ." Budget ceiling: " BUDGET @ . CR
  ." Planned spend:  " TOTAL @ . CR
  BUDGET @ TOTAL @ - RESERVE !
  ." Reserve after purchase: " RESERVE @ . CR
  RESERVE @ 0 < IF
    ." Status: over budget. Reduce quantities before ordering." CR
  ELSE
    ." Status: within budget. Reserve is available for repairs." CR
  THEN ;

: CAPACITY-REPORT
  CR ." WORKSHOP CAPACITY" CR
  ." Planned seats: " CHAIRS @ . CR
  CHAIRS @ 4 / ." Small-group teams: " . CR
  ." Learning kits support " KITS @ . ." learners." CR ;

: DRAW-BUDGET-BAR
  CR ." Spend marker: "
  TOTAL @ 100 * BUDGET @ / 0 DO
    35 EMIT
  LOOP
  CR ;

CR ." ============================================================" CR
." COMMUNITY WORKSHOP | RESOURCE PLANNER" CR
." Inventory estimate and budget review" CR
." ============================================================" CR

LOAD-PLAN
CALCULATE-TOTAL
REPORT-ITEMS
REPORT-BUDGET
CAPACITY-REPORT
DRAW-BUDGET-BAR
CR ." Planner complete. Adjust LOAD-PLAN to test another event." CR

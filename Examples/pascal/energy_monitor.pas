{ NEIGHBORHOOD ENERGY MONITOR }
{ Room usage, cost forecast, peak detection, and savings plan }

program EnergyMonitor;

const
  N = 6;
  RATE = 0.18;
  TARGET = 900;

var
  ZoneName: array[1..6] of string;
  KilowattHours: array[1..6] of integer;
  Cost: array[1..6] of real;
  i, TotalUsage, PeakZone, AboveTarget: integer;
  TotalCost, ProjectedSavings, AverageUsage: real;

begin
  ZoneName[1] := "Studio Lab";
  ZoneName[2] := "Learning Room";
  ZoneName[3] := "Media Corner";
  ZoneName[4] := "Office";
  ZoneName[5] := "Workshop";
  ZoneName[6] := "Shared Hall";
  KilowattHours[1] := 286;
  KilowattHours[2] := 174;
  KilowattHours[3] := 132;
  KilowattHours[4] := 96;
  KilowattHours[5] := 241;
  KilowattHours[6] := 118;

  writeln("============================================================");
  writeln("  NEIGHBORHOOD ENERGY MONITOR");
  writeln("  Usage forecast | Cost model | Efficiency actions");
  writeln("============================================================");
  writeln("Sample monthly usage at $0.18 per kWh");
  writeln;
  writeln("ZONE                KWH       ESTIMATED COST");
  writeln("------------------------------------------------------------");

  TotalUsage := 0;
  TotalCost := 0;
  PeakZone := 1;
  AboveTarget := 0;
  ProjectedSavings := 0;

  for i := 1 to N do begin
    Cost[i] := KilowattHours[i] * RATE;
    TotalUsage := TotalUsage + KilowattHours[i];
    TotalCost := TotalCost + Cost[i];
    if KilowattHours[i] > KilowattHours[PeakZone] then
      PeakZone := i;
    if KilowattHours[i] > 150 then begin
      AboveTarget := AboveTarget + 1;
      ProjectedSavings := ProjectedSavings + KilowattHours[i] * 0.10 * RATE;
    end;
    writeln("  ", ZoneName[i], "   ", KilowattHours[i], " kWh   $", Cost[i]);
  end;

  AverageUsage := TotalUsage / N;
  writeln;
  writeln("MONTHLY SUMMARY");
  writeln("  Total usage:       ", TotalUsage, " kWh");
  writeln("  Average per zone:  ", AverageUsage, " kWh");
  writeln("  Estimated bill:    $", TotalCost);
  writeln("  Highest use:       ", ZoneName[PeakZone], " (", KilowattHours[PeakZone], " kWh)");
  writeln("  Zones over target: ", AboveTarget);
  writeln;
  writeln("SAVINGS PLAN");
  writeln("  Reduce high-use zones by 10 percent.");
  writeln("  Estimated monthly savings: $", ProjectedSavings);
  if TotalUsage > TARGET then
    writeln("  Status: usage exceeds the neighborhood target of ", TARGET, " kWh.")
  else
    writeln("  Status: usage is within the neighborhood target.");
  writeln;
  writeln("Report complete. Change the sample data to model another month.");
end.

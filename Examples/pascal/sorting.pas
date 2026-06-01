program SortingAlgorithms;

{ Bubble sort, insertion sort, selection sort, and quicksort }

const
    SIZE = 10;

type
    TArray = array[1..SIZE] of Integer;

{ ── Print array ──────────────────────────────────────────────── }
procedure PrintArray(const A: TArray; N: Integer);
var
    I: Integer;
begin
    Write('[ ');
    for I := 1 to N do
    begin
        Write(A[I]);
        if I < N then Write(', ');
    end;
    WriteLn(' ]');
end;

{ ── Bubble Sort ──────────────────────────────────────────────── }
procedure BubbleSort(var A: TArray; N: Integer);
var
    I, J, Tmp: Integer;
    Swapped: Boolean;
begin
    for I := 1 to N-1 do
    begin
        Swapped := False;
        for J := 1 to N-I do
            if A[J] > A[J+1] then
            begin
                Tmp    := A[J];
                A[J]   := A[J+1];
                A[J+1] := Tmp;
                Swapped := True;
            end;
        if not Swapped then Break;  { early exit }
    end;
end;

{ ── Insertion Sort ───────────────────────────────────────────── }
procedure InsertionSort(var A: TArray; N: Integer);
var
    I, J, Key: Integer;
begin
    for I := 2 to N do
    begin
        Key := A[I];
        J   := I - 1;
        while (J >= 1) and (A[J] > Key) do
        begin
            A[J+1] := A[J];
            Dec(J);
        end;
        A[J+1] := Key;
    end;
end;

{ ── Selection Sort ───────────────────────────────────────────── }
procedure SelectionSort(var A: TArray; N: Integer);
var
    I, J, MinIdx, Tmp: Integer;
begin
    for I := 1 to N-1 do
    begin
        MinIdx := I;
        for J := I+1 to N do
            if A[J] < A[MinIdx] then MinIdx := J;
        if MinIdx <> I then
        begin
            Tmp       := A[I];
            A[I]      := A[MinIdx];
            A[MinIdx] := Tmp;
        end;
    end;
end;

{ ── QuickSort ────────────────────────────────────────────────── }
procedure QuickSort(var A: TArray; Low, High: Integer);
var
    I, J, Pivot, Tmp: Integer;
begin
    if Low >= High then Exit;
    Pivot := A[(Low + High) div 2];
    I := Low;
    J := High;
    repeat
        while A[I] < Pivot do Inc(I);
        while A[J] > Pivot do Dec(J);
        if I <= J then
        begin
            Tmp  := A[I];
            A[I] := A[J];
            A[J] := Tmp;
            Inc(I);
            Dec(J);
        end;
    until I > J;
    QuickSort(A, Low, J);
    QuickSort(A, I, High);
end;

{ ── Copy array ───────────────────────────────────────────────── }
procedure CopyArr(const Src: TArray; var Dst: TArray; N: Integer);
var I: Integer;
begin
    for I := 1 to N do Dst[I] := Src[I];
end;

var
    Original, Work: TArray;
    I: Integer;
begin
    { Initialize test data }
    Original[1]  := 64;
    Original[2]  := 25;
    Original[3]  := 12;
    Original[4]  := 90;
    Original[5]  := 3;
    Original[6]  := 77;
    Original[7]  := 44;
    Original[8]  := 18;
    Original[9]  := 55;
    Original[10] := 37;

    WriteLn('=== Sorting Algorithms Demo ===');
    Write('Original: ');
    PrintArray(Original, SIZE);
    WriteLn;

    { Bubble Sort }
    CopyArr(Original, Work, SIZE);
    BubbleSort(Work, SIZE);
    Write('Bubble Sort:    ');
    PrintArray(Work, SIZE);

    { Insertion Sort }
    CopyArr(Original, Work, SIZE);
    InsertionSort(Work, SIZE);
    Write('Insertion Sort: ');
    PrintArray(Work, SIZE);

    { Selection Sort }
    CopyArr(Original, Work, SIZE);
    SelectionSort(Work, SIZE);
    Write('Selection Sort: ');
    PrintArray(Work, SIZE);

    { QuickSort }
    CopyArr(Original, Work, SIZE);
    QuickSort(Work, 1, SIZE);
    Write('QuickSort:      ');
    PrintArray(Work, SIZE);
end.

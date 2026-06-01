program LinkedListDemo;

{ Singly-linked list with insert, delete, search, reverse, and print }

type
    PNode = ^TNode;
    TNode = record
        Data: Integer;
        Next: PNode;
    end;

    TList = record
        Head: PNode;
        Size: Integer;
    end;

{ Initialize empty list }
procedure InitList(var L: TList);
begin
    L.Head := nil;
    L.Size := 0;
end;

{ Insert at front (O(1)) }
procedure InsertFront(var L: TList; Val: Integer);
var
    N: PNode;
begin
    New(N);
    N^.Data := Val;
    N^.Next := L.Head;
    L.Head := N;
    Inc(L.Size);
end;

{ Insert at back (O(n)) }
procedure InsertBack(var L: TList; Val: Integer);
var
    N, Cur: PNode;
begin
    New(N);
    N^.Data := Val;
    N^.Next := nil;
    if L.Head = nil then
        L.Head := N
    else
    begin
        Cur := L.Head;
        while Cur^.Next <> nil do
            Cur := Cur^.Next;
        Cur^.Next := N;
    end;
    Inc(L.Size);
end;

{ Delete first occurrence of Val }
function Delete(var L: TList; Val: Integer): Boolean;
var
    Cur, Prev: PNode;
begin
    Delete := False;
    if L.Head = nil then Exit;

    if L.Head^.Data = Val then
    begin
        Cur := L.Head;
        L.Head := L.Head^.Next;
        Dispose(Cur);
        Dec(L.Size);
        Delete := True;
        Exit;
    end;

    Prev := L.Head;
    Cur  := L.Head^.Next;
    while Cur <> nil do
    begin
        if Cur^.Data = Val then
        begin
            Prev^.Next := Cur^.Next;
            Dispose(Cur);
            Dec(L.Size);
            Delete := True;
            Exit;
        end;
        Prev := Cur;
        Cur  := Cur^.Next;
    end;
end;

{ Search for Val, return position (1-based) or 0 }
function Search(L: TList; Val: Integer): Integer;
var
    Cur: PNode;
    Pos: Integer;
begin
    Cur := L.Head;
    Pos := 1;
    while Cur <> nil do
    begin
        if Cur^.Data = Val then
        begin
            Search := Pos;
            Exit;
        end;
        Cur := Cur^.Next;
        Inc(Pos);
    end;
    Search := 0;
end;

{ Reverse the list in place }
procedure Reverse(var L: TList);
var
    Prev, Cur, Next: PNode;
begin
    Prev := nil;
    Cur  := L.Head;
    while Cur <> nil do
    begin
        Next      := Cur^.Next;
        Cur^.Next := Prev;
        Prev      := Cur;
        Cur       := Next;
    end;
    L.Head := Prev;
end;

{ Print all elements }
procedure PrintList(L: TList);
var
    Cur: PNode;
begin
    Cur := L.Head;
    Write('[ ');
    while Cur <> nil do
    begin
        Write(Cur^.Data);
        if Cur^.Next <> nil then Write(' -> ');
        Cur := Cur^.Next;
    end;
    WriteLn(' ]  (size: ', L.Size, ')');
end;

{ Free all nodes }
procedure FreeList(var L: TList);
var
    Cur, Next: PNode;
begin
    Cur := L.Head;
    while Cur <> nil do
    begin
        Next := Cur^.Next;
        Dispose(Cur);
        Cur := Next;
    end;
    L.Head := nil;
    L.Size := 0;
end;

var
    MyList: TList;
    Pos:    Integer;
    Found:  Boolean;

begin
    WriteLn('=== Linked List Demo ===');
    InitList(MyList);

    { Build list }
    WriteLn('Insert at back: 10, 20, 30, 40, 50');
    InsertBack(MyList, 10);
    InsertBack(MyList, 20);
    InsertBack(MyList, 30);
    InsertBack(MyList, 40);
    InsertBack(MyList, 50);
    PrintList(MyList);

    { Insert at front }
    WriteLn('Insert 5 at front:');
    InsertFront(MyList, 5);
    PrintList(MyList);

    { Search }
    Pos := Search(MyList, 30);
    if Pos > 0 then
        WriteLn('Found 30 at position ', Pos)
    else
        WriteLn('30 not found');

    { Delete }
    Found := Delete(MyList, 20);
    WriteLn('Delete 20: ', Found);
    PrintList(MyList);

    Found := Delete(MyList, 99);
    WriteLn('Delete 99: ', Found);

    { Reverse }
    WriteLn('Reversed:');
    Reverse(MyList);
    PrintList(MyList);

    FreeList(MyList);
    WriteLn('List freed.');
end.

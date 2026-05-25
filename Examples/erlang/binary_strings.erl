-module(binary_strings).
-export([main/0]).

%% Binary/bitstring examples for Time Warp Studio
main() ->
    %% Basic binary construction
    Greeting = <<"Hello, Erlang!">>,
    io:format("Binary: ~w~n", [Greeting]),
    io:format("Size: ~w bytes~n", [byte_size(Greeting)]),

    %% Binary from integers
    Bytes = <<72, 101, 108, 108, 111>>,
    io:format("From integers: ~w~n", [Bytes]),

    %% Convert binary to list of integers
    List = binary_to_list(Greeting),
    io:format("As list length: ~w~n", [length(List)]),

    %% Convert list back to binary
    Bin2 = list_to_binary(List),
    io:format("Roundtrip OK: ~w~n", [byte_size(Bin2) =:= byte_size(Greeting)]),

    %% Binary concatenation via list_to_binary + append
    Part1 = <<"Hello">>,
    Part2 = <<", World!">>,
    Combined = list_to_binary(binary_to_list(Part1) ++ binary_to_list(Part2)),
    io:format("Combined: ~w~n", [Combined]),

    %% binary_part — slice
    Slice = binary_part(Greeting, 0, 5),
    io:format("First 5 bytes: ~w~n", [Slice]),

    %% Packed integers
    Packed = <<1:8, 255:8, 42:8>>,
    io:format("Packed: ~w~n", [Packed]),

    io:format("Done.~n").

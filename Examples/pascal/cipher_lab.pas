program CipherLab;

{ ============================================================
  CIPHER LAB — Classical Cryptography Toolkit in Pascal
  ROT13 * Caesar * Vigenere * Frequency Analysis * Affine
  Time Warp Studio — Pascal Language Showcase
  ============================================================ }

var
  i, j, k : integer;
  msg, result, key : string;
  shift : integer;

{ ===== UTILITY ===== }
function to_upper_char(c : char) : char;
begin
  if (c >= 'a') and (c <= 'z') then
    to_upper_char := chr(ord(c) - 32)
  else
    to_upper_char := c;
end;

function is_letter(c : char) : boolean;
begin
  is_letter := ((c >= 'A') and (c <= 'Z')) or
               ((c >= 'a') and (c <= 'z'));
end;

{ ===== ROT13 ===== }
function rot13(s : string) : string;
var
  res : string;
  idx : integer;
  c : char;
begin
  res := '';
  for idx := 1 to length(s) do begin
    c := s[idx];
    if (c >= 'A') and (c <= 'Z') then
      res := res + chr((ord(c) - ord('A') + 13) mod 26 + ord('A'))
    else if (c >= 'a') and (c <= 'z') then
      res := res + chr((ord(c) - ord('a') + 13) mod 26 + ord('a'))
    else
      res := res + c;
  end;
  rot13 := res;
end;

{ ===== CAESAR CIPHER ===== }
function caesar_encrypt(s : string; sh : integer) : string;
var
  res : string;
  idx : integer;
  c : char;
begin
  res := '';
  sh := ((sh mod 26) + 26) mod 26;
  for idx := 1 to length(s) do begin
    c := s[idx];
    if (c >= 'A') and (c <= 'Z') then
      res := res + chr((ord(c) - ord('A') + sh) mod 26 + ord('A'))
    else if (c >= 'a') and (c <= 'z') then
      res := res + chr((ord(c) - ord('a') + sh) mod 26 + ord('a'))
    else
      res := res + c;
  end;
  caesar_encrypt := res;
end;

function caesar_decrypt(s : string; sh : integer) : string;
begin
  caesar_decrypt := caesar_encrypt(s, 26 - sh);
end;

{ ===== VIGENERE CIPHER ===== }
function vigenere_encrypt(s : string; k : string) : string;
var
  res : string;
  idx, ki, kshift : integer;
  c : char;
begin
  res := '';
  ki := 0;
  for idx := 1 to length(s) do begin
    c := s[idx];
    if is_letter(c) then begin
      kshift := ord(to_upper_char(k[ki mod length(k) + 1])) - ord('A');
      if (c >= 'A') and (c <= 'Z') then
        res := res + chr((ord(c) - ord('A') + kshift) mod 26 + ord('A'))
      else
        res := res + chr((ord(c) - ord('a') + kshift) mod 26 + ord('a'));
      ki := ki + 1;
    end else
      res := res + c;
  end;
  vigenere_encrypt := res;
end;

function vigenere_decrypt(s : string; k : string) : string;
var
  res : string;
  idx, ki, kshift : integer;
  c : char;
begin
  res := '';
  ki := 0;
  for idx := 1 to length(s) do begin
    c := s[idx];
    if is_letter(c) then begin
      kshift := ord(to_upper_char(k[ki mod length(k) + 1])) - ord('A');
      if (c >= 'A') and (c <= 'Z') then
        res := res + chr((ord(c) - ord('A') - kshift + 26) mod 26 + ord('A'))
      else
        res := res + chr((ord(c) - ord('a') - kshift + 26) mod 26 + ord('a'));
      ki := ki + 1;
    end else
      res := res + c;
  end;
  vigenere_decrypt := res;
end;

{ ===== AFFINE CIPHER (Ax + B mod 26) ===== }
function affine_encrypt(s : string; a_key, b_key : integer) : string;
var
  res : string;
  idx : integer;
  c : char;
begin
  res := '';
  for idx := 1 to length(s) do begin
    c := s[idx];
    if (c >= 'A') and (c <= 'Z') then
      res := res + chr((a_key * (ord(c) - ord('A')) + b_key) mod 26 + ord('A'))
    else if (c >= 'a') and (c <= 'z') then
      res := res + chr((a_key * (ord(c) - ord('a')) + b_key) mod 26 + ord('a'))
    else
      res := res + c;
  end;
  affine_encrypt := res;
end;

{ ===== FREQUENCY ANALYSIS ===== }
procedure freq_analysis(s : string);
var
  freq : array[0..25] of integer;
  total_letters : integer;
  idx, max_idx, count : integer;
  c : char;
  top_letter : char;
begin
  for idx := 0 to 25 do freq[idx] := 0;
  total_letters := 0;
  for idx := 1 to length(s) do begin
    c := to_upper_char(s[idx]);
    if (c >= 'A') and (c <= 'Z') then begin
      freq[ord(c) - ord('A')] := freq[ord(c) - ord('A')] + 1;
      total_letters := total_letters + 1;
    end;
  end;
  writeln('  Total letters: ', total_letters);
  writeln('  Frequency table (letters present):');
  write('  ');
  for idx := 0 to 25 do begin
    if freq[idx] > 0 then begin
      write(chr(idx + ord('A')), ':', freq[idx], '  ');
    end;
  end;
  writeln;
  { Find most common — likely maps to 'E' in English }
  max_idx := 0;
  for idx := 1 to 25 do
    if freq[idx] > freq[max_idx] then max_idx := idx;
  top_letter := chr(max_idx + ord('A'));
  writeln('  Most frequent: ''', top_letter, ''' (', freq[max_idx], ' times)');
  writeln('  Caesar shift guess (if most freq maps to E): ',
          (max_idx - 4 + 26) mod 26);
end;

{ ===== CAESAR BRUTE FORCE ===== }
procedure brute_force_caesar(ciphertext : string);
var
  sh : integer;
begin
  writeln('  Brute force all 26 Caesar shifts:');
  for sh := 0 to 25 do
    writeln('  Shift ', sh, ': ', caesar_decrypt(ciphertext, sh));
end;

{ ===== COLUMNAR TRANSPOSITION ===== }
function columnar_encrypt(s : string; cols : integer) : string;
var
  res : string;
  col, row, rows, pos : integer;
  padded : string;
begin
  { Pad to multiple of cols }
  padded := s;
  while length(padded) mod cols <> 0 do
    padded := padded + 'X';
  rows := length(padded) div cols;
  res := '';
  { Read column by column }
  for col := 1 to cols do
    for row := 0 to rows - 1 do begin
      pos := row * cols + col;
      if pos <= length(padded) then
        res := res + padded[pos];
    end;
  columnar_encrypt := res;
end;

{ ===== MAIN PROGRAM ===== }
begin
  writeln('============================================================');
  writeln('  CIPHER LAB — Classical Cryptography in Pascal');
  writeln('  Explore ciphers used from ancient Rome to World War II');
  writeln('============================================================');
  writeln;

  msg := 'The quick brown fox jumps over the lazy dog';

  { ---- Section 1: ROT13 ---- }
  writeln('[ 1 ] ROT13 CIPHER (rotate each letter by 13)');
  writeln('  Plaintext:   ', msg);
  result := rot13(msg);
  writeln('  ROT13:       ', result);
  writeln('  ROT13 again: ', rot13(result), '  <- back to original!');
  writeln('  ROT13 is its own inverse — used in Usenet spoilers');
  writeln;

  { ---- Section 2: Caesar ---- }
  writeln('[ 2 ] CAESAR CIPHER (Julius Caesar used shift=3)');
  writeln('  Original: ', msg);
  for shift := 1 to 5 do begin
    result := caesar_encrypt(msg, shift);
    writeln('  Shift ', shift, ':  ', result);
  end;
  writeln;
  result := caesar_encrypt(msg, 13);
  writeln('  Shift 13 (ROT13): ', result);
  writeln('  Decrypted back:   ', caesar_decrypt(result, 13));
  writeln;

  { ---- Section 3: Vigenere ---- }
  writeln('[ 3 ] VIGENERE CIPHER (polyalphabetic — key: PASCAL)');
  key := 'PASCAL';
  msg := 'Programming is an art and a science';
  writeln('  Plaintext:  ', msg);
  writeln('  Key:        ', key);
  result := vigenere_encrypt(msg, key);
  writeln('  Encrypted:  ', result);
  writeln('  Decrypted:  ', vigenere_decrypt(result, key));
  writeln;

  { ---- Section 4: Affine ---- }
  writeln('[ 4 ] AFFINE CIPHER (f(x) = ax + b mod 26, a=7, b=10)');
  msg := 'HELLO WORLD FROM PASCAL';
  writeln('  Plaintext:  ', msg);
  result := affine_encrypt(msg, 7, 10);
  writeln('  Encrypted:  ', result);
  writeln('  (Decryption requires modular inverse of a=7 mod 26=15)');
  writeln;

  { ---- Section 5: Frequency Analysis ---- }
  writeln('[ 5 ] FREQUENCY ANALYSIS');
  writeln('  English text uses letters in predictable frequency:');
  writeln('  E T A O I N S H R D L C U M W F G Y P B...');
  writeln;
  msg := 'Wxe quick jzown fox jumps over the lazy dog andthe red fox ran away';
  writeln('  Analysing: "', msg, '"');
  freq_analysis(msg);
  writeln;

  { ---- Section 6: Columnar Transposition ---- }
  writeln('[ 6 ] COLUMNAR TRANSPOSITION (key=5 columns)');
  msg := 'ATTACK AT DAWN DO NOT RETREAT UNDER ANY CONDITION';
  writeln('  Original:   ', msg);
  result := columnar_encrypt(msg, 5);
  writeln('  Encrypted:  ', result);
  writeln('  (Columns reordered — transpose the grid to decrypt)');
  writeln;

  { ---- Section 7: Historical facts ---- }
  writeln('[ 7 ] CRYPTOGRAPHY TIMELINE');
  writeln('  ~50 BC  : Caesar cipher (shift=3) used by Julius Caesar');
  writeln('  ~1400s  : Vigenere cipher invented by Giovan Bellaso');
  writeln('  1917    : One-time pad invented — theoretically unbreakable');
  writeln('  1918    : ADFGVX cipher used in WWI by Germany');
  writeln('  1923    : Enigma machine patented by Arthur Scherbius');
  writeln('  1977    : RSA public-key cryptography published');
  writeln('  2001    : AES (Rijndael) adopted as US Federal standard');
  writeln('  Today   : 256-bit AES — would take universe''s lifetime to crack');
  writeln;

  writeln('============================================================');
  writeln('  Pascal Cipher Lab Complete!');
  writeln('  From Caesar to Vigenere — the mathematics of secrecy!');
  writeln('============================================================');
end.

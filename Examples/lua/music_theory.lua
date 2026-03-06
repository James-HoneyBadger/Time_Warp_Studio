-- ================================================================
--  MUSIC THEORY TOOLKIT  (music_theory.lua)
--  A complete music theory library: notes, intervals, scales,
--  chords, progressions, circle of fifths, and transposition.
-- ================================================================

-- ──────────────────────────────────────────────────────────────
--  NOTE SYSTEM  (chromatic, 0-based: C=0, C#=1, D=2, ...)
-- ──────────────────────────────────────────────────────────────

local NOTE_NAMES_SHARP = {"C","C#","D","D#","E","F","F#","G","G#","A","A#","B"}
local NOTE_NAMES_FLAT  = {"C","Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B"}
local NOTE_MAP  = {}  -- lowercase lookup
for i, n in ipairs(NOTE_NAMES_SHARP) do NOTE_MAP[n:lower()] = i - 1 end
for i, n in ipairs(NOTE_NAMES_FLAT)  do NOTE_MAP[n:lower()] = i - 1 end

local function noteIndex(name)
  local idx = NOTE_MAP[name:lower()]
  assert(idx, "Unknown note: " .. tostring(name))
  return idx
end

local function noteName(index, useFlat)
  index = index % 12
  return useFlat and NOTE_NAMES_FLAT[index + 1] or NOTE_NAMES_SHARP[index + 1]
end

-- ──────────────────────────────────────────────────────────────
--  INTERVALS
-- ──────────────────────────────────────────────────────────────

local INTERVALS = {
  ["P1"]  = 0,   ["m2"]  = 1,   ["M2"]  = 2,
  ["m3"]  = 3,   ["M3"]  = 4,   ["P4"]  = 5,
  ["TT"]  = 6,   ["P5"]  = 7,   ["m6"]  = 8,
  ["M6"]  = 9,   ["m7"]  = 10,  ["M7"]  = 11,
  ["P8"]  = 12,  ["m9"]  = 13,  ["M9"]  = 14,
  ["m10"] = 15,  ["M10"] = 16,  ["P11"] = 17,
  ["A11"] = 18,  ["P12"] = 19,  ["m13"] = 20,
  ["M13"] = 21,
}

local INTERVAL_NAMES = {}
for name, semis in pairs(INTERVALS) do INTERVAL_NAMES[semis % 12] = name end

local function intervalName(semitones)
  return INTERVAL_NAMES[semitones % 12] or (semitones .. " semitones")
end

local function transpose(noteName_, semitones, useFlat)
  local idx = noteIndex(noteName_)
  return noteName((idx + semitones) % 12, useFlat)
end

-- ──────────────────────────────────────────────────────────────
--  SCALES
-- ──────────────────────────────────────────────────────────────

local SCALE_FORMULAS = {
  major         = {0, 2, 4, 5, 7, 9, 11},
  natural_minor = {0, 2, 3, 5, 7, 8, 10},
  harmonic_minor= {0, 2, 3, 5, 7, 8, 11},
  melodic_minor = {0, 2, 3, 5, 7, 9, 11},
  pentatonic_major = {0, 2, 4, 7, 9},
  pentatonic_minor = {0, 3, 5, 7, 10},
  blues         = {0, 3, 5, 6, 7, 10},
  dorian        = {0, 2, 3, 5, 7, 9, 10},
  phrygian      = {0, 1, 3, 5, 7, 8, 10},
  lydian        = {0, 2, 4, 6, 7, 9, 11},
  mixolydian    = {0, 2, 4, 5, 7, 9, 10},
  locrian       = {0, 1, 3, 5, 6, 8, 10},
  whole_tone    = {0, 2, 4, 6, 8, 10},
  diminished    = {0, 2, 3, 5, 6, 8, 9, 11},
  chromatic     = {0,1,2,3,4,5,6,7,8,9,10,11},
}

local function buildScale(root, scaleName, useFlat)
  local formula = SCALE_FORMULAS[scaleName]
  assert(formula, "Unknown scale: " .. tostring(scaleName))
  local rootIdx = noteIndex(root)
  local notes = {}
  for _, interval in ipairs(formula) do
    table.insert(notes, noteName((rootIdx + interval) % 12, useFlat))
  end
  return notes
end

local function printScale(root, scaleName, useFlat)
  local notes = buildScale(root, scaleName, useFlat)
  print(string.format("  %-8s %-20s → %s", root:upper(), scaleName, table.concat(notes, "  ")))
end

-- ──────────────────────────────────────────────────────────────
--  CHORDS
-- ──────────────────────────────────────────────────────────────

local CHORD_TYPES = {
  [""]      = {0, 4, 7},            -- major triad
  ["m"]     = {0, 3, 7},            -- minor triad
  ["dim"]   = {0, 3, 6},            -- diminished triad
  ["aug"]   = {0, 4, 8},            -- augmented triad
  ["sus2"]  = {0, 2, 7},
  ["sus4"]  = {0, 5, 7},
  ["5"]     = {0, 7},               -- power chord
  ["7"]     = {0, 4, 7, 10},        -- dominant 7th
  ["maj7"]  = {0, 4, 7, 11},        -- major 7th
  ["m7"]    = {0, 3, 7, 10},        -- minor 7th
  ["m7b5"]  = {0, 3, 6, 10},        -- half-diminished
  ["dim7"]  = {0, 3, 6, 9},         -- fully diminished 7th
  ["mM7"]   = {0, 3, 7, 11},        -- minor major 7th
  ["9"]     = {0, 4, 7, 10, 14},    -- dominant 9th
  ["maj9"]  = {0, 4, 7, 11, 14},
  ["m9"]    = {0, 3, 7, 10, 14},
  ["11"]    = {0, 4, 7, 10, 14, 17},
  ["13"]    = {0, 4, 7, 10, 14, 17, 21},
  ["add9"]  = {0, 4, 7, 14},
  ["6"]     = {0, 4, 7, 9},
  ["m6"]    = {0, 3, 7, 9},
  ["6/9"]   = {0, 4, 7, 9, 14},
}

local function buildChord(root, chordType, useFlat)
  chordType = chordType or ""
  local intervals = CHORD_TYPES[chordType]
  assert(intervals, "Unknown chord type: '" .. tostring(chordType) .. "'")
  local rootIdx = noteIndex(root)
  local notes = {}
  for _, iv in ipairs(intervals) do
    table.insert(notes, noteName((rootIdx + iv) % 12, useFlat))
  end
  return notes
end

local function printChord(root, chordType, useFlat)
  local notes = buildChord(root, chordType, useFlat)
  local name = root:upper() .. (chordType or "")
  print(string.format("  %-10s → %s", name, table.concat(notes, "  ")))
end

-- ──────────────────────────────────────────────────────────────
--  DIATONIC CHORDS (chords built from a scale)
-- ──────────────────────────────────────────────────────────────

-- Roman numeral labels
local ROMAN = {"I","II","III","IV","V","VI","VII"}

local function diatonicChords(root, scaleName, useFlat)
  local scale = buildScale(root, scaleName, useFlat)
  local scaleIdx = {}
  for _, n in ipairs(scale) do scaleIdx[noteIndex(n)] = true end

  local result = {}
  for i, note in ipairs(scale) do
    -- Build triad from this scale degree
    local rootI = noteIndex(note)
    local third  = scale[((i-1+2) % #scale) + 1]
    local fifth  = scale[((i-1+4) % #scale) + 1]
    local r3     = (noteIndex(third) - rootI + 12) % 12
    local r5     = (noteIndex(fifth) - rootI + 12) % 12

    local quality = ""
    if r3 == 4 and r5 == 7 then quality = ""     -- major
    elseif r3 == 3 and r5 == 7 then quality = "m" -- minor
    elseif r3 == 3 and r5 == 6 then quality = "dim"
    elseif r3 == 4 and r5 == 8 then quality = "aug"
    else quality = "?"
    end

    local cname = note:upper() .. quality
    local cnotes = {note, third, fifth}
    table.insert(result, {
      degree  = ROMAN[i],
      root    = note,
      quality = quality,
      name    = cname,
      notes   = cnotes,
    })
  end
  return result
end

-- ──────────────────────────────────────────────────────────────
--  CHORD PROGRESSIONS
-- ──────────────────────────────────────────────────────────────

local PROGRESSIONS = {
  ["I-IV-V"]        = {1, 4, 5},
  ["I-V-vi-IV"]     = {1, 5, 6, 4},   -- "axis" progression (Let It Be, etc.)
  ["ii-V-I"]        = {2, 5, 1},       -- jazz standard
  ["I-vi-IV-V"]     = {1, 6, 4, 5},   -- "50s progression"
  ["I-IV-vi-V"]     = {1, 4, 6, 5},
  ["vi-IV-I-V"]     = {6, 4, 1, 5},
  ["I-V-IV-I"]      = {1, 5, 4, 1},   -- blues feel
  ["iii-VI-ii-V-I"] = {3, 6, 2, 5, 1}, -- jazz turnaround
}

local function playProgression(root, scaleName, progName, useFlat)
  local chords = diatonicChords(root, scaleName, useFlat)
  local degrees = PROGRESSIONS[progName]
  assert(degrees, "Unknown progression: " .. tostring(progName))

  print(string.format("  %s %s — %s", root:upper(), scaleName, progName))
  for _, deg in ipairs(degrees) do
    if deg <= #chords then
      local ch = chords[deg]
      print(string.format("    %s  [%s] → %s", ch.degree, ch.name, table.concat(ch.notes, " ")))
    end
  end
  print()
end

-- ──────────────────────────────────────────────────────────────
--  CIRCLE OF FIFTHS
-- ──────────────────────────────────────────────────────────────

local CIRCLE_MAJOR = {"C","G","D","A","E","B","F#","Db","Ab","Eb","Bb","F"}
local CIRCLE_MINOR = {"Am","Em","Bm","F#m","C#m","G#m","D#m","Bbm","Fm","Cm","Gm","Dm"}
local KEY_SIGNATURES = {
  ["C"]  = 0,  ["G"]= 1, ["D"]= 2,  ["A"]= 3,  ["E"]= 4,  ["B"]= 5,
  ["F#"]= 6,  ["Db"]=-5,["Ab"]=-4, ["Eb"]=-3, ["Bb"]=-2, ["F"]=-1,
}

local function printCircleOfFifths()
  print("  ╔══════════════════════════════════════════════╗")
  print("  ║          CIRCLE OF FIFTHS                    ║")
  print("  ╠══════════════════════╦═══════════════════════╣")
  print("  ║ Major Key  Sharps/Flats  Relative Minor       ║")
  print("  ╠══════════════════════╬═══════════════════════╣")
  for i, key in ipairs(CIRCLE_MAJOR) do
    local sigs = KEY_SIGNATURES[key] or 0
    local sigStr
    if sigs == 0 then sigStr = "  no sharps/flats"
    elseif sigs > 0 then sigStr = string.format("  %d sharp%s", sigs, sigs>1 and "s" or "")
    else sigStr = string.format("  %d flat%s", -sigs, -sigs>1 and "s" or "")
    end
    print(string.format("  ║  %-4s  %-20s  %-8s  ║",
          key, sigStr, CIRCLE_MINOR[i]))
  end
  print("  ╚══════════════════════════════════════════════╝")
end

-- ──────────────────────────────────────────────────────────────
--  INTERVAL CALCULATOR
-- ──────────────────────────────────────────────────────────────

local function calcInterval(note1, note2)
  local i1 = noteIndex(note1)
  local i2 = noteIndex(note2)
  local semis = (i2 - i1 + 12) % 12
  return semis, intervalName(semis)
end

-- ──────────────────────────────────────────────────────────────
--  DEMO
-- ──────────────────────────────────────────────────────────────

print("╔══════════════════════════════════════════════════════════════╗")
print("║              MUSIC THEORY TOOLKIT — Lua Demo                 ║")
print("╚══════════════════════════════════════════════════════════════╝\n")

-- Scales demo
print("── SCALES ──────────────────────────────────────────────────────")
local demo_scales = {"major","natural_minor","pentatonic_major",
                     "blues","dorian","lydian","whole_tone"}
for _, sname in ipairs(demo_scales) do
  printScale("C", sname, false)
end
for _, sname in ipairs({"major","natural_minor"}) do
  printScale("Bb", sname, true)
end

-- Chords demo
print("\n── CHORDS IN C ─────────────────────────────────────────────────")
local chord_types = {"","m","dim","aug","7","maj7","m7","dim7","9","maj9","sus4","add9"}
for _, ct in ipairs(chord_types) do
  printChord("C", ct, false)
end

-- Diatonic chords
print("\n── DIATONIC CHORDS: C MAJOR ────────────────────────────────────")
local dchords = diatonicChords("C", "major", false)
for _, ch in ipairs(dchords) do
  local nstr = table.concat(ch.notes, "  ")
  print(string.format("  %s  %-10s → %s", ch.degree, ch.name, nstr))
end

print("\n── DIATONIC CHORDS: A MINOR ────────────────────────────────────")
local aminor_chords = diatonicChords("A", "natural_minor", false)
for _, ch in ipairs(aminor_chords) do
  local nstr = table.concat(ch.notes, "  ")
  print(string.format("  %s  %-10s → %s", ch.degree, ch.name, nstr))
end

-- Progressions
print("\n── COMMON PROGRESSIONS ─────────────────────────────────────────")
playProgression("G", "major", "I-V-vi-IV", false)
playProgression("D", "major", "ii-V-I",    false)
playProgression("C", "major", "I-vi-IV-V", false)

-- Interval calculator
print("── INTERVAL CALCULATOR ─────────────────────────────────────────")
local pairs_to_check = {
  {"C","E"}, {"C","G"}, {"A","C"}, {"F","B"},
  {"G","F"}, {"D","A"}, {"C","Bb"}
}
for _, p in ipairs(pairs_to_check) do
  local semis, name = calcInterval(p[1], p[2])
  print(string.format("  %s → %s  =  %2d semitones  (%s)", p[1], p[2], semis, name))
end

-- Transposition
print("\n── TRANSPOSITION ───────────────────────────────────────────────")
local melody = {"C","D","E","G","A","G","E","D","C"}
print("  Original  (C): " .. table.concat(melody, " "))
local transposed_G = {}
for _, n in ipairs(melody) do
  table.insert(transposed_G, transpose(n, 7, false))  -- up a P5
end
print("  Up a 5th (to G): " .. table.concat(transposed_G, " "))
local transposed_F = {}
for _, n in ipairs(melody) do
  table.insert(transposed_F, transpose(n, -7, true))  -- down a P5
end
print("  Down a 5th (to F): " .. table.concat(transposed_F, " "))

-- Circle of Fifths
print("\n── CIRCLE OF FIFTHS ────────────────────────────────────────────")
printCircleOfFifths()

print("\nMusic theory toolkit complete!")

--[[
  TURTLE PATTERNS — Lua Language Showcase
  Mathematical curves, spirographs, rose patterns with turtle graphics
  Time Warp Studio — Lua Language Demo
--]]

local turtle = require("turtle")

-- ============================================================
-- MATH HELPERS
-- ============================================================

local PI = math.pi
local sin, cos, sqrt, floor = math.sin, math.cos, math.sqrt, math.floor

--- Convert degrees to radians
local function rad(d) return d * PI / 180 end

--- Map a value from one range to another
local function map(v, from_lo, from_hi, to_lo, to_hi)
  return to_lo + (v - from_lo) * (to_hi - to_lo) / (from_hi - from_lo)
end

-- ============================================================
-- PATTERN 1: SPIROGRAPH (Hypotrochoid)
-- r = inner circle radius, R = outer circle radius, d = pen distance
-- x(t) = (R-r)*cos(t) + d*cos((R-r)/r * t)
-- y(t) = (R-r)*sin(t) - d*sin((R-r)/r * t)
-- ============================================================

local function spirograph(R, r, d, steps, scale, color_r, color_g, color_b)
  turtle.setpencolor(color_r, color_g, color_b)
  local x0 = (R - r) + d
  local y0 = 0
  turtle.penup()
  turtle.setxy(x0 * scale, y0 * scale)
  turtle.pendown()
  for i = 1, steps do
    local t = i * 2 * PI / steps
    local x = (R - r) * cos(t) + d * cos((R - r) / r * t)
    local y = (R - r) * sin(t) - d * sin((R - r) / r * t)
    turtle.setxy(x * scale, y * scale)
  end
  turtle.penup()
end

-- ============================================================
-- PATTERN 2: Rose Curve r = cos(n * theta)
-- n odd: n petals, n even: 2n petals
-- ============================================================

local function rose_curve(n, size, steps, color_r, color_g, color_b)
  turtle.setpencolor(color_r, color_g, color_b)
  local t = 0
  local dt = 2 * PI / steps
  turtle.penup()
  local r0 = size * cos(n * 0)
  turtle.setxy(r0, 0)
  turtle.pendown()
  for i = 1, steps do
    t = i * dt
    local r = size * cos(n * t)
    local x = r * cos(t)
    local y = r * sin(t)
    turtle.setxy(x, y)
  end
  turtle.penup()
end

-- ============================================================
-- PATTERN 3: Lissajous figure
-- x(t) = A*sin(a*t + delta), y(t) = B*sin(b*t)
-- ============================================================

local function lissajous(A, B, a, b, delta_deg, steps, color_r, color_g, color_b)
  turtle.setpencolor(color_r, color_g, color_b)
  local delta = rad(delta_deg)
  turtle.penup()
  local x0 = A * sin(delta)
  local y0 = 0
  turtle.setxy(x0, y0)
  turtle.pendown()
  for i = 1, steps do
    local t = i * 2 * PI / steps
    local x = A * sin(a * t + delta)
    local y = B * sin(b * t)
    turtle.setxy(x, y)
  end
  turtle.penup()
end

-- ============================================================
-- PATTERN 4: Archimedean Spiral
-- r = a + b*theta
-- ============================================================

local function spiral(a, b, turns, steps, color_r, color_g, color_b)
  turtle.setpencolor(color_r, color_g, color_b)
  turtle.penup()
  turtle.setxy(a, 0)
  turtle.pendown()
  local total_angle = turns * 2 * PI
  for i = 1, steps do
    local theta = i * total_angle / steps
    local r = a + b * theta
    local x = r * cos(theta)
    local y = r * sin(theta)
    turtle.setxy(x, y)
  end
  turtle.penup()
end

-- ============================================================
-- PATTERN 5: Epitrochoid
-- x(t) = (R+r)*cos(t) - d*cos((R+r)/r * t)
-- y(t) = (R+r)*sin(t) - d*sin((R+r)/r * t)
-- ============================================================

local function epitrochoid(R, r, d, steps, scale, color_r, color_g, color_b)
  turtle.setpencolor(color_r, color_g, color_b)
  turtle.penup()
  local x0 = (R + r) - d
  turtle.setxy(x0 * scale, 0)
  turtle.pendown()
  for i = 1, steps do
    local t = i * 2 * PI / steps
    local x = (R + r) * cos(t) - d * cos((R + r) / r * t)
    local y = (R + r) * sin(t) - d * sin((R + r) / r * t)
    turtle.setxy(x * scale, y * scale)
  end
  turtle.penup()
end

-- ============================================================
-- PATTERN 6: Star polygon {n/k}
-- ============================================================

local function star_polygon(n, k, size, color_r, color_g, color_b)
  turtle.setpencolor(color_r, color_g, color_b)
  turtle.penup()
  turtle.setxy(0, size)
  turtle.pendown()
  for i = 1, n do
    local angle = rad(90 + i * 360 * k / n)
    local x = size * cos(angle)
    local y = size * sin(angle)
    turtle.setxy(x, y)
  end
  turtle.penup()
end

-- ============================================================
-- MAIN PROGRAM
-- ============================================================

print("============================================================")
print("  TURTLE PATTERNS — Lua Showcase")
print("  Spirographs, Rose Curves, Lissajous, Spirals")
print("============================================================")
print()

-- Pattern 1: Spirographs
print("Drawing Pattern 1: Spirograph collection...")
print("  Hypotrochoid with R=5, r=3, d=5 (5-petal spirograph)")
spirograph(5, 3, 5, 300, 20, 255, 80, 80)

print("  Hypotrochoid with R=5, r=1, d=3 (star spirograph)")
spirograph(5, 1, 3, 300, 20, 80, 200, 255)

print("  Epitrochoid with R=3, r=1, d=5 (outer spirograph)")
epitrochoid(3, 1, 5, 300, 15, 180, 80, 255)

-- Pattern 2: Rose Curves
print()
print("Drawing Pattern 2: Rose Curves r = cos(n*theta)...")
turtle.setxy(0, 0)

print("  3-petal rose (n=3)")
rose_curve(3, 120, 300, 255, 100, 150)

print("  5-petal rose (n=5)")
rose_curve(5, 100, 500, 100, 255, 100)

print("  8-petal rose (n=4, 2n=8 petals)")
rose_curve(4, 90, 400, 255, 200, 50)

-- Pattern 3: Lissajous figures
print()
print("Drawing Pattern 3: Lissajous Figures...")
print("  Lissajous 3:2 (delta=90°) — figure-8 variant")
lissajous(150, 120, 3, 2, 90, 300, 50, 220, 255)

print("  Lissajous 5:4 (delta=45°)")
lissajous(130, 100, 5, 4, 45, 500, 255, 180, 80)

print("  Lissajous 7:6 (delta=30°)")
lissajous(110, 90, 7, 6, 30, 700, 200, 80, 255)

-- Pattern 4: Archimedean spirals
print()
print("Drawing Pattern 4: Archimedean Spirals...")
print("  Expanding spiral (a=0, b=2, 8 turns)")
spiral(0, 2, 8, 400, 200, 100, 255)

print("  Tight spiral (a=5, b=3, 12 turns)")
spiral(5, 3, 12, 600, 100, 200, 200)

-- Pattern 5: Star polygons
print()
print("Drawing Pattern 5: Star Polygons...")
print("  Star {5/2} — pentagram")
star_polygon(5, 2, 100, 255, 220, 50)

print("  Star {7/2} — heptagram")
star_polygon(7, 2, 120, 80, 255, 180)

print("  Star {7/3} — acute heptagram")
star_polygon(7, 3, 110, 255, 80, 200)

print("  Star {9/4} — star nonagon")
star_polygon(9, 4, 130, 180, 255, 80)

-- Pattern 6: Rainbow spiral
print()
print("Drawing Pattern 6: Rainbow Color Spiral...")
local colors = {
  {255, 0, 0}, {255, 127, 0}, {255, 255, 0},
  {0, 255, 0}, {0, 127, 255}, {0, 0, 255},
  {127, 0, 255}, {255, 0, 127},
}
for ci, col in ipairs(colors) do
  local offset = (ci - 1) * PI / 4
  turtle.setpencolor(col[1], col[2], col[3])
  turtle.penup()
  turtle.setxy(0, 0)
  turtle.pendown()
  for i = 0, 200 do
    local theta = i * 4 * PI / 200
    local r = 1.5 * theta
    local x = r * cos(theta + offset)
    local y = r * sin(theta + offset)
    turtle.setxy(x * 10, y * 10)
  end
  turtle.penup()
end

print()
print("============================================================")
print("  All 6 pattern sets complete!")
print("  Spirograph * Rose * Lissajous * Spiral * Stars * Rainbow")
print("============================================================")

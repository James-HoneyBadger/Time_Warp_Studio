-- ============================================================
-- DUNGEON ADVENTURE ENGINE — Lua Language Showcase
-- Complete text adventure with OOP, inventory, combat, maps
-- Time Warp Studio — Lua Language Demo
-- ============================================================

-- ===== UTILITY =====

local function printf(fmt, ...)
    io.write(string.format(fmt, ...))
end

local function center(text, width)
    local pad = math.floor((width - #text) / 2)
    return string.rep(" ", math.max(0, pad)) .. text
end

local function divider(char, width)
    return string.rep(char or "-", width or 60)
end

-- ===== ITEM CLASS =====

local Item = {}
Item.__index = Item

function Item.new(name, desc, damage, heal, value)
    return setmetatable({
        name   = name,
        desc   = desc,
        damage = damage or 0,
        heal   = heal   or 0,
        value  = value  or 0,
    }, Item)
end

function Item:__tostring()
    local parts = { self.name }
    if self.damage > 0 then parts[#parts+1] = string.format("ATK+%d", self.damage) end
    if self.heal   > 0 then parts[#parts+1] = string.format("HP+%d",  self.heal)   end
    if self.value  > 0 then parts[#parts+1] = string.format("%dgp",   self.value)  end
    return table.concat(parts, " | ")
end

-- ===== CREATURE CLASS =====

local Creature = {}
Creature.__index = Creature

function Creature.new(name, hp, attack, defense, xp, gold)
    return setmetatable({
        name    = name,
        hp      = hp,
        max_hp  = hp,
        attack  = attack,
        defense = defense,
        xp      = xp  or 0,
        gold    = gold or 0,
        alive   = true,
    }, Creature)
end

function Creature:take_damage(dmg)
    local actual = math.max(1, dmg - self.defense)
    self.hp = self.hp - actual
    if self.hp <= 0 then
        self.hp = 0
        self.alive = false
    end
    return actual
end

function Creature:is_alive() return self.alive end

function Creature:hp_bar()
    local filled = math.floor(self.hp / self.max_hp * 20)
    return "[" .. string.rep("█", filled) ..
           string.rep("░", 20 - filled) .. "]"
end

-- ===== PLAYER CLASS =====

local Player = setmetatable({}, { __index = Creature })
Player.__index = Player

function Player.new(name)
    local self = Creature.new(name, 100, 12, 3, 0, 0)
    self.level    = 1
    self.xp       = 0
    self.xp_next  = 50
    self.gold     = 20
    self.inventory = {}
    self.equipped  = nil
    self.kills     = 0
    self.steps     = 0
    return setmetatable(self, Player)
end

function Player:pick_up(item)
    table.insert(self.inventory, item)
    printf("    Picked up: %s\n", tostring(item))
end

function Player:equip_best_weapon()
    local best, best_dmg = nil, 0
    for _, item in ipairs(self.inventory) do
        if item.damage > best_dmg then
            best_dmg = item.damage
            best = item
        end
    end
    if best then
        self.equipped = best
        self.attack = 12 + best.damage
    end
end

function Player:use_best_potion()
    for i, item in ipairs(self.inventory) do
        if item.heal > 0 then
            self.hp = math.min(self.max_hp, self.hp + item.heal)
            table.remove(self.inventory, i)
            printf("    Used %s — restored %d HP\n", item.name, item.heal)
            return true
        end
    end
    return false
end

function Player:gain_xp(amount)
    self.xp = self.xp + amount
    while self.xp >= self.xp_next do
        self.level = self.level + 1
        self.max_hp = self.max_hp + 20
        self.hp = self.max_hp
        self.attack = self.attack + 3
        self.defense = self.defense + 1
        self.xp_next = math.floor(self.xp_next * 1.6)
        printf("  *** LEVEL UP! Now level %d ***\n", self.level)
        printf("      HP max: %d  ATK: %d  DEF: %d\n",
               self.max_hp, self.attack, self.defense)
    end
end

function Player:status()
    printf("\n  ── %s (Lv.%d) ──\n", self.name, self.level)
    printf("  HP: %3d/%3d  %s\n", self.hp, self.max_hp, self:hp_bar())
    printf("  ATK: %d  DEF: %d  XP: %d/%d  Gold: %d\n",
           self.attack, self.defense, self.xp, self.xp_next, self.gold)
    if self.equipped then
        printf("  Weapon: %s\n", self.equipped.name)
    end
    printf("  Inventory: %d items | Kills: %d | Steps: %d\n",
           #self.inventory, self.kills, self.steps)
end

-- ===== COMBAT SYSTEM =====

local function combat(player, enemy)
    printf("\n  ⚔  COMBAT: %s vs %s\n", player.name, enemy.name)
    printf("  %s  HP:%d  ATK:%d  DEF:%d\n\n",
           enemy.name, enemy.hp, enemy.attack, enemy.defense)

    local round = 1
    while player:is_alive() and enemy:is_alive() do
        -- Player attacks
        local p_dmg = player:take_damage(0)  -- dummy call
        local e_dmg = enemy:take_damage(player.attack + math.random(-3, 5))
        printf("  Rnd %d: You deal %d dmg → %s HP:%d %s\n",
               round, e_dmg, enemy.name, enemy.hp, enemy:hp_bar())

        if not enemy:is_alive() then break end

        -- Enemy attacks
        local p_hit = player:take_damage(enemy.attack + math.random(-2, 4))
        printf("         %s deals %d dmg → You HP:%d %s\n",
               enemy.name, p_hit, player.hp, player:hp_bar())

        -- Auto-potion at low health
        if player.hp < 25 then
            if not player:use_best_potion() then
                printf("         [No potions available!]\n")
            end
        end

        round = round + 1
        if round > 30 then
            printf("  [Battle stalemate — both sides retreat]\n")
            break
        end
    end

    if not enemy:is_alive() then
        player.kills = player.kills + 1
        player.gold = player.gold + enemy.gold
        player:gain_xp(enemy.xp)
        printf("  Victory! Gained %d XP, %d gold\n", enemy.xp, enemy.gold)
        return true
    else
        printf("  Defeated! The adventure ends here...\n")
        return false
    end
end

-- ===== DUNGEON ROOMS =====

local ROOMS = {
    {
        name = "Entrance Hall",
        desc = "Crumbling stone walls. Torches flicker. A dark corridor leads deeper.",
        enemy = nil,
        item  = Item.new("Rusty Sword", "Better than bare hands", 5, 0, 10),
        gold  = 5,
    },
    {
        name = "Goblin Warren",
        desc = "Stench of unwashed creature. Three goblins leap from the shadows!",
        enemy = Creature.new("Goblin Warrior", 30, 8, 1, 25, 8),
        item  = Item.new("Health Potion", "Restores 30 HP", 0, 30, 15),
        gold  = 12,
    },
    {
        name = "Ancient Library",
        desc = "Rows of rotting books. A skeleton slumps over a dusty tome.",
        enemy = Creature.new("Skeleton Mage", 45, 14, 2, 40, 15),
        item  = Item.new("Magic Staff", "Imbued with frost", 12, 0, 50),
        gold  = 20,
    },
    {
        name = "Crystal Cavern",
        desc = "Glittering crystals line the walls. Something massive stirs in the dark...",
        enemy = Creature.new("Crystal Golem", 70, 18, 5, 60, 25),
        item  = Item.new("Crystal Shield", "Absorbs 3 extra damage", 0, 0, 40),
        gold  = 30,
    },
    {
        name = "Dragon's Lair",
        desc = "Gold coins everywhere. An ancient dragon opens one enormous eye.",
        enemy = Creature.new("Ancient Dragon", 120, 25, 8, 150, 100),
        item  = Item.new("Dragon Scale Armor", "Legendary protection", 5, 0, 200),
        gold  = 100,
    },
}

-- ===== SHOP =====

local SHOP_ITEMS = {
    Item.new("Greater Potion", "Restores 60 HP", 0, 60, 30),
    Item.new("Enchanted Blade", "Razor-sharp edge", 18, 0, 60),
    Item.new("Iron Shield",     "Absorbs 5 damage", 0, 0, 45),
    Item.new("Max Potion",      "Fully restores HP", 0, 9999, 80),
}

local function visit_shop(player)
    printf("\n  ═══ TRAVELING MERCHANT ═══\n")
    printf("  Welcome, adventurer! You have %d gold.\n\n", player.gold)
    for i, item in ipairs(SHOP_ITEMS) do
        printf("  [%d] %s — %d gold\n", i, tostring(item), item.value)
    end
    printf("\n  Auto-buying best available items...\n")
    local bought = false
    for _, item in ipairs(SHOP_ITEMS) do
        if player.gold >= item.value then
            player.gold = player.gold - item.value
            player:pick_up(item)
            bought = true
        end
    end
    if not bought then
        printf("  Not enough gold for any items.\n")
    end
    player:equip_best_weapon()
end

-- ===== MAIN ADVENTURE =====

print(divider("═", 60))
print(center("DUNGEON ADVENTURE ENGINE", 60))
print(center("A Complete RPG in Lua — OOP & Metatables", 60))
print(divider("═", 60))

-- Initialize random with fixed seed for reproducibility
math.randomseed(12345)

local hero = Player.new("Adventurer")
hero:pick_up(Item.new("Bread", "Restores 15 HP", 0, 15, 2))
hero:equip_best_weapon()

print("\n  Your quest begins. Descend into the dungeon...")
hero:status()

local survived = true
for room_num, room in ipairs(ROOMS) do
    printf("\n%s\n", divider("─", 60))
    printf("  ROOM %d: %s\n", room_num, room.name)
    printf("  %s\n", room.desc)
    printf("%s\n", divider("─", 60))

    hero.steps = hero.steps + 1
    hero.gold = hero.gold + room.gold

    -- Shop before boss rooms
    if room_num == 3 then
        visit_shop(hero)
    end

    -- Combat
    if room.enemy then
        local won = combat(hero, room.enemy)
        if not won then
            survived = false
            break
        end
    else
        printf("  The room is clear. You rest briefly (+10 HP).\n")
        hero.hp = math.min(hero.max_hp, hero.hp + 10)
    end

    -- Loot item
    if room.item then
        hero:pick_up(room.item)
        hero:equip_best_weapon()
    end

    hero:status()
end

-- ===== FINAL RESULTS =====
printf("\n%s\n", divider("═", 60))
if survived then
    printf("%s\n", center("⭐  VICTORY! THE DUNGEON IS CONQUERED!  ⭐", 60))
else
    printf("%s\n", center("☠  GAME OVER — YOUR LEGEND LIVES ON  ☠", 60))
end
printf("%s\n", divider("═", 60))
hero:status()

-- Final score calculation
local score = hero.kills * 100 + hero.gold * 5 + hero.level * 500 + hero.steps
printf("\n  FINAL SCORE: %d\n", score)
printf("  Rank: %s\n",
    score >= 3000 and "LEGENDARY HERO" or
    score >= 2000 and "MASTER ADVENTURER" or
    score >= 1000 and "SEASONED WARRIOR" or
    "APPRENTICE ADVENTURER")

printf("\n  Inventory collected:\n")
for _, item in ipairs(hero.inventory) do
    printf("    • %s\n", tostring(item))
end

printf("\n%s\n", divider("═", 60))
printf("  Lua Dungeon Adventure Complete!\n")
printf("  OOP via metatables, closures, string.format, math.*\n")
printf("%s\n", divider("═", 60))

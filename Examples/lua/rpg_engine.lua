-- ═══════════════════════════════════════════════════════════════
--  LEGEND OF LUA: Mini RPG Engine
--  A fully playable terminal RPG demonstrating Lua's OOP,
--  closures, metatables, coroutines, and table manipulation.
-- ═══════════════════════════════════════════════════════════════

-- ─── UTILITIES ──────────────────────────────────────────────────
math.randomseed(os.time())

local function rnd(min, max)
    return math.random(min, max)
end

local function printf(fmt, ...)
    io.write(string.format(fmt, ...))
end

local function divider(char, len)
    print(string.rep(char or "─", len or 50))
end

local function choose(t)
    return t[rnd(1, #t)]
end

-- ─── ITEM SYSTEM ────────────────────────────────────────────────
local Item = {}
Item.__index = Item

function Item.new(name, kind, value, effect)
    return setmetatable({
        name = name, kind = kind,
        value = value, effect = effect
    }, Item)
end

function Item:describe()
    return string.format("[%s] %s (val:%d)", self.kind, self.name, self.value)
end

-- Item database
local ITEMS = {
    weapons = {
        Item.new("Rusty Sword",    "WEAPON", 10, {attack=5}),
        Item.new("Iron Blade",     "WEAPON", 30, {attack=12}),
        Item.new("Enchanted Staff","WEAPON", 80, {attack=20, magic=10}),
        Item.new("Dragon Slayer",  "WEAPON",150, {attack=35}),
    },
    armor = {
        Item.new("Leather Tunic", "ARMOR", 15, {defense=4}),
        Item.new("Chain Mail",    "ARMOR", 50, {defense=10}),
        Item.new("Plate Armor",   "ARMOR",120, {defense=22}),
    },
    potions = {
        Item.new("Healing Potion","POTION", 20, {hp=30}),
        Item.new("Mana Elixir",   "POTION", 25, {mp=20}),
        Item.new("Full Restore",  "POTION", 60, {hp=100, mp=50}),
    },
    misc = {
        Item.new("Magic Key",    "KEY",    5, {}),
        Item.new("Ancient Map",  "MISC",  10, {}),
        Item.new("Gold Coin",    "GOLD",   1, {}),
    }
}

-- ─── CHARACTER CLASS ────────────────────────────────────────────
local Character = {}
Character.__index = Character

function Character.new(name, class_name, stats)
    local self = setmetatable({}, Character)
    self.name      = name
    self.class     = class_name
    self.level     = 1
    self.exp       = 0
    self.exp_next  = 100
    self.gold      = 50
    self.inventory = {}
    self.equipped  = { weapon = nil, armor = nil }
    -- copy base stats
    self.base_hp  = stats.hp
    self.base_mp  = stats.mp
    self.hp       = stats.hp
    self.mp       = stats.mp
    self.attack   = stats.attack
    self.defense  = stats.defense
    self.speed    = stats.speed
    return self
end

function Character:max_hp()
    return self.base_hp + (self.level - 1) * 15
end

function Character:max_mp()
    return self.base_mp + (self.level - 1) * 8
end

function Character:equip(item)
    if item.kind == "WEAPON" then
        self.equipped.weapon = item
        self.attack = self.attack + (item.effect.attack or 0)
        printf("  ⚔  Equipped %s (+%d ATK)\n", item.name, item.effect.attack or 0)
    elseif item.kind == "ARMOR" then
        self.equipped.armor = item
        self.defense = self.defense + (item.effect.defense or 0)
        printf("  🛡  Equipped %s (+%d DEF)\n", item.name, item.effect.defense or 0)
    end
end

function Character:use_potion(item)
    if item.kind == "POTION" then
        if item.effect.hp then
            self.hp = math.min(self:max_hp(), self.hp + item.effect.hp)
            printf("  💊 Used %s: +%d HP (now %d/%d)\n",
                item.name, item.effect.hp, self.hp, self:max_hp())
        end
        if item.effect.mp then
            self.mp = math.min(self:max_mp(), self.mp + item.effect.mp)
            printf("  ✨ Restored %d MP (now %d/%d)\n",
                item.effect.mp, self.mp, self:max_mp())
        end
    end
end

function Character:gain_exp(amount)
    self.exp = self.exp + amount
    printf("  📊 +%d EXP (%d/%d)\n", amount, self.exp, self.exp_next)
    if self.exp >= self.exp_next then
        self:level_up()
    end
end

function Character:level_up()
    self.level    = self.level + 1
    self.exp      = self.exp - self.exp_next
    self.exp_next = math.floor(self.exp_next * 1.5)
    self.base_hp  = self.base_hp  + 15
    self.base_mp  = self.base_mp  + 8
    self.hp       = self:max_hp()
    self.mp       = self:max_mp()
    self.attack   = self.attack  + 3
    self.defense  = self.defense + 2
    printf("\n  🌟 LEVEL UP! Now level %d!\n", self.level)
    printf("     HP: %d  MP: %d  ATK: %d  DEF: %d\n",
        self:max_hp(), self:max_mp(), self.attack, self.defense)
end

function Character:status()
    printf("  %-12s LV%d  HP:%d/%d  MP:%d/%d  ATK:%d  DEF:%d  Gold:%d\n",
        self.name, self.level,
        self.hp, self:max_hp(),
        self.mp, self:max_mp(),
        self.attack, self.defense, self.gold)
end

function Character:is_alive()
    return self.hp > 0
end

-- ─── MONSTER SYSTEM ─────────────────────────────────────────────
local MONSTERS = {
    { name="Slime",       hp=20,  attack=4,  defense=1, exp=15, gold=5,  symbol="🟢" },
    { name="Goblin",      hp=35,  attack=8,  defense=3, exp=25, gold=10, symbol="👺" },
    { name="Wolf",        hp=45,  attack=12, defense=4, exp=35, gold=8,  symbol="🐺" },
    { name="Skeleton",    hp=55,  attack=15, defense=7, exp=50, gold=15, symbol="💀" },
    { name="Orc Warrior", hp=80,  attack=20, defense=10,exp=75, gold=25, symbol="👹" },
    { name="Dark Wizard", hp=60,  attack=28, defense=5, exp=90, gold=40, symbol="🧙" },
    { name="Dragon",      hp=200, attack=40, defense=20,exp=200,gold=100,symbol="🐉" },
}

local function spawn_monster(tier)
    tier = tier or 1
    local candidates = {}
    for i, m in ipairs(MONSTERS) do
        if i <= math.min(tier + 2, #MONSTERS) then
            candidates[#candidates+1] = m
        end
    end
    local proto  = choose(candidates)
    local scale  = 1 + (tier - 1) * 0.2
    return {
        name    = proto.name,
        symbol  = proto.symbol,
        hp      = math.floor(proto.hp * scale),
        max_hp  = math.floor(proto.hp * scale),
        attack  = math.floor(proto.attack * scale),
        defense = proto.defense,
        exp     = proto.exp,
        gold    = proto.gold,
    }
end

-- ─── BATTLE ENGINE ──────────────────────────────────────────────
local function do_battle(player, monster)
    printf("\n⚔  BATTLE: %s vs %s %s!\n", player.name, monster.symbol, monster.name)
    divider()

    local rounds = 0
    while player:is_alive() and monster.hp > 0 do
        rounds = rounds + 1
        printf("\nRound %d\n", rounds)

        -- Player attacks
        local dmg = math.max(1, player.attack - monster.defense + rnd(-3, 5))
        monster.hp = monster.hp - dmg
        printf("  → %s attacks %s for %d damage! (HP: %d/%d)\n",
            player.name, monster.name, dmg, math.max(0, monster.hp), monster.max_hp)

        if monster.hp <= 0 then break end

        -- Monster attacks
        local mdmg = math.max(1, monster.attack - player.defense + rnd(-2, 4))
        player.hp  = player.hp - mdmg
        printf("  ← %s attacks for %d damage! (HP: %d/%d)\n",
            monster.name, mdmg, math.max(0, player.hp), player:max_hp())

        -- Auto-heal if low (simple AI)
        if player.hp < player:max_hp() * 0.25 then
            for i, item in ipairs(player.inventory) do
                if item.kind == "POTION" and item.effect.hp then
                    player:use_potion(item)
                    table.remove(player.inventory, i)
                    break
                end
            end
        end
    end

    if player:is_alive() then
        printf("\n  🏆 Victory! Defeated %s in %d rounds!\n", monster.name, rounds)
        printf("  +%d EXP  +%d Gold\n", monster.exp, monster.gold)
        player.gold = player.gold + monster.gold
        player:gain_exp(monster.exp)

        -- Random loot drop
        if rnd(1, 3) == 1 then
            local loot_pool = {}
            for _, tt in ipairs({ITEMS.weapons, ITEMS.armor, ITEMS.potions}) do
                for _, it in ipairs(tt) do
                    loot_pool[#loot_pool+1] = it
                end
            end
            local loot = choose(loot_pool)
            printf("  💎 Drop: %s!\n", loot:describe())
            player.inventory[#player.inventory+1] = loot
        end
        return true
    else
        printf("\n  💔 %s was defeated by %s...\n", player.name, monster.name)
        return false
    end
end

-- ─── DUNGEON / OVERWORLD ────────────────────────────────────────
local AREAS = {
    { name="Forest Path",     tier=1, desc="A misty forest trail. Wolves and goblins lurk." },
    { name="Haunted Crypt",   tier=2, desc="Ancient tombs rattle. Undead patrol the halls." },
    { name="Orc Stronghold",  tier=3, desc="Drums beat. Orcish warriors guard every gate." },
    { name="Dragon's Peak",   tier=4, desc="Snow and fire. The Dragon awaits at the summit." },
}

local function explore(player)
    print("\n🗺  EXPLORE - Choose an area:")
    for i, area in ipairs(AREAS) do
        printf("  [%d] %s (Tier %d) - %s\n", i, area.name, area.tier, area.desc)
    end
    io.write("  Choice: ")
    local choice = tonumber(io.read()) or 1
    choice = math.max(1, math.min(#AREAS, choice))
    local area = AREAS[choice]

    printf("\n🌄 Entering %s...\n", area.name)
    local encounters = rnd(1, 3)
    for i = 1, encounters do
        if not player:is_alive() then break end
        local monster = spawn_monster(area.tier)
        do_battle(player, monster)
    end

    -- Treasure chest chance
    if rnd(1, 4) == 1 and player:is_alive() then
        printf("\n📦 You found a chest!\n")
        local loot = choose(ITEMS.potions)
        printf("  Contents: %s\n", loot:describe())
        player.inventory[#player.inventory+1] = loot
    end
end

-- ─── SHOP ───────────────────────────────────────────────────────
local SHOP_STOCK = {
    ITEMS.weapons[2], ITEMS.weapons[3],
    ITEMS.armor[2],
    ITEMS.potions[1], ITEMS.potions[2], ITEMS.potions[3],
}

local function shop(player)
    printf("\n🏪 SHOP  (Your Gold: %d)\n", player.gold)
    divider()
    for i, item in ipairs(SHOP_STOCK) do
        printf("  [%d] %-20s  %d gold\n", i, item.name, item.value)
    end
    printf("  [0] Leave\n  Buy: ")
    local choice = tonumber(io.read()) or 0
    if choice >= 1 and choice <= #SHOP_STOCK then
        local item = SHOP_STOCK[choice]
        if player.gold >= item.value then
            player.gold = player.gold - item.value
            player.inventory[#player.inventory+1] = item
            printf("  ✅ Bought %s for %d gold.\n", item.name, item.value)
        else
            print("  ❌ Not enough gold!")
        end
    end
end

-- ─── CLASS SELECTION ────────────────────────────────────────────
local CLASSES = {
    { name="Warrior", stats={ hp=120, mp=20,  attack=18, defense=14, speed=8  } },
    { name="Mage",    stats={ hp=70,  mp=100, attack=25, defense=6,  speed=10 } },
    { name="Rogue",   stats={ hp=90,  mp=40,  attack=22, defense=8,  speed=15 } },
    { name="Paladin", stats={ hp=110, mp=60,  attack=16, defense=16, speed=7  } },
}

-- ─── MAIN GAME LOOP ─────────────────────────────────────────────
local function main()
    print("╔══════════════════════════════════════════╗")
    print("║        LEGEND OF LUA - Mini RPG          ║")
    print("╚══════════════════════════════════════════╝")

    print("\n  Choose your class:")
    for i, c in ipairs(CLASSES) do
        printf("  [%d] %-8s  HP:%-4d MP:%-4d ATK:%-3d DEF:%-3d\n",
            i, c.name, c.stats.hp, c.stats.mp, c.stats.attack, c.stats.defense)
    end
    io.write("  Choice [1-4]: ")
    local ci = tonumber(io.read()) or 1
    ci = math.max(1, math.min(#CLASSES, ci))

    io.write("  Enter hero name: ")
    local hero_name = io.read() or "Hero"
    local cls = CLASSES[ci]
    local player = Character.new(hero_name, cls.name, cls.stats)

    -- Starting gear
    player:equip(ITEMS.weapons[1])
    player:equip(ITEMS.armor[1])
    player.inventory[#player.inventory+1] = ITEMS.potions[1]
    player.inventory[#player.inventory+1] = ITEMS.potions[1]

    printf("\n  Welcome, %s the %s!\n", player.name, player.class)

    -- Main loop
    while player:is_alive() do
        divider("═")
        printf("\n  %s the %s  |  Level %d\n", player.name, player.class, player.level)
        player:status()
        print("\n  What will you do?")
        print("  [1] Explore   [2] Rest (+20 HP)   [3] Shop")
        print("  [4] Inventory [5] Status           [0] Quit")
        io.write("  > ")
        local action = tonumber(io.read()) or 1

        if action == 0 then
            printf("\n  Farewell, %s! Your legend lives on.\n", player.name)
            break
        elseif action == 1 then
            explore(player)
        elseif action == 2 then
            player.hp = math.min(player:max_hp(), player.hp + 20)
            printf("  😴 Rested. HP: %d/%d\n", player.hp, player:max_hp())
        elseif action == 3 then
            shop(player)
        elseif action == 4 then
            printf("\n  📦 Inventory (%d items):\n", #player.inventory)
            if #player.inventory == 0 then
                print("  (empty)")
            else
                for i, item in ipairs(player.inventory) do
                    printf("  [%d] %s\n", i, item:describe())
                end
                io.write("  Use item [0 to cancel]: ")
                local ic = tonumber(io.read()) or 0
                if ic >= 1 and ic <= #player.inventory then
                    local item = player.inventory[ic]
                    if item.kind == "POTION" then
                        player:use_potion(item)
                        table.remove(player.inventory, ic)
                    elseif item.kind == "WEAPON" or item.kind == "ARMOR" then
                        player:equip(item)
                        table.remove(player.inventory, ic)
                    end
                end
            end
        elseif action == 5 then
            print("\n  📊 Full Status:")
            player:status()
            if player.equipped.weapon then
                printf("  Weapon: %s\n", player.equipped.weapon.name)
            end
            if player.equipped.armor then
                printf("  Armor:  %s\n", player.equipped.armor.name)
            end
        end

        if not player:is_alive() then
            divider("═")
            printf("\n  💀 GAME OVER\n  %s fell in battle. Final level: %d\n",
                player.name, player.level)
        end
    end
end

main()

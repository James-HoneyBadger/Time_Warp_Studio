# ============================================================
# ECOSYSTEM SIMULATION — Ruby OOP Showcase
# Predator-Prey Dynamics * Lotka-Volterra * Evolution
# Time Warp Studio — Ruby Language Demo
# ============================================================

# ===== BASE ORGANISM CLASS =====

class Organism
  attr_accessor :name, :age, :energy, :alive

  @@total_born  = 0
  @@total_died  = 0

  def initialize(name, energy)
    @name      = name
    @age       = 0
    @energy    = energy
    @alive     = true
    @@total_born += 1
  end

  def self.total_born  = @@total_born
  def self.total_died  = @@total_died
  def self.total_died_inc; @@total_died += 1; end

  def die
    @alive = false
    @energy = 0
    Organism.total_died_inc
  end

  def age!
    @age += 1
    @energy -= 1
    die if @energy <= 0
  end

  def to_s
    "#{@name}(age=#{@age},E=#{@energy})"
  end
end

# ===== PLANT CLASS =====

class Plant < Organism
  @@count = 0

  def initialize
    @@count += 1
    super("Plant##{@@count}", rand(20..40))
  end

  def self.count = @@count

  def photosynthesize
    return unless @alive
    @energy += rand(3..8)
    @energy = [@energy, 50].min
  end

  def reproduce?
    @alive && @energy >= 35 && rand < 0.15
  end
end

# ===== HERBIVORE CLASS =====

class Herbivore < Organism
  attr_accessor :species
  @@count = 0

  SPECIES = %w[Rabbit Deer Mouse Sheep]

  def initialize
    @@count += 1
    @species = SPECIES.sample
    super("#{@species}##{@@count}", rand(30..50))
  end

  def self.count = @@count

  def eat(plants)
    return unless @alive
    plant = plants.select(&:alive).first
    return unless plant
    gained = [plant.energy, rand(8..15)].min
    plant.energy -= gained
    plant.die if plant.energy <= 0
    @energy += gained
  end

  def reproduce?
    @alive && @energy >= 40 && @age >= 2 && rand < 0.12
  end
end

# ===== PREDATOR CLASS =====

class Predator < Organism
  attr_accessor :species, :kills
  @@count = 0

  SPECIES = %w[Wolf Fox Eagle Lynx]

  def initialize
    @@count += 1
    @species = SPECIES.sample
    @kills   = 0
    super("#{@species}##{@@count}", rand(40..70))
  end

  def self.count = @@count

  def hunt(prey_list)
    return unless @alive
    target = prey_list.select(&:alive).first
    return unless target
    success_chance = @energy > 30 ? 0.65 : 0.40
    if rand < success_chance
      gained = rand(20..35)
      @energy += gained
      @kills += 1
      target.die
      gained
    else
      @energy -= 3  # energy cost of failed hunt
      nil
    end
  end

  def reproduce?
    @alive && @energy >= 55 && @kills >= 2 && rand < 0.08
  end
end

# ===== WORLD CLASS =====

class World
  attr_reader :year, :history

  def initialize(plants: 50, herbivores: 20, predators: 5)
    @year       = 0
    @plants     = Array.new(plants)    { Plant.new }
    @herbivores = Array.new(herbivores) { Herbivore.new }
    @predators  = Array.new(predators)  { Predator.new }
    @history    = []
    record_state
  end

  def plant_count      = @plants.count(&:alive)
  def herbivore_count  = @herbivores.count(&:alive)
  def predator_count   = @predators.count(&:alive)

  def tick
    @year += 1

    # Plants photosynthesize and age
    @plants.each { |p| p.photosynthesize if p.alive }

    # New plants (reproduction)
    @plants.select { |p| p.alive && p.reproduce? }.each do
      @plants << Plant.new
    end

    # Herbivores eat, age, die
    @herbivores.shuffle.each do |h|
      h.eat(@plants.select(&:alive).shuffle) if h.alive
      h.age! if h.alive
    end

    # New herbivores
    @herbivores.select { |h| h.alive && h.reproduce? }.each do
      @herbivores << Herbivore.new
    end

    # Predators hunt, age, die
    @predators.shuffle.each do |pred|
      pred.hunt(@herbivores.select(&:alive).shuffle) if pred.alive
      pred.age! if pred.alive
    end

    # New predators
    @predators.select { |p| p.alive && p.reproduce? }.each do
      @predators << Predator.new
    end

    # Pruning dead organisms
    @plants      = @plants.select(&:alive).first(200)
    @herbivores  = @herbivores.select(&:alive).first(150)
    @predators   = @predators.select(&:alive).first(40)

    # Prevent total extinction events (seed population)
    @plants << Plant.new if plant_count == 0
    @herbivores << Herbivore.new if herbivore_count == 0

    record_state
  end

  def record_state
    @history << {
      year:        @year,
      plants:      plant_count,
      herbivores:  herbivore_count,
      predators:   predator_count,
    }
  end

  def statistics
    plant_avg = @history.map { |h| h[:plants] }.sum.to_f / @history.size
    herb_avg  = @history.map { |h| h[:herbivores] }.sum.to_f / @history.size
    pred_avg  = @history.map { |h| h[:predators] }.sum.to_f / @history.size

    plant_max = @history.map { |h| h[:plants] }.max
    herb_max  = @history.map { |h| h[:herbivores] }.max
    pred_max  = @history.map { |h| h[:predators] }.max

    {
      plant_avg:   plant_avg.round(1),  plant_max:  plant_max,
      herb_avg:    herb_avg.round(1),   herb_max:   herb_max,
      pred_avg:    pred_avg.round(1),   pred_max:   pred_max,
      total_born:  Organism.total_born,
      total_died:  Organism.total_died,
    }
  end
end

# ===== REPORTER MODULE =====

module Reporter
  BAR_WIDTH = 40

  def self.bar(value, max, char = '█')
    filled = max > 0 ? (value.to_f / max * BAR_WIDTH).round : 0
    char * [filled, BAR_WIDTH].min
  end

  def self.snapshot(world)
    h = world.history.last
    p_bar = bar(h[:plants],     200, '🌿')
    r_bar = bar(h[:herbivores], 150, '🐇')
    w_bar = bar(h[:predators],   40, '🐺')
    puts format("  Year %3d │ Plants:%3d │ Herbivores:%3d │ Predators:%2d",
                h[:year], h[:plants], h[:herbivores], h[:predators])
  end

  def self.ascii_chart(history, key, label, max_val, char = '█')
    puts "\n  #{label} over time (#{history.size} years):"
    # Show every 5th year in a mini chart
    history.each_with_index do |h, i|
      next if i % 5 != 0 && i != history.size - 1
      val    = h[key]
      filled = max_val > 0 ? (val.to_f / max_val * 30).round : 0
      puts format("  Yr%3d │%s│ %d", h[:year], char * filled + ' ' * (30 - filled), val)
    end
  end
end

# ===== MAIN SIMULATION =====

puts "=" * 60
puts "  ECOSYSTEM SIMULATION — Ruby OOP Showcase"
puts "  Lotka-Volterra Predator-Prey Population Dynamics"
puts "=" * 60

world = World.new(plants: 60, herbivores: 25, predators: 6)

puts "\n  Simulating 50 years of ecosystem dynamics..."
puts "  " + "-" * 56

# Run and print every 5 years
51.times do |y|
  world.tick if y > 0
  Reporter.snapshot(world) if y % 5 == 0
end

# Final stats
puts "\n" + "=" * 60
puts "  SIMULATION STATISTICS"
puts "=" * 60

stats = world.statistics
puts "\n  Population averages over 50 years:"
puts "    Plants:     avg=#{stats[:plant_avg]}  max=#{stats[:plant_max]}"
puts "    Herbivores: avg=#{stats[:herb_avg]}  max=#{stats[:herb_max]}"
puts "    Predators:  avg=#{stats[:pred_avg]}  max=#{stats[:pred_max]}"
puts "\n  Lifecycle statistics:"
puts "    Total organisms ever born: #{stats[:total_born]}"
puts "    Total organisms that died: #{stats[:total_died]}"
puts "    Net survivors: #{stats[:total_born] - stats[:total_died]}"

# Population charts
Reporter.ascii_chart(world.history, :plants,     "PLANT POPULATION",     150, '🌿')
Reporter.ascii_chart(world.history, :herbivores, "HERBIVORE POPULATION", 100, '🐇')
Reporter.ascii_chart(world.history, :predators,  "PREDATOR POPULATION",   30, '🐺')

# Lotka-Volterra analysis
puts "\n  LOTKA-VOLTERRA ANALYSIS"
puts "  In predator-prey models:"
puts "  • When prey increase → predators increase (more food)"
puts "  • When predators increase → prey decrease (more hunting)"
puts "  • When prey decrease → predators decrease (less food)"
puts "  • When predators decrease → prey rebound (less pressure)"
puts "  This creates oscillating cycles as seen in nature."

# Species summary
puts "\n  FINAL POPULATION STATE (Year 50):"
last = world.history.last
puts "    Plants:     #{last[:plants]} surviving"
puts "    Herbivores: #{last[:herbivores]} surviving"
puts "    Predators:  #{last[:predators]} surviving"
if last[:predators] == 0
  puts "    ⚠  Predators went extinct — herbivore explosion risk!"
elsif last[:herbivores] == 0
  puts "    ⚠  Herbivores went extinct — predator collapse risk!"
else
  puts "    ✓  Stable ecosystem maintained!"
end

puts "\n" + "=" * 60
puts "  Ruby Ecosystem Simulation Complete!"
puts "  Classes | Inheritance | Modules | Blocks | Iterators"
puts "=" * 60

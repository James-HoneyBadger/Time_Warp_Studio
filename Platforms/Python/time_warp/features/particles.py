"""
Particle system for Time Warp Studio.
Simple particle effects for games.
"""

import math
import random
from dataclasses import dataclass, field
from typing import List, Optional, Tuple


@dataclass
class Particle:
    """A single particle in the system."""

    x: float
    y: float
    vx: float  # Velocity X
    vy: float  # Velocity Y
    life: float  # Remaining life (0.0 to 1.0)
    decay: float  # Life decay per frame
    size: float
    color: Tuple[int, int, int]  # RGB
    alpha: float = 1.0  # Transparency


@dataclass
class ParticleEmitter:
    """Emits particles with configurable properties."""

    x: float
    y: float
    active: bool = True
    particles: List[Particle] = field(default_factory=list)

    # Emission settings
    rate: float = 10.0  # Particles per frame
    burst: int = 0  # One-time burst count

    # Particle properties
    speed_min: float = 1.0
    speed_max: float = 5.0
    angle_min: float = 0.0  # Degrees
    angle_max: float = 360.0
    life_min: float = 0.5
    life_max: float = 1.0
    size_min: float = 2.0
    size_max: float = 5.0
    decay_min: float = 0.02
    decay_max: float = 0.05

    # Physics
    gravity: float = 0.0
    friction: float = 1.0  # 1.0 = no friction

    # Colors
    color_start: Tuple[int, int, int] = (255, 255, 255)
    color_end: Tuple[int, int, int] = (255, 255, 255)

    # Internal state
    _emit_accumulator: float = 0.0

    def emit_particle(self) -> Particle:
        """Emit a single particle."""
        angle = math.radians(random.uniform(self.angle_min, self.angle_max))
        speed = random.uniform(self.speed_min, self.speed_max)

        return Particle(
            x=self.x,
            y=self.y,
            vx=math.cos(angle) * speed,
            vy=math.sin(angle) * speed,
            life=random.uniform(self.life_min, self.life_max),
            decay=random.uniform(self.decay_min, self.decay_max),
            size=random.uniform(self.size_min, self.size_max),
            color=self.color_start,
            alpha=1.0,
        )

    def update(self, dt: float = 1.0):
        """Update all particles.

        Args:
            dt: Delta time multiplier (1.0 = normal speed)
        """
        # Emit new particles
        if self.active:
            if self.burst > 0:
                for _ in range(self.burst):
                    self.particles.append(self.emit_particle())
                self.burst = 0
            else:
                self._emit_accumulator += self.rate * dt
                while self._emit_accumulator >= 1.0:
                    self.particles.append(self.emit_particle())
                    self._emit_accumulator -= 1.0

        # Update existing particles
        alive_particles = []
        for p in self.particles:
            # Apply physics
            p.vy += self.gravity * dt
            p.vx *= self.friction
            p.vy *= self.friction
            p.x += p.vx * dt
            p.y += p.vy * dt

            # Update life
            p.life -= p.decay * dt

            if p.life > 0:
                # Update alpha based on life
                p.alpha = min(1.0, p.life * 2)

                # Interpolate color
                t = 1.0 - p.life
                cs, ce = self.color_start, self.color_end
                p.color = (
                    int(cs[0] + t * (ce[0] - cs[0])),
                    int(cs[1] + t * (ce[1] - cs[1])),
                    int(cs[2] + t * (ce[2] - cs[2])),
                )

                alive_particles.append(p)

        self.particles = alive_particles

    def clear(self):
        """Remove all particles."""
        self.particles.clear()
        self._emit_accumulator = 0.0


class ParticlePresets:
    """Pre-configured particle effects."""

    @staticmethod
    def explosion(x: float, y: float, intensity: float = 1.0) -> ParticleEmitter:
        """Create an explosion effect."""
        emitter = ParticleEmitter(x=x, y=y, active=False)
        emitter.burst = int(50 * intensity)
        emitter.speed_min = 3.0 * intensity
        emitter.speed_max = 10.0 * intensity
        emitter.angle_min = 0.0
        emitter.angle_max = 360.0
        emitter.life_min = 0.3
        emitter.life_max = 0.8
        emitter.size_min = 3.0
        emitter.size_max = 8.0
        emitter.decay_min = 0.03
        emitter.decay_max = 0.08
        emitter.gravity = 0.2
        emitter.color_start = (255, 200, 50)  # Yellow
        emitter.color_end = (255, 50, 0)  # Red
        return emitter

    @staticmethod
    def fire(x: float, y: float) -> ParticleEmitter:
        """Create a fire effect."""
        emitter = ParticleEmitter(x=x, y=y)
        emitter.rate = 15.0
        emitter.speed_min = 1.0
        emitter.speed_max = 3.0
        emitter.angle_min = 250.0  # Upward (screen Y is inverted)
        emitter.angle_max = 290.0
        emitter.life_min = 0.5
        emitter.life_max = 1.0
        emitter.size_min = 5.0
        emitter.size_max = 10.0
        emitter.decay_min = 0.02
        emitter.decay_max = 0.05
        emitter.gravity = -0.1  # Rise up
        emitter.color_start = (255, 200, 50)  # Yellow
        emitter.color_end = (255, 50, 0)  # Red
        return emitter

    @staticmethod
    def smoke(x: float, y: float) -> ParticleEmitter:
        """Create a smoke effect."""
        emitter = ParticleEmitter(x=x, y=y)
        emitter.rate = 8.0
        emitter.speed_min = 0.5
        emitter.speed_max = 2.0
        emitter.angle_min = 250.0
        emitter.angle_max = 290.0
        emitter.life_min = 1.0
        emitter.life_max = 2.0
        emitter.size_min = 8.0
        emitter.size_max = 15.0
        emitter.decay_min = 0.01
        emitter.decay_max = 0.02
        emitter.gravity = -0.05
        emitter.color_start = (150, 150, 150)  # Gray
        emitter.color_end = (50, 50, 50)  # Dark gray
        return emitter

    @staticmethod
    def sparkle(x: float, y: float) -> ParticleEmitter:
        """Create a sparkle/glitter effect."""
        emitter = ParticleEmitter(x=x, y=y)
        emitter.rate = 5.0
        emitter.speed_min = 0.5
        emitter.speed_max = 2.0
        emitter.angle_min = 0.0
        emitter.angle_max = 360.0
        emitter.life_min = 0.2
        emitter.life_max = 0.5
        emitter.size_min = 1.0
        emitter.size_max = 3.0
        emitter.decay_min = 0.05
        emitter.decay_max = 0.1
        emitter.friction = 0.95
        emitter.color_start = (255, 255, 100)  # Bright yellow
        emitter.color_end = (255, 255, 255)  # White
        return emitter

    @staticmethod
    def rain(width: float, _height: float) -> ParticleEmitter:
        """Create a rain effect covering an area."""
        emitter = ParticleEmitter(x=width / 2, y=0)
        emitter.rate = 30.0
        emitter.speed_min = 8.0
        emitter.speed_max = 12.0
        emitter.angle_min = 85.0  # Slightly angled down-right
        emitter.angle_max = 95.0
        emitter.life_min = 1.0
        emitter.life_max = 1.5
        emitter.size_min = 1.0
        emitter.size_max = 2.0
        emitter.decay_min = 0.02
        emitter.decay_max = 0.03
        emitter.color_start = (100, 150, 255)  # Light blue
        emitter.color_end = (50, 100, 200)  # Darker blue
        return emitter

    @staticmethod
    def snow(width: float) -> ParticleEmitter:
        """Create a snow effect."""
        emitter = ParticleEmitter(x=width / 2, y=0)
        emitter.rate = 10.0
        emitter.speed_min = 1.0
        emitter.speed_max = 3.0
        emitter.angle_min = 80.0
        emitter.angle_max = 100.0
        emitter.life_min = 2.0
        emitter.life_max = 4.0
        emitter.size_min = 2.0
        emitter.size_max = 5.0
        emitter.decay_min = 0.005
        emitter.decay_max = 0.01
        emitter.friction = 0.99
        emitter.color_start = (255, 255, 255)  # White
        emitter.color_end = (200, 200, 255)  # Light blue
        return emitter

    @staticmethod
    def confetti(x: float, y: float) -> ParticleEmitter:
        """Create a confetti burst effect."""
        emitter = ParticleEmitter(x=x, y=y, active=False)
        emitter.burst = 100
        emitter.speed_min = 2.0
        emitter.speed_max = 8.0
        emitter.angle_min = 200.0
        emitter.angle_max = 340.0
        emitter.life_min = 1.0
        emitter.life_max = 2.0
        emitter.size_min = 3.0
        emitter.size_max = 6.0
        emitter.decay_min = 0.01
        emitter.decay_max = 0.02
        emitter.gravity = 0.15
        emitter.friction = 0.99
        # Random bright colors
        emitter.color_start = (255, 100, 100)
        emitter.color_end = (100, 100, 255)
        return emitter

    @staticmethod
    def trail(x: float, y: float) -> ParticleEmitter:
        """Create a following trail effect."""
        emitter = ParticleEmitter(x=x, y=y)
        emitter.rate = 20.0
        emitter.speed_min = 0.0
        emitter.speed_max = 0.5
        emitter.angle_min = 0.0
        emitter.angle_max = 360.0
        emitter.life_min = 0.3
        emitter.life_max = 0.6
        emitter.size_min = 3.0
        emitter.size_max = 6.0
        emitter.decay_min = 0.05
        emitter.decay_max = 0.1
        emitter.color_start = (100, 200, 255)  # Cyan
        emitter.color_end = (255, 100, 255)  # Magenta
        return emitter


class ParticleSystem:
    """Manages multiple particle emitters."""

    def __init__(self):
        self.emitters: List[ParticleEmitter] = []
        self.presets = ParticlePresets()

    def add_emitter(self, emitter: ParticleEmitter) -> int:
        """Add an emitter and return its index."""
        self.emitters.append(emitter)
        return len(self.emitters) - 1

    def remove_emitter(self, index: int):
        """Remove an emitter by index."""
        if 0 <= index < len(self.emitters):
            self.emitters.pop(index)

    def create_effect(self, effect_name: str, x: float, y: float, **kwargs) -> int:
        """Create a preset effect at the given position.

        Args:
            effect_name: Name of the preset (explosion, fire, smoke, etc.)
            x, y: Position
            **kwargs: Additional arguments for the preset

        Returns:
            Emitter index
        """
        effect_name = effect_name.lower()

        if effect_name == "explosion":
            intensity = kwargs.get("intensity", 1.0)
            emitter = self.presets.explosion(x, y, intensity)
        elif effect_name == "fire":
            emitter = self.presets.fire(x, y)
        elif effect_name == "smoke":
            emitter = self.presets.smoke(x, y)
        elif effect_name == "sparkle":
            emitter = self.presets.sparkle(x, y)
        elif effect_name == "rain":
            width = kwargs.get("width", 800)
            height = kwargs.get("height", 600)
            emitter = self.presets.rain(width, height)
        elif effect_name == "snow":
            width = kwargs.get("width", 800)
            emitter = self.presets.snow(width)
        elif effect_name == "confetti":
            emitter = self.presets.confetti(x, y)
        elif effect_name == "trail":
            emitter = self.presets.trail(x, y)
        else:
            # Default simple emitter
            emitter = ParticleEmitter(x=x, y=y)

        return self.add_emitter(emitter)

    def update(self, dt: float = 1.0):
        """Update all emitters."""
        for emitter in self.emitters:
            emitter.update(dt)

        # Remove inactive emitters with no particles
        self.emitters = [e for e in self.emitters if e.active or len(e.particles) > 0]

    def get_all_particles(self) -> List[Particle]:
        """Get all particles from all emitters."""
        all_particles = []
        for emitter in self.emitters:
            all_particles.extend(emitter.particles)
        return all_particles

    def clear(self):
        """Clear all emitters and particles."""
        self.emitters.clear()

    def total_particles(self) -> int:
        """Get total particle count across all emitters."""
        return sum(len(e.particles) for e in self.emitters)


# Global instance
_particle_system: Optional[ParticleSystem] = None


def get_particle_system() -> ParticleSystem:
    """Get the global particle system instance."""
    global _particle_system  # pylint: disable=global-statement
    if _particle_system is None:
        _particle_system = ParticleSystem()
    return _particle_system

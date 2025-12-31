"""Hardware simulation dashboard for IoT/robotics development."""

import json
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Tuple


class HardwareType(Enum):
    """Types of simulated hardware."""

    RASPBERRY_PI = "raspberry_pi"
    ARDUINO = "arduino"
    GENERIC_SENSOR = "sensor"
    GENERIC_ACTUATOR = "actuator"
    LED = "led"
    BUTTON = "button"
    MOTOR = "motor"
    SERVO = "servo"
    TEMPERATURE_SENSOR = "temperature_sensor"
    DISTANCE_SENSOR = "distance_sensor"


class SensorType(Enum):
    """Types of sensors."""

    TEMPERATURE = "temperature"
    HUMIDITY = "humidity"
    DISTANCE = "distance"
    LIGHT = "light"
    ACCELERATION = "acceleration"
    GYROSCOPE = "gyroscope"
    PRESSURE = "pressure"
    GAS = "gas"


@dataclass
class SensorReading:
    """A sensor reading."""

    sensor_id: str
    sensor_type: SensorType
    value: float
    unit: str
    timestamp: float
    is_simulated: bool = True


@dataclass
class ActuatorCommand:
    """Command to an actuator."""

    actuator_id: str
    command: str
    value: Any
    timestamp: float = field(default_factory=time.time)


@dataclass
class SimulatedDevice:
    """Simulated hardware device."""

    device_id: str
    device_type: HardwareType
    pin: int
    is_active: bool = False
    current_value: float = 0.0
    readings: List[SensorReading] = field(default_factory=list)
    commands: List[ActuatorCommand] = field(default_factory=list)
    last_update: float = field(default_factory=time.time)


class HardwareSimulator:
    """Simulates IoT/robotics hardware for development."""

    def __init__(self):
        """Initialize hardware simulator."""
        self.devices: Dict[str, SimulatedDevice] = {}
        self.virtual_environment = (
            {}
        )  # Environmental simulation (temp, light, etc.)
        self.callbacks: Dict[str, List[Callable]] = {}
        self.recording_enabled = False
        self.event_log: List[Dict] = []

    def add_device(
        self, device_id: str, device_type: HardwareType, pin: int
    ) -> SimulatedDevice:
        """Add a simulated device."""
        device = SimulatedDevice(
            device_id=device_id, device_type=device_type, pin=pin
        )
        self.devices[device_id] = device
        self._trigger_event("device_added", device)
        return device

    def remove_device(self, device_id: str) -> bool:
        """Remove a simulated device."""
        if device_id in self.devices:
            del self.devices[device_id]
            self._trigger_event("device_removed", device_id)
            return True
        return False

    def activate_device(
        self, device_id: str, value: Optional[float] = None
    ) -> bool:
        """Activate a device (turn on LED, etc.)."""
        if device_id not in self.devices:
            return False

        device = self.devices[device_id]
        device.is_active = True
        if value is not None:
            device.current_value = value

        self._log_event(
            "device_activated", {"device_id": device_id, "value": value}
        )
        self._trigger_event("device_activated", device_id, value)
        return True

    def deactivate_device(self, device_id: str) -> bool:
        """Deactivate a device (turn off LED, etc.)."""
        if device_id not in self.devices:
            return False

        device = self.devices[device_id]
        device.is_active = False
        device.current_value = 0.0

        self._log_event("device_deactivated", {"device_id": device_id})
        self._trigger_event("device_deactivated", device_id)
        return True

    def set_device_value(self, device_id: str, value: float) -> bool:
        """Set device value (brightness, angle, speed, etc.)."""
        if device_id not in self.devices:
            return False

        device = self.devices[device_id]
        device.current_value = max(0, min(100, value))  # Clamp 0-100

        self._log_event(
            "device_value_set",
            {"device_id": device_id, "value": device.current_value},
        )
        self._trigger_event(
            "device_value_changed", device_id, device.current_value
        )
        return True

    def read_sensor(self, sensor_id: str) -> Optional[SensorReading]:
        """
        Read from a sensor device.

        Returns: Latest sensor reading or None
        """
        if sensor_id not in self.devices:
            return None

        device = self.devices[sensor_id]

        # Generate realistic simulated data
        reading = self._generate_sensor_data(sensor_id, device)
        device.readings.append(reading)

        self._log_event(
            "sensor_read",
            {
                "sensor_id": sensor_id,
                "value": reading.value,
                "unit": reading.unit,
            },
        )
        self._trigger_event("sensor_reading", reading)

        return reading

    def inject_sensor_data(
        self,
        sensor_id: str,
        value: float,
        sensor_type: SensorType = SensorType.TEMPERATURE,
    ):
        """Inject test data into sensor for testing."""
        if sensor_id not in self.devices:
            return

        device = self.devices[sensor_id]

        reading = SensorReading(
            sensor_id=sensor_id,
            sensor_type=sensor_type,
            value=value,
            unit=self._get_unit_for_sensor(sensor_type),
            timestamp=time.time(),
            is_simulated=False,
        )

        device.readings.append(reading)
        self._trigger_event("sensor_reading", reading)

    def simulate_motion(
        self,
        duration: float,
        acceleration_x: float = 0,
        acceleration_y: float = 0,
        acceleration_z: float = 0,
    ):
        """Simulate motion sensor data."""
        device_id = "accelerometer"
        if device_id not in self.devices:
            self.add_device(device_id, HardwareType.GENERIC_SENSOR, -1)

        readings = []
        steps = int(duration * 10)  # 10 samples per second

        for i in range(steps):
            reading = SensorReading(
                sensor_id=device_id,
                sensor_type=SensorType.ACCELERATION,
                value=acceleration_x,
                unit="m/s²",
                timestamp=time.time() + (i * 0.1),
            )
            readings.append(reading)

        self.devices[device_id].readings.extend(readings)
        self._trigger_event("motion_simulated", readings)

    def set_environment(self, **kwargs):
        """
        Set environmental conditions for sensors.

        Examples:
            temperature=25, humidity=60, light_level=800
        """
        self.virtual_environment.update(kwargs)
        self._log_event("environment_changed", kwargs)
        self._trigger_event("environment_changed", self.virtual_environment)

    def get_environment(self) -> Dict[str, float]:
        """Get current environmental conditions."""
        return self.virtual_environment.copy()

    def get_device_status(self, device_id: str) -> Optional[Dict[str, Any]]:
        """Get status of a device."""
        if device_id not in self.devices:
            return None

        device = self.devices[device_id]
        return {
            "device_id": device.device_id,
            "type": device.device_type.value,
            "pin": device.pin,
            "is_active": device.is_active,
            "current_value": device.current_value,
            "reading_count": len(device.readings),
            "last_reading": device.readings[-1] if device.readings else None,
            "last_update": device.last_update,
        }

    def get_all_device_status(self) -> List[Dict[str, Any]]:
        """Get status of all devices."""
        return [
            self.get_device_status(device_id) for device_id in self.devices
        ]

    def get_sensor_history(
        self, sensor_id: str, limit: Optional[int] = None
    ) -> List[SensorReading]:
        """Get historical sensor readings."""
        if sensor_id not in self.devices:
            return []

        readings = self.devices[sensor_id].readings
        if limit:
            readings = readings[-limit:]

        return readings

    def export_log(self) -> str:
        """Export event log as JSON."""
        return json.dumps(self.event_log, indent=2, default=str)

    def clear_log(self):
        """Clear event log."""
        self.event_log.clear()

    def _generate_sensor_data(
        self, sensor_id: str, device: SimulatedDevice
    ) -> SensorReading:
        """Generate realistic simulated sensor data."""
        sensor_type = SensorType.TEMPERATURE  # Default

        # Determine sensor type from device
        if "temp" in sensor_id.lower():
            sensor_type = SensorType.TEMPERATURE
            value = self.virtual_environment.get("temperature", 25.0)
            value += 0.1 * (len(device.readings) % 10)  # Slight variation
        elif "humidity" in sensor_id.lower():
            sensor_type = SensorType.HUMIDITY
            value = self.virtual_environment.get("humidity", 50.0)
        elif "distance" in sensor_id.lower():
            sensor_type = SensorType.DISTANCE
            value = 50.0  # 50cm default
        elif "light" in sensor_id.lower():
            sensor_type = SensorType.LIGHT
            value = self.virtual_environment.get("light_level", 500.0)
        else:
            value = device.current_value

        return SensorReading(
            sensor_id=sensor_id,
            sensor_type=sensor_type,
            value=value,
            unit=self._get_unit_for_sensor(sensor_type),
            timestamp=time.time(),
        )

    def _get_unit_for_sensor(self, sensor_type: SensorType) -> str:
        """Get unit of measurement for sensor type."""
        units = {
            SensorType.TEMPERATURE: "°C",
            SensorType.HUMIDITY: "%",
            SensorType.DISTANCE: "cm",
            SensorType.LIGHT: "lux",
            SensorType.ACCELERATION: "m/s²",
            SensorType.GYROSCOPE: "°/s",
            SensorType.PRESSURE: "hPa",
            SensorType.GAS: "ppm",
        }
        return units.get(sensor_type, "units")

    def on_event(self, event_type: str, callback: Callable):
        """Register event callback."""
        if event_type not in self.callbacks:
            self.callbacks[event_type] = []
        self.callbacks[event_type].append(callback)

    def _trigger_event(self, event_type: str, *args):
        """Trigger event callbacks."""
        if event_type in self.callbacks:
            for callback in self.callbacks[event_type]:
                try:
                    callback(*args)
                except Exception as e:
                    print(f"Callback error: {e}")

    def _log_event(self, event_type: str, data: Dict):
        """Log an event."""
        if self.recording_enabled:
            self.event_log.append(
                {"type": event_type, "timestamp": time.time(), "data": data}
            )


class RobotSimulator:
    """High-level robot simulator for navigation and control."""

    def __init__(self):
        """Initialize robot simulator."""
        self.x = 0.0  # Position X
        self.y = 0.0  # Position Y
        self.angle = 0.0  # Heading in degrees (0 = north)
        self.speed = 0.0  # Current speed
        self.path_history: List[Tuple[float, float]] = [(0.0, 0.0)]
        self.hardware = HardwareSimulator()

    def move_forward(self, distance: float):
        """Move forward by distance (units)."""
        import math

        rad = math.radians(self.angle)
        self.x += distance * math.sin(rad)
        self.y += distance * math.cos(rad)
        self.path_history.append((self.x, self.y))

    def turn(self, angle: float):
        """Turn by angle (degrees)."""
        self.angle = (self.angle + angle) % 360

    def set_speed(self, speed: float):
        """Set motor speed (0-100)."""
        self.speed = max(0, min(100, speed))

    def get_position(self) -> Tuple[float, float]:
        """Get current position."""
        return (self.x, self.y)

    def get_path(self) -> List[Tuple[float, float]]:
        """Get path history."""
        return self.path_history.copy()

    def reset(self):
        """Reset robot to origin."""
        self.x = 0.0
        self.y = 0.0
        self.angle = 0.0
        self.speed = 0.0
        self.path_history = [(0.0, 0.0)]

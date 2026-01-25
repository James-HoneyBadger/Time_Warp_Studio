"""
Hardware abstraction layer for Time Warp Studio
Enables IoT integration with Arduino, Raspberry Pi, and sensors
"""

import json
import os
import threading
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Optional


class HardwareError(Exception):
    """Base exception for hardware-related errors"""


class HardwareConnectionError(HardwareError):
    """Raised when hardware connection fails"""


class DeviceNotFoundError(HardwareError):
    """Raised when specified device is not found"""


class Protocol(Enum):
    """Supported communication protocols"""

    SERIAL = "serial"
    I2C = "i2c"
    SPI = "spi"
    GPIO = "gpio"
    ANALOG = "analog"


class PinMode(Enum):
    """GPIO pin modes"""

    INPUT = "input"
    OUTPUT = "output"
    INPUT_PULLUP = "input_pullup"
    INPUT_PULLDOWN = "input_pulldown"


class PinState(Enum):
    """Digital pin states"""

    LOW = 0
    HIGH = 1


@dataclass
class DeviceInfo:
    """Information about a connected device"""

    id: str
    name: str
    type: str
    protocol: Protocol
    address: Optional[str] = None
    port: Optional[str] = None
    capabilities: Optional[List[str]] = None

    def __post_init__(self):
        if self.capabilities is None:
            self.capabilities = []


@dataclass
class PinConfig:
    """Configuration for a hardware pin"""

    number: int
    mode: PinMode
    name: Optional[str] = None
    description: Optional[str] = None


class HardwareDevice(ABC):
    """Abstract base class for hardware devices"""

    def __init__(self, device_info: DeviceInfo):
        self.device_info = device_info
        self.connected = False
        self._lock = threading.Lock()

    @abstractmethod
    def connect(self) -> bool:
        """Establish connection to the device"""
        raise NotImplementedError

    @abstractmethod
    def disconnect(self) -> bool:
        """Close connection to the device"""
        raise NotImplementedError

    @abstractmethod
    def is_connected(self) -> bool:
        """Check if device is currently connected"""
        raise NotImplementedError

    @abstractmethod
    def read_data(self) -> Dict[str, Any]:
        """Read current sensor/actuator data"""
        raise NotImplementedError

    @abstractmethod
    def write_data(self, data: Dict[str, Any]) -> bool:
        """Write data to actuators"""
        raise NotImplementedError

    def get_info(self) -> DeviceInfo:
        """Get device information"""
        return self.device_info

    def __enter__(self):
        self.connect()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.disconnect()


class SerialDevice(HardwareDevice):
    """Serial communication device (Arduino, etc.)"""

    def __init__(self, device_info: DeviceInfo, baud_rate: int = 9600):
        super().__init__(device_info)
        self.baud_rate = baud_rate
        self.serial_connection = None
        self._buffer = ""

    def connect(self) -> bool:
        try:
            # In simulation mode, just mark as connected
            simulation_env = os.environ.get("TIME_WARP_SIMULATION", "false")
            if simulation_env.lower() == "true":
                self.connected = True
                return True

            # Real hardware connection would go here
            # import serial
            # self.serial_connection = serial.Serial(
            #     self.device_info.port, self.baud_rate)
            self.connected = True
            return True
        except (ImportError, OSError, RuntimeError) as e:
            raise HardwareConnectionError(
                f"Failed to connect to serial device: {e}"
            ) from e

    def disconnect(self) -> bool:
        if self.serial_connection:
            try:
                self.serial_connection.close()
            except OSError:
                pass
        self.connected = False
        return True

    def is_connected(self) -> bool:
        return self.connected

    def read_data(self) -> Dict[str, Any]:
        """Read serial data"""
        if not self.connected:
            return {}

        # Simulation mode
        simulation_env = os.environ.get("TIME_WARP_SIMULATION", "false")
        if simulation_env.lower() == "true":
            return {
                "timestamp": time.time(),
                "data": f"simulated_data_{int(time.time())}",
                "protocol": "serial",
            }

        # Real hardware reading would go here
        return {}

    def write_data(self, data: Dict[str, Any]) -> bool:
        """Write data to serial device"""
        if not self.connected:
            return False

        try:
            if self.serial_connection:
                # Real hardware writing would go here
                pass
            return True
        except (OSError, RuntimeError):
            return False


class GPIODevice(HardwareDevice):
    """GPIO control device (Raspberry Pi, etc.)"""

    def __init__(self, device_info: DeviceInfo):
        super().__init__(device_info)
        self.pins: Dict[int, PinConfig] = {}
        self.pin_states: Dict[int, PinState] = {}
        self.gpio_interface = None

    def connect(self) -> bool:
        try:
            # Simulation mode
            simulation_env = os.environ.get("TIME_WARP_SIMULATION", "false")
            if simulation_env.lower() == "true":
                self.connected = True
                return True

            # Real GPIO setup would go here
            # import RPi.GPIO as GPIO
            # GPIO.setmode(GPIO.BCM)
            # self.gpio_interface = GPIO
            self.connected = True
            return True
        except (ImportError, OSError, RuntimeError) as e:
            msg = f"Failed to initialize GPIO: {e}"
            raise HardwareConnectionError(msg) from e

    def disconnect(self) -> bool:
        if self.gpio_interface:
            try:
                # GPIO.cleanup()
                pass
            except OSError:
                pass
        self.connected = False
        return True

    def is_connected(self) -> bool:
        return self.connected

    def configure_pin(self, pin_config: PinConfig) -> bool:
        """Configure a GPIO pin"""
        if not self.connected:
            return False

        try:
            self.pins[pin_config.number] = pin_config

            # Real GPIO configuration would go here
            # if self.gpio_interface:
            #     mode = (
            #         GPIO.IN
            #         if pin_config.mode == PinMode.INPUT
            #         else GPIO.OUT
            #     )
            #     self.gpio_interface.setup(pin_config.number, mode)

            return True
        except (OSError, RuntimeError):
            return False

    def set_pin_state(self, pin_number: int, state: PinState) -> bool:
        """Set digital pin state"""
        if not self.connected or pin_number not in self.pins:
            return False

        try:
            self.pin_states[pin_number] = state

            # Real GPIO writing would go here
            # if self.gpio_interface:
            #     self.gpio_interface.output(pin_number, state.value)

            return True
        except (OSError, ValueError):
            return False

    def get_pin_state(self, pin_number: int) -> Optional[PinState]:
        """Get digital pin state"""
        if not self.connected:
            return None

        # Simulation mode
        if os.environ.get("TIME_WARP_SIMULATION", "false").lower() == "true":
            if (int(time.time()) % 2) == 0:
                return PinState.HIGH
            return PinState.LOW

        # Real GPIO reading would go here
        return self.pin_states.get(pin_number)

    def read_data(self) -> Dict[str, Any]:
        """Read GPIO pin states"""
        if not self.connected:
            return {}

        pin_data = {}
        for pin_num in self.pins:
            state = self.get_pin_state(pin_num)
            pin_data[f"pin_{pin_num}"] = state.value if state else None

        return {"timestamp": time.time(), "pins": pin_data, "protocol": "gpio"}

    def write_data(self, data: Dict[str, Any]) -> bool:
        """Write GPIO data"""
        if not self.connected:
            return False

        try:
            for key, value in data.items():
                if key.startswith("pin_"):
                    pin_num = int(key[4:])  # Remove 'pin_' prefix
                    state = PinState(value)
                    if not self.set_pin_state(pin_num, state):
                        return False
            return True
        except (OSError, ValueError):
            return False


class HardwareManager:
    """Central manager for hardware devices"""

    def __init__(self):
        self.devices: Dict[str, HardwareDevice] = {}
        self.device_configs: Dict[str, Dict[str, Any]] = {}
        self._simulation_mode = (
            os.environ.get("TIME_WARP_SIMULATION", "false").lower() == "true"
        )
        self._lock = threading.Lock()

    def set_simulation_mode(self, enabled: bool):
        """Enable or disable simulation mode"""
        self._simulation_mode = enabled
        os.environ["TIME_WARP_SIMULATION"] = "true" if enabled else "false"

    def is_simulation_mode(self) -> bool:
        """Check if simulation mode is enabled"""
        return self._simulation_mode

    def discover_devices(self) -> List[DeviceInfo]:
        """Discover available hardware devices"""
        devices = []

        if self._simulation_mode:
            # Return simulated devices
            devices.extend(
                [
                    DeviceInfo(
                        id="sim_arduino_1",
                        name="Arduino Uno (Simulated)",
                        type="arduino",
                        protocol=Protocol.SERIAL,
                        port="/dev/ttyACM0",
                        capabilities=["analog", "digital", "serial"],
                    ),
                    DeviceInfo(
                        id="sim_rpi_1",
                        name="Raspberry Pi (Simulated)",
                        type="raspberry_pi",
                        protocol=Protocol.GPIO,
                        capabilities=["gpio", "i2c", "spi"],
                    ),
                ]
            )
        else:
            # Real device discovery would go here.
            # Check for Arduino devices, Raspberry Pi, I2C/SPI, etc.
            pass

        return devices

    def connect_device(
        self,
        device_info: DeviceInfo,
    ) -> Optional[HardwareDevice]:
        """Connect to a hardware device"""
        with self._lock:
            try:
                # Local variable typed to the abstract base type so that
                # assignments from different concrete subclasses are valid
                device: HardwareDevice
                if device_info.protocol == Protocol.SERIAL:
                    device = SerialDevice(device_info)
                elif device_info.protocol == Protocol.GPIO:
                    device = GPIODevice(device_info)
                else:
                    msg = f"Unsupported protocol: {device_info.protocol}"
                    raise ValueError(msg)

                if device.connect():
                    self.devices[device_info.id] = device
                    return device
                return None

            except (HardwareConnectionError, ValueError) as e:
                print(f"Failed to connect to device {device_info.id}: {e}")
                return None

    def disconnect_device(self, device_id: str) -> bool:
        """Disconnect a hardware device"""
        with self._lock:
            if device_id in self.devices:
                device = self.devices[device_id]
                success = device.disconnect()
                del self.devices[device_id]
                return success
            return False

    def get_device(self, device_id: str) -> Optional[HardwareDevice]:
        """Get a connected device by ID"""
        return self.devices.get(device_id)

    def get_connected_devices(self) -> List[HardwareDevice]:
        """Get all connected devices"""
        return list(self.devices.values())

    def read_all_devices(self) -> Dict[str, Dict[str, Any]]:
        """Read data from all connected devices"""
        results = {}
        for device_id, device in self.devices.items():
            try:
                data = device.read_data()
                if data:
                    results[device_id] = data
            except (OSError, RuntimeError) as e:
                print(f"Error reading from device {device_id}: {e}")
        return results

    def save_device_config(self, device_id: str, config: Dict[str, Any]):
        """Save device configuration"""
        self.device_configs[device_id] = config

    def load_device_config(self, device_id: str) -> Optional[Dict[str, Any]]:
        """Load device configuration"""
        return self.device_configs.get(device_id)

    def export_config(self, filename: str):
        """Export all device configurations to file"""
        config = {
            "simulation_mode": self._simulation_mode,
            "devices": self.device_configs,
        }
        with open(filename, "w", encoding="utf-8") as f:
            json.dump(config, f, indent=2)

    def import_config(self, filename: str):
        """Import device configurations from file"""
        try:
            with open(filename, "r", encoding="utf-8") as f:
                config = json.load(f)
                self._simulation_mode = config.get("simulation_mode", False)
                self.device_configs = config.get("devices", {})
        except (OSError, json.JSONDecodeError) as e:
            # Preserve original exception context when re-raising
            raise HardwareError(f"Failed to import config: {e}") from e


# Global hardware manager instance
# This intentionally uses a private module-level variable name; pylint flags
# C0103 for constant-style names here so disable that check for this symbol.
# pylint: disable=invalid-name
_hardware_manager = None


def get_hardware_manager() -> HardwareManager:
    """Get the global hardware manager instance.

    Use the module globals dictionary instead of the ``global`` keyword so
    static analyzers do not warn about the global statement.
    """
    if (
        "_hardware_manager" not in globals()
        or globals().get("_hardware_manager") is None
    ):
        globals()["_hardware_manager"] = HardwareManager()
    return globals()["_hardware_manager"]

"""
Arduino integration for Time Warp Studio
Provides Firmata protocol support and sensor/actuator control
"""

import os
import time
from typing import Any, Dict, Optional, Union

from .. import (
    DeviceInfo,
    HardwareDevice,
    HardwareError,
    PinMode,
    PinState,
    Protocol,
)


class ArduinoError(HardwareError):
    """Arduino-specific hardware errors"""

    # No body required; the docstring denotes the specialized error class.


class ArduinoDevice(HardwareDevice):
    """Arduino device with Firmata protocol support"""

    # Standard Arduino pin capabilities
    ANALOG_PINS = list(range(16))  # A0-A15
    DIGITAL_PINS = list(range(14))  # D0-D13
    PWM_PINS = [3, 5, 6, 9, 10, 11]  # PWM-capable pins

    def __init__(self, device_info: DeviceInfo, baud_rate: int = 57600):
        super().__init__(device_info)
        self.baud_rate = baud_rate
        self.firmata = None
        self.pin_modes: Dict[int, PinMode] = {}
        self.pin_states: Dict[int, Union[int, float]] = {}
        self.analog_values: Dict[int, int] = {}
        self.digital_values: Dict[int, bool] = {}

    def connect(self) -> bool:
        try:
            # Simulation mode
            if self._is_simulation_mode():
                self.connected = True
                self._initialize_simulated_pins()
                return True

            # Real Arduino connection would use pyfirmata
            try:
                # from pyfirmata import Arduino, util
                # self.firmata = Arduino(
                #     self.device_info.port,
                #     baudrate=self.baud_rate,
                # )
                # # Start iterator for analog reads
                # iterator = util.Iterator(self.firmata)
                # iterator.start()
                self.connected = True
                return True
            except ImportError as exc:
                raise ArduinoError(
                    "pyfirmata not installed; run: pip install pyfirmata",
                ) from exc

        except (OSError, RuntimeError) as e:
            raise ArduinoError(f"Failed to connect to Arduino: {e}") from e

    def disconnect(self) -> bool:
        if self.firmata:
            try:
                # self.firmata.exit()
                pass
            except OSError:
                pass
        self.connected = False
        return True

    def is_connected(self) -> bool:
        return self.connected

    def _is_simulation_mode(self) -> bool:
        """Check if running in simulation mode"""

        env = os.environ.get("TIME_WARP_SIMULATION", "false")
        return env.lower() == "true"

    def _initialize_simulated_pins(self):
        """Initialize pins for simulation mode"""
        # Set up digital pins
        for pin in self.DIGITAL_PINS:
            self.pin_modes[pin] = PinMode.OUTPUT
            self.digital_values[pin] = False

        # Set up analog pins
        for pin in self.ANALOG_PINS:
            self.analog_values[pin] = 0

    def set_pin_mode(self, pin_number: int, mode: PinMode) -> bool:
        """Set the mode of a pin"""
        if not self.connected:
            return False

        if pin_number not in self.DIGITAL_PINS and pin_number not in self.ANALOG_PINS:
            return False

        try:
            self.pin_modes[pin_number] = mode

            if not self._is_simulation_mode() and self.firmata:
                # Real hardware pin mode setting
                # firmata_mode = self._convert_pin_mode(mode)
                # self.firmata.digital[pin_number].mode = firmata_mode
                pass

            return True
        except (OSError, RuntimeError):
            return False

    def _convert_pin_mode(self, mode: PinMode):
        """Convert our PinMode to pyfirmata mode"""
        # This would convert PinMode enum to pyfirmata constants
        # pyfirmata.INPUT, pyfirmata.OUTPUT, etc.
        return mode.value

    def digital_write(self, pin_number: int, state: PinState) -> bool:
        """Write digital value to pin"""
        if not self.connected or pin_number not in self.DIGITAL_PINS:
            return False

        try:
            self.digital_values[pin_number] = state == PinState.HIGH

            if not self._is_simulation_mode() and self.firmata:
                # self.firmata.digital[pin_number].write(state.value)
                pass

            return True
        except (OSError, ValueError):
            return False

    def digital_read(self, pin_number: int) -> Optional[bool]:
        """Read digital value from pin"""
        if not self.connected or pin_number not in self.DIGITAL_PINS:
            return None

        if self._is_simulation_mode():
            # Simulate changing digital values
            return (int(time.time() * 1000) % 2000) > 1000

        if self.firmata:
            try:
                # return self.firmata.digital[pin_number].read()
                return self.digital_values.get(pin_number, False)
            except (OSError, ValueError):
                return None

        return None

    def analog_write(self, pin_number: int, value: int) -> bool:
        """Write analog value (PWM) to pin"""
        if not self.connected or pin_number not in self.PWM_PINS:
            return False

        if not 0 <= value <= 255:
            return False

        try:
            self.pin_states[pin_number] = value

            if not self._is_simulation_mode() and self.firmata:
                # self.firmata.digital[pin_number].write(value / 255.0)
                pass

            return True
        except (OSError, ValueError):
            return False

    def analog_read(self, pin_number: int) -> Optional[int]:
        """Read analog value from pin"""
        if not self.connected or pin_number not in self.ANALOG_PINS:
            return None

        if self._is_simulation_mode():
            # Simulate analog sensor values
            return int((time.time() * 100) % 1024)

        if self.firmata:
            try:
                # return int(self.firmata.analog[pin_number].read() * 1023)
                return self.analog_values.get(pin_number, 0)
            except (OSError, ValueError):
                return None

        return None

    def read_temperature_sensor(self, pin_number: int) -> Optional[float]:
        """Read temperature from analog pin (assuming TMP117 or similar)"""
        raw_value = self.analog_read(pin_number)
        if raw_value is None:
            return None

        # Convert ADC value to temperature (example conversion)
        # This would depend on the specific sensor
        voltage = (raw_value / 1023.0) * 5.0  # 5V reference
        temperature = (voltage - 0.5) * 100.0  # LM35 formula
        return temperature

    def read_light_sensor(self, pin_number: int) -> Optional[float]:
        """Read light level from analog pin (assuming LDR)"""
        raw_value = self.analog_read(pin_number)
        if raw_value is None:
            return None

        # Convert to percentage (0-100)
        return (raw_value / 1023.0) * 100.0

    def control_servo(self, pin_number: int, angle: int) -> bool:
        """Control servo motor (0-180 degrees)"""
        if not 0 <= angle <= 180:
            return False

        # Convert angle to PWM value (typically 1000-2000us pulse width)
        pwm_value = int(255 * (angle / 180.0))
        return self.analog_write(pin_number, pwm_value)

    def blink_led(
        self,
        pin_number: int,
        duration: float = 1.0,
        times: int = 5,
    ):
        """Blink an LED for testing"""
        if not self.connected or pin_number not in self.DIGITAL_PINS:
            return

        for _ in range(times):
            self.digital_write(pin_number, PinState.HIGH)
            time.sleep(duration / 2)
            self.digital_write(pin_number, PinState.LOW)
            time.sleep(duration / 2)

    def read_data(self) -> Dict[str, Any]:
        """Read all sensor data"""
        if not self.connected:
            return {}

        # Type nested maps explicitly so mypy knows the expected value types
        digital_pins: Dict[str, Optional[bool]] = {}
        analog_pins: Dict[str, Optional[int]] = {}
        sensors: Dict[str, Any] = {}

        data: Dict[str, Any] = {
            "timestamp": time.time(),
            "protocol": "arduino",
            "digital_pins": digital_pins,
            "analog_pins": analog_pins,
            "sensors": sensors,
        }

        # Read digital pins
        for pin in self.DIGITAL_PINS:
            value = self.digital_read(pin)
            digital_pins[f"D{pin}"] = value

        # Read analog pins
        for pin in self.ANALOG_PINS:
            analog_value = self.analog_read(pin)
            analog_pins[f"A{pin}"] = analog_value

            # Add sensor interpretations
            if pin == 0:  # Assume temperature sensor on A0
                temp = self.read_temperature_sensor(pin)
                if temp is not None:
                    sensors["temperature_c"] = temp

                if pin == 1:  # Assume light sensor on A1
                    light = self.read_light_sensor(pin)
                    if light is not None:
                        sensors["light_percent"] = light

        return data

    def write_data(self, data: Dict[str, Any]) -> bool:
        """Write actuator data"""
        if not self.connected:
            return False

        try:
            for key, value in data.items():
                if key.startswith("D"):  # Digital pin
                    pin_num = int(key[1:])
                    state = PinState(value)
                    if not self.digital_write(pin_num, state):
                        return False

                elif key.startswith("A"):  # Analog/PWM pin
                    pin_num = int(key[1:])
                    if not self.analog_write(pin_num, int(value)):
                        return False

                elif key == "servo_angle":
                    # Assume servo on pin 9
                    if not self.control_servo(9, int(value)):
                        return False

            return True
        except (OSError, ValueError):
            return False

    def get_pin_info(self) -> Dict[str, Any]:
        """Get information about all pins"""
        return {
            "digital_pins": self.DIGITAL_PINS,
            "analog_pins": self.ANALOG_PINS,
            "pwm_pins": self.PWM_PINS,
            "pin_modes": self.pin_modes.copy(),
            "pin_states": self.pin_states.copy(),
        }


def create_arduino_device(port: str, baud_rate: int = 57600) -> ArduinoDevice:
    """Create an Arduino device instance"""
    device_info = DeviceInfo(
        id=f"arduino_{port.replace('/', '_')}",
        name=f"Arduino on {port}",
        type="arduino",
        protocol=Protocol.SERIAL,
        port=port,
        capabilities=["analog", "digital", "pwm", "servo", "sensors"],
    )
    return ArduinoDevice(device_info, baud_rate)

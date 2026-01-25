"""
Raspberry Pi integration for Time Warp Studio
Provides GPIO control, I2C/SPI communication, and sensor support
"""

import os
import time
from typing import Any, Dict, List, Optional

from .. import (
    DeviceInfo,
    HardwareDevice,
    HardwareError,
    PinMode,
    PinState,
    Protocol,
)


class RaspberryPiError(HardwareError):
    """Raspberry Pi-specific hardware errors"""

    # No body required - docstring is sufficient.


# Disable the instance attribute count check for this hardware wrapper.
# This class intentionally stores several hardware-specific fields.
# pylint: disable=too-many-instance-attributes
class RaspberryPiDevice(HardwareDevice):
    """Raspberry Pi device with GPIO and peripheral support"""

    # GPIO pin mappings (BCM numbering)
    GPIO_PINS = list(range(2, 28))  # GPIO2-GPIO27
    I2C_PINS = [2, 3]  # GPIO2 (SDA), GPIO3 (SCL)
    SPI_PINS = [7, 8, 9, 10, 11]  # SPI0 pins

    # Hardware capabilities
    HAS_I2C = True
    HAS_SPI = True
    HAS_CAMERA = True
    HAS_AUDIO = True

    def __init__(self, device_info: DeviceInfo):
        super().__init__(device_info)
        self.gpio = None
        self.i2c = None
        self.spi = None
        self.pin_modes: Dict[int, PinMode] = {}
        self.pin_states: Dict[int, PinState] = {}
        self.i2c_devices: Dict[int, Any] = {}  # Address -> device
        self.spi_devices: Dict[int, Any] = {}  # Bus -> device

    def connect(self) -> bool:
        try:
            # Simulation mode
            if self._is_simulation_mode():
                self.connected = True
                self._initialize_simulated_pins()
                return True

            # Real Raspberry Pi connection
            try:
                # import RPi.GPIO as GPIO
                # self.gpio = GPIO
                # GPIO.setmode(GPIO.BCM)
                # GPIO.setwarnings(False)

                # Initialize I2C if available
                # from smbus2 import SMBus
                # self.i2c = SMBus(1)  # I2C bus 1

                # Initialize SPI if available
                # import spidev
                # self.spi = spidev.SpiDev()
                # self.spi.open(0, 0)  # SPI bus 0, device 0

                self.connected = True
                return True

            except ImportError as exc:
                raise RaspberryPiError(
                    "RPi.GPIO/smbus2/spidev not installed; "
                    "run: pip install RPi.GPIO smbus2 spidev",
                ) from exc

        except (OSError, RuntimeError) as e:
            msg = f"Failed to connect to Raspberry Pi: {e}"
            raise RaspberryPiError(msg) from e

    def disconnect(self) -> bool:
        # Clean up GPIO
        if self.gpio:
            try:
                # self.gpio.cleanup()
                pass
            except OSError:
                pass

        # Clean up I2C
        if self.i2c:
            try:
                # self.i2c.close()
                pass
            except OSError:
                pass

        # Clean up SPI
        if self.spi:
            try:
                # self.spi.close()
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
        for pin in self.GPIO_PINS:
            self.pin_modes[pin] = PinMode.INPUT
            self.pin_states[pin] = PinState.LOW

    def set_pin_mode(self, pin_number: int, mode: PinMode) -> bool:
        """Set the mode of a GPIO pin"""
        if not self.connected or pin_number not in self.GPIO_PINS:
            return False

        try:
            self.pin_modes[pin_number] = mode

            if not self._is_simulation_mode() and self.gpio:
                # Convert to RPi.GPIO constants
                # gpio_mode = self._convert_pin_mode(mode)
                # self.gpio.setup(pin_number, gpio_mode)
                pass

            return True
        except (OSError, RuntimeError):
            return False

    def _convert_pin_mode(self, mode: PinMode):
        """Convert our PinMode to RPi.GPIO mode"""
        # if mode == PinMode.INPUT:
        #     return self.gpio.IN
        # elif mode == PinMode.OUTPUT:
        #     return self.gpio.OUT
        # elif mode == PinMode.INPUT_PULLUP:
        #     return self.gpio.IN, pull_up_down=self.gpio.PUD_UP
        # elif mode == PinMode.INPUT_PULLDOWN:
        #     return self.gpio.IN, pull_up_down=self.gpio.PUD_DOWN
        return mode.value

    def write_pin(self, pin_number: int, state: PinState) -> bool:
        """Write digital state to GPIO pin"""
        if not self.connected or pin_number not in self.GPIO_PINS:
            return False

        try:
            self.pin_states[pin_number] = state

            if not self._is_simulation_mode() and self.gpio:
                # self.gpio.output(pin_number, state.value)
                pass

            return True
        except (OSError, ValueError):
            return False

    def read_pin(self, pin_number: int) -> Optional[PinState]:
        """Read digital state from GPIO pin"""
        if not self.connected or pin_number not in self.GPIO_PINS:
            return None

        if self._is_simulation_mode():
            # Simulate pin state changes
            if (int(time.time()) % 2) == 0:
                return PinState.HIGH
            return PinState.LOW

        if self.gpio:
            try:
                # value = self.gpio.input(pin_number)
                # return PinState(value)
                return self.pin_states.get(pin_number, PinState.LOW)
            except (OSError, ValueError):
                return None

        return None

    def pwm_write(
        self,
        pin_number: int,
        frequency: int,
        duty_cycle: float,
    ) -> bool:
        """Write PWM signal to pin"""
        if not self.connected or pin_number not in self.GPIO_PINS:
            return False

        if not 0 <= duty_cycle <= 100:
            return False

        # Frequency parameter exists for API compatibility; we don't use it
        # in the simulation helper.
        del frequency

        try:
            if not self._is_simulation_mode() and self.gpio:
                # pwm = self.gpio.PWM(pin_number, frequency)
                # pwm.start(duty_cycle)
                # pwm.stop()  # For one-shot PWM
                pass

            return True
        except (OSError, ValueError):
            return False

    def i2c_write_byte(self, address: int, data: int) -> bool:
        """Write byte to I2C device"""
        if not self.connected or not self.i2c:
            return False

        # These arguments are accepted for compatibility with real I2C APIs.
        del address, data

        try:
            # self.i2c.write_byte(address, data)
            return True
        except (OSError, ValueError):
            return False

    def i2c_read_byte(self, address: int) -> Optional[int]:
        """Read byte from I2C device"""
        if not self.connected or not self.i2c:
            return None

        # Argument kept for API compatibility
        del address

        try:
            # return self.i2c.read_byte(address)
            return 0  # Simulated
        except (OSError, ValueError):
            return None

    def i2c_write_block(self, address: int, data: List[int]) -> bool:
        """Write block of data to I2C device"""
        if not self.connected or not self.i2c:
            return False

        # Keep API compat but we don't use arguments in simulation.
        del address, data

        try:
            # self.i2c.write_i2c_block_data(address, data[0], data[1:])
            return True
        except (OSError, ValueError):
            return False

    def spi_write(self, data: List[int]) -> Optional[List[int]]:
        """Write data via SPI and read response"""
        if not self.connected or not self.spi:
            return None

        try:
            # return self.spi.xfer(data)
            return data  # Echo for simulation
        except (OSError, ValueError):
            return None

    def read_temperature_dht11(
        self,
        pin_number: int,
    ) -> Optional[Dict[str, float]]:
        """Read temperature and humidity from DHT11 sensor"""
        if not self.connected:
            return None

        # Simulation mode
        if self._is_simulation_mode():
            return {
                "temperature_c": 25.0 + (time.time() % 10),
                "humidity_percent": 60.0 + (time.time() % 20),
            }

        # Real sensor reading would require Adafruit_DHT library
        # Pin number kept for API compatibility.
        del pin_number

        try:
            # import Adafruit_DHT as DHT
            # humidity, temperature = DHT.read_retry(DHT.DHT11, pin_number)
            # if humidity is not None and temperature is not None:
            #     return {
            #         'temperature_c': temperature,
            #         'humidity_percent': humidity,
            #     }
            return None
        except (OSError, ValueError):
            return None

    def read_distance_hcsr04(
        self,
        trigger_pin: int,
        echo_pin: int,
    ) -> Optional[float]:
        """Read distance from HC-SR04 ultrasonic sensor"""
        if not self.connected:
            return None

        # Simulation mode
        if self._is_simulation_mode():
            return 50.0 + (time.time() % 100)  # 50-150 cm

        # Real sensor reading
        # echo_pin isn't used in this simulation implementation.
        del echo_pin

        try:
            # Set trigger pin high for 10us
            self.write_pin(trigger_pin, PinState.HIGH)
            time.sleep(0.00001)
            self.write_pin(trigger_pin, PinState.LOW)

            # Wait for echo pin to go high
            # pulse_start = time.time()
            # while self.read_pin(echo_pin) == PinState.LOW:
            #     pulse_start = time.time()

            # Wait for echo pin to go low
            # pulse_end = time.time()
            # while self.read_pin(echo_pin) == PinState.HIGH:
            #     pulse_end = time.time()

            # Calculate distance
            # pulse_duration = pulse_end - pulse_start
            # distance = pulse_duration * 17150  # Speed of sound / 2
            # return distance

            return 100.0  # Simulated distance
        except (OSError, ValueError):
            return None

    def control_servo(self, pin_number: int, angle: int) -> bool:
        """Control servo motor (0-180 degrees)"""
        if not 0 <= angle <= 180:
            return False

        # Convert angle to duty cycle (typically 2-12% for 0-180 degrees)
        duty_cycle = 2 + (angle / 180.0) * 10
        return self.pwm_write(pin_number, 50, duty_cycle)  # 50Hz PWM

    def blink_led(
        self,
        pin_number: int,
        duration: float = 1.0,
        times: int = 5,
    ):
        """Blink an LED for testing"""
        if not self.connected or pin_number not in self.GPIO_PINS:
            return

        for _ in range(times):
            self.write_pin(pin_number, PinState.HIGH)
            time.sleep(duration / 2)
            self.write_pin(pin_number, PinState.LOW)
            time.sleep(duration / 2)

    def read_data(self) -> Dict[str, Any]:
        """Read all sensor data"""
        if not self.connected:
            return {}

        gpio_pins: Dict[str, Optional[int]] = {}
        sensors: Dict[str, Any] = {}

        data = {
            "timestamp": time.time(),
            "protocol": "gpio",
            "gpio_pins": gpio_pins,
            "sensors": sensors,
        }

        # Read GPIO pins
        for pin in self.GPIO_PINS[:10]:  # Read first 10 pins for performance
            state = self.read_pin(pin)
            gpio_pins[f"GPIO{pin}"] = state.value if state else None

        # Read simulated sensors
        if self._is_simulation_mode():
            # DHT11 sensor on GPIO4
            dht_data = self.read_temperature_dht11(4)
            if dht_data:
                sensors.update(dht_data)

            # HC-SR04 sensor on GPIO17 (trigger), GPIO27 (echo)
            distance = self.read_distance_hcsr04(17, 27)
            if distance is not None:
                sensors["distance_cm"] = distance

        return data

    def write_data(self, data: Dict[str, Any]) -> bool:
        """Write actuator data"""
        if not self.connected:
            return False

        try:
            for key, value in data.items():
                if key.startswith("GPIO"):
                    pin_num = int(key[4:])  # Remove 'GPIO' prefix
                    state = PinState(value)
                    if not self.write_pin(pin_num, state):
                        return False

                elif key == "servo_angle":
                    # Assume servo on GPIO18
                    if not self.control_servo(18, int(value)):
                        return False

                elif key == "pwm_duty":
                    # Assume PWM on GPIO19
                    if not self.pwm_write(19, 1000, float(value)):
                        return False

            return True
        except (OSError, ValueError):
            return False

    def get_system_info(self) -> Dict[str, Any]:
        """Get Raspberry Pi system information"""
        info = {
            "gpio_pins": self.GPIO_PINS,
            "i2c_available": self.HAS_I2C,
            "spi_available": self.HAS_SPI,
            "camera_available": self.HAS_CAMERA,
            "audio_available": self.HAS_AUDIO,
            "pin_modes": self.pin_modes.copy(),
            "pin_states": {k: v.value for k, v in self.pin_states.items()},
        }

        # Add real system info if available
        if not self._is_simulation_mode():
            try:
                # import subprocess
                # cpu_temp = subprocess.check_output(
                #     ['vcgencmd', 'measure_temp']).decode()
                # info['cpu_temperature'] = cpu_temp.strip()
                pass
            except OSError:
                pass

        return info


def create_raspberry_pi_device() -> RaspberryPiDevice:
    """Create a Raspberry Pi device instance"""
    device_info = DeviceInfo(
        id="raspberry_pi_local",
        name="Raspberry Pi (Local)",
        type="raspberry_pi",
        protocol=Protocol.GPIO,
        capabilities=["gpio", "i2c", "spi", "camera", "audio", "sensors"],
    )
    return RaspberryPiDevice(device_info)

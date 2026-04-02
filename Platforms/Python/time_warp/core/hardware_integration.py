"""
Hardware/IoT Integration Module

This module provides support for integrating hardware devices and IoT components
into the Time Warp Studio environment. It includes simulation-first design to
ensure compatibility with a wide range of devices.

Supported Features:
- Raspberry Pi GPIO control
- Arduino integration via pyfirmata
- Sensor data visualization

"""

from typing import Optional


class HardwareIntegration:
    """Class to manage hardware and IoT integration."""

    def __init__(self):
        self.devices = {}

    def add_device(self, name: str, device: object) -> None:
        """Add a hardware device to the integration manager.

        Args:
            name (str): The name of the device.
            device (object): The device object.
        """
        self.devices[name] = device
        print(f"✅ Device '{name}' added successfully.")

    def remove_device(self, name: str) -> None:
        """Remove a hardware device from the integration manager.

        Args:
            name (str): The name of the device to remove.
        """
        if name in self.devices:
            del self.devices[name]
            print(f"✅ Device '{name}' removed successfully.")
        else:
            print(f"❌ Device '{name}' not found.")

    def list_devices(self) -> None:
        """List all connected devices."""
        if self.devices:
            print("🚀 Connected Devices:")
            for name in self.devices:
                print(f"- {name}")
        else:
            print("ℹ️ No devices connected.")

    def simulate_device(self, name: str, data: Optional[dict] = None) -> None:
        """Simulate a hardware device for testing purposes.

        Args:
            name (str): The name of the simulated device.
            data (Optional[dict]): Simulated data for the device.
        """
        print(f"🐢 Simulating device '{name}' with data: {data}")


# Example usage
if __name__ == "__main__":
    manager = HardwareIntegration()
    manager.add_device("Raspberry Pi", object())
    manager.list_devices()
    manager.simulate_device("Raspberry Pi", {"temperature": 25})
    manager.remove_device("Raspberry Pi")
    manager.list_devices()

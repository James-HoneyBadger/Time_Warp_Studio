"""Hardware control panel UI for Time Warp IDE
Provides real-time sensor visualization and actuator control
"""

# PySide6 widgets are dynamically exported; static analysis tools often
# report false positives like `no-name-in-module`. Explicitly disable
# that warning for this UI module.
# pylint: disable=no-name-in-module

import time
from typing import Any, Dict

from PySide6.QtCore import Qt, QTimer, Signal
from PySide6.QtGui import QFont
from PySide6.QtWidgets import (
    QCheckBox,
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QScrollArea,
    QSlider,
    QVBoxLayout,
    QWidget,
)

from .. import HardwareDevice, get_hardware_manager
from .data_logger import get_data_logger


class SensorWidget(QGroupBox):  # pylint: disable=too-few-public-methods
    """Widget for displaying sensor data"""

    def __init__(self, sensor_name: str, sensor_type: str, parent=None):
        super().__init__(parent)
        self.sensor_name = sensor_name
        self.sensor_type = sensor_type
        self.value_label = QLabel("0.0")
        self.unit_label = QLabel("")
        self.timestamp_label = QLabel("Never")

        self._setup_ui()
        self._set_units()

    def _setup_ui(self):
        """Setup the sensor widget UI"""
        layout = QVBoxLayout()

        # Title
        title_label = QLabel(self.sensor_name)
        title_label.setFont(QFont("Arial", 12, QFont.Bold))
        layout.addWidget(title_label)

        # Value display
        value_layout = QHBoxLayout()
        self.value_label.setFont(QFont("Arial", 16, QFont.Bold))
        self.value_label.setAlignment(Qt.AlignCenter)
        value_layout.addWidget(self.value_label)

        self.unit_label.setFont(QFont("Arial", 10))
        value_layout.addWidget(self.unit_label)
        layout.addLayout(value_layout)

        # Timestamp
        self.timestamp_label.setFont(QFont("Arial", 8))
        self.timestamp_label.setStyleSheet("color: gray;")
        layout.addWidget(self.timestamp_label)

        self.setLayout(layout)
        self.setMinimumSize(150, 100)
        self.setMaximumSize(200, 120)

    def _set_units(self):
        """Set appropriate units based on sensor type"""
        unit_map = {
            "temperature": "°C",
            "humidity": "%",
            "distance": "cm",
            "light": "%",
            "pressure": "hPa",
            "voltage": "V",
            "current": "mA",
        }
        self.unit_label.setText(unit_map.get(self.sensor_type, ""))

    def update_value(self, value: float, timestamp: float):
        """Update the sensor value display"""
        self.value_label.setText(f"{value:.1f}")

        # Color coding based on value ranges
        if self.sensor_type == "temperature":
            if value < 0:
                color = "blue"
            elif value > 30:
                color = "red"
            else:
                color = "green"
        elif self.sensor_type == "humidity":
            if value < 30 or value > 70:
                color = "orange"
            else:
                color = "green"
        else:
            color = "black"

        self.value_label.setStyleSheet(f"color: {color};")

        # Update timestamp
        time_str = time.strftime("%H:%M:%S", time.localtime(timestamp))
        self.timestamp_label.setText(f"Updated: {time_str}")


class ActuatorWidget(QGroupBox):  # pylint: disable=too-few-public-methods
    """Widget for controlling actuators"""

    value_changed = Signal(str, object)  # actuator_name, value

    def __init__(self, actuator_name: str, actuator_type: str, parent=None):
        super().__init__(parent)
        self.actuator_name = actuator_name
        self.actuator_type = actuator_type

        self._setup_ui()

    def _setup_ui(self):
        """Setup the actuator widget UI"""
        layout = QVBoxLayout()

        # Title
        title_label = QLabel(self.actuator_name)
        title_label.setFont(QFont("Arial", 12, QFont.Bold))
        layout.addWidget(title_label)

        if self.actuator_type == "digital":
            # Digital switch (LED, relay, etc.)
            self.control = QCheckBox("ON/OFF")
            self.control.stateChanged.connect(self._on_digital_changed)
            layout.addWidget(self.control)

        elif self.actuator_type == "pwm":
            # PWM control (servo, motor speed, etc.)
            self.control = QSlider(Qt.Horizontal)
            self.control.setRange(0, 255)
            self.control.setValue(0)
            self.control.valueChanged.connect(self._on_pwm_changed)

            value_label = QLabel("0")

            def _update_value_label(v, lbl=value_label):
                lbl.setText(str(v))

            self.control.valueChanged.connect(_update_value_label)

            layout.addWidget(self.control)
            layout.addWidget(value_label)

        elif self.actuator_type == "servo":
            # Servo control (0-180 degrees)
            self.control = QSlider(Qt.Horizontal)
            self.control.setRange(0, 180)
            self.control.setValue(90)
            self.control.valueChanged.connect(self._on_servo_changed)

            value_label = QLabel("90°")

            def _update_degree_label(v, lbl=value_label):
                lbl.setText(f"{v}°")

            self.control.valueChanged.connect(_update_degree_label)

            layout.addWidget(self.control)
            layout.addWidget(value_label)

        self.setLayout(layout)
        self.setMinimumSize(150, 100)
        self.setMaximumSize(200, 120)

    def _on_digital_changed(self, state):
        """Handle digital control changes"""
        value = state == Qt.Checked
        self.value_changed.emit(self.actuator_name, value)

    def _on_pwm_changed(self, value):
        """Handle PWM control changes"""
        self.value_changed.emit(self.actuator_name, value)

    def _on_servo_changed(self, value):
        """Handle servo control changes"""
        self.value_changed.emit(self.actuator_name, value)


class DeviceWidget(QGroupBox):
    """Widget representing a hardware device"""

    def __init__(self, device: HardwareDevice, parent=None):
        super().__init__(parent)
        self.device = device
        self.sensors: Dict[str, SensorWidget] = {}
        self.actuators: Dict[str, ActuatorWidget] = {}

        self._setup_ui()
        self._create_sensor_widgets()
        self._create_actuator_widgets()

    def _setup_ui(self):
        """Setup the device widget UI"""
        self.setTitle(f"{self.device.device_info.name}")
        self.setFont(QFont("Arial", 10, QFont.Bold))

        self.layout = QVBoxLayout()

        # Connection status
        self.status_label = QLabel("Disconnected")
        self.status_label.setStyleSheet("color: red;")
        self.layout.addWidget(self.status_label)

        # Sensors section
        if self.sensors:
            sensors_group = QGroupBox("Sensors")
            sensors_layout = QHBoxLayout()

            for sensor in self.sensors.values():
                sensors_layout.addWidget(sensor)

            sensors_group.setLayout(sensors_layout)
            self.layout.addWidget(sensors_group)

        # Actuators section
        if self.actuators:
            actuators_group = QGroupBox("Actuators")
            actuators_layout = QHBoxLayout()

            for actuator in self.actuators.values():
                actuators_layout.addWidget(actuator)
                actuator.value_changed.connect(self._on_actuator_changed)

            actuators_group.setLayout(actuators_layout)
            self.layout.addWidget(actuators_group)

        self.setLayout(self.layout)

    def _create_sensor_widgets(self):
        """Create sensor widgets based on device capabilities"""
        capabilities = self.device.device_info.capabilities

        if "sensors" in capabilities or "temperature" in capabilities:
            temp_sensor = SensorWidget("Temperature", "temperature")
            self.sensors["temperature"] = temp_sensor

        if "sensors" in capabilities or "humidity" in capabilities:
            humid_sensor = SensorWidget("Humidity", "humidity")
            self.sensors["humidity"] = humid_sensor

        if "sensors" in capabilities or "distance" in capabilities:
            dist_sensor = SensorWidget("Distance", "distance")
            self.sensors["distance"] = dist_sensor

        if "sensors" in capabilities or "light" in capabilities:
            light_sensor = SensorWidget("Light", "light")
            self.sensors["light"] = light_sensor

    def _create_actuator_widgets(self):
        """Create actuator widgets based on device capabilities"""
        capabilities = self.device.device_info.capabilities

        if "gpio" in capabilities or "digital" in capabilities:
            led_actuator = ActuatorWidget("LED", "digital")
            self.actuators["led"] = led_actuator

        if "pwm" in capabilities:
            pwm_actuator = ActuatorWidget("PWM Output", "pwm")
            self.actuators["pwm"] = pwm_actuator

        if "servo" in capabilities:
            servo_actuator = ActuatorWidget("Servo", "servo")
            self.actuators["servo"] = servo_actuator

    def _on_actuator_changed(self, actuator_name: str, value):
        """Handle actuator value changes"""
        # Convert actuator names to device-specific commands
        data = {}
        if actuator_name == "led":
            data["D13"] = value  # Assume LED on pin 13
        elif actuator_name == "pwm":
            data["pwm_duty"] = value
        elif actuator_name == "servo":
            data["servo_angle"] = value

        if data:
            try:
                self.device.write_data(data)
            except Exception as e:  # pylint: disable=broad-except
                device_id = self.device.device_info.id
                msg = f"Error writing to device {device_id}: {e}"
                print(msg)

    def update_status(self, connected: bool):
        """Update connection status display"""
        if connected:
            self.status_label.setText("Connected")
            self.status_label.setStyleSheet("color: green;")
        else:
            self.status_label.setText("Disconnected")
            self.status_label.setStyleSheet("color: red;")

    def update_sensor_data(self, data: Dict[str, Any]):
        """Update sensor displays with new data"""
        sensors_data = data.get("sensors", {})

        for sensor_name, sensor_widget in self.sensors.items():
            if sensor_name in sensors_data:
                value = sensors_data[sensor_name]
                timestamp = data.get("timestamp", time.time())
                sensor_widget.update_value(value, timestamp)


class HardwareControlPanel(QWidget):
    """Main hardware control panel widget

    This widget aggregates many sub-widgets and maintains runtime state (timers
    and device lists) so intentionally has several attributes and public
    methods for UI behaviour.
    """

    # Large UI container — allow several instance attributes and public
    # methods for event handlers and lifecycle management.
    # pylint: disable=too-many-instance-attributes,too-few-public-methods

    def __init__(self, parent=None):
        super().__init__(parent)
        self.hardware_manager = get_hardware_manager()
        self.devices: Dict[str, DeviceWidget] = {}
        self.update_timer = QTimer()
        self.data_logger = get_data_logger()

        self._setup_ui()
        self._connect_signals()
        self._start_monitoring()

    def _setup_ui(self):
        """Setup the main UI"""
        layout = QVBoxLayout()

        # Header
        header_layout = QHBoxLayout()

        title = QLabel("Hardware Control Panel")
        title.setFont(QFont("Arial", 16, QFont.Bold))
        header_layout.addWidget(title)

        header_layout.addStretch()

        # Simulation mode toggle
        self.sim_mode_checkbox = QCheckBox("Simulation Mode")
        checked = self.hardware_manager.is_simulation_mode()
        self.sim_mode_checkbox.setChecked(checked)
        self.sim_mode_checkbox.stateChanged.connect(self._on_sim_mode_changed)
        header_layout.addWidget(self.sim_mode_checkbox)

        # Export buttons
        export_csv_btn = QPushButton("Export CSV")
        export_csv_btn.clicked.connect(self._export_csv)
        header_layout.addWidget(export_csv_btn)

        export_json_btn = QPushButton("Export JSON")
        export_json_btn.clicked.connect(self._export_json)
        header_layout.addWidget(export_json_btn)

        # Refresh button
        refresh_btn = QPushButton("Refresh Devices")
        refresh_btn.clicked.connect(self._refresh_devices)
        header_layout.addWidget(refresh_btn)

        layout.addLayout(header_layout)

        # Device list
        self.device_scroll = QScrollArea()
        self.device_container = QWidget()
        self.device_layout = QVBoxLayout(self.device_container)

        self.device_scroll.setWidget(self.device_container)
        self.device_scroll.setWidgetResizable(True)
        self.device_scroll.setMinimumHeight(400)

        layout.addWidget(self.device_scroll)

        # Status bar
        self.status_label = QLabel("Ready")
        self.status_label.setStyleSheet("color: blue;")
        layout.addWidget(self.status_label)

        self.setLayout(layout)
        self.setMinimumSize(800, 600)

    def _connect_signals(self):
        """Connect UI signals"""
        self.update_timer.timeout.connect(self._update_device_data)

    def _start_monitoring(self):
        """Start device monitoring"""
        self.update_timer.start(1000)  # Update every second
        self._refresh_devices()

    def _on_sim_mode_changed(self, state):
        """Handle simulation mode toggle"""
        enabled = state == Qt.Checked
        self.hardware_manager.set_simulation_mode(enabled)
        self._refresh_devices()

        if enabled:
            status = "Simulation mode enabled"
        else:
            status = "Simulation mode disabled"
        self.status_label.setText(status)

    def _refresh_devices(self):
        """Refresh the device list"""
        # Clear existing devices
        for device_widget in self.devices.values():
            device_widget.setParent(None)
            device_widget.deleteLater()
        self.devices.clear()

        # Discover new devices
        try:
            discovered_devices = self.hardware_manager.discover_devices()

            for device_info in discovered_devices:
                # Try to connect to the device
                device = self.hardware_manager.connect_device(device_info)
                if device:
                    device_widget = DeviceWidget(device)
                    self.devices[device_info.id] = device_widget
                    self.device_layout.addWidget(device_widget)

            device_count = len(self.devices)
            self.status_label.setText(f"Connected to {device_count} device(s)")

        except (OSError, ValueError) as e:
            self.status_label.setText(f"Error discovering devices: {e}")
            self.status_label.setStyleSheet("color: red;")

    def _update_device_data(self):
        """Update data for all connected devices"""
        try:
            all_data = self.hardware_manager.read_all_devices()

            for device_id, data in all_data.items():
                if device_id in self.devices:
                    device_widget = self.devices[device_id]
                    device_widget.update_sensor_data(data)
                    device_widget.update_status(True)

                    # Log the data
                    self.data_logger.log_data(device_id, data)
                else:
                    # Device not in our list, mark as disconnected
                    pass

        except (OSError, ValueError) as e:
            print(f"Error updating device data: {e}")

    def _export_csv(self):
        """Export logged data to CSV"""
        try:
            available_devices = self.data_logger.get_available_devices()
            if not available_devices:
                self.status_label.setText("No data to export")
                return

            # Export session data
            filepath = self.data_logger.export_session_to_csv()
            self.status_label.setText(f"Data exported to {filepath}")

        except (OSError, ValueError, IOError) as e:
            self.status_label.setText(f"Export failed: {e}")
            self.status_label.setStyleSheet("color: red;")

    def _export_json(self):
        """Export logged data to JSON"""
        try:
            available_devices = self.data_logger.get_available_devices()
            if not available_devices:
                self.status_label.setText("No data to export")
                return

            # Export first available device
            device_id = available_devices[0]
            filepath = self.data_logger.export_to_json(device_id)
            self.status_label.setText(f"Data exported to {filepath}")

        except (OSError, ValueError, IOError) as e:
            self.status_label.setText(f"Export failed: {e}")
            self.status_label.setStyleSheet("color: red;")

    def closeEvent(self, event):  # pylint: disable=invalid-name
        """Handle widget close event"""
        self.update_timer.stop()

        # Disconnect all devices
        for device_id in list(self.devices.keys()):
            self.hardware_manager.disconnect_device(device_id)

        event.accept()


# Factory function for creating the hardware panel
def create_hardware_control_panel(parent=None) -> HardwareControlPanel:
    """Create and return a hardware control panel widget"""
    return HardwareControlPanel(parent)

"""Focused checks for hardware state in interpreter variable snapshots."""

from time_warp.core.interpreter import Interpreter
from time_warp.features.hardware_simulator import HardwareType


def test_get_variables_includes_hardware_snapshot():
    interpreter = Interpreter()
    interpreter.hardware.add_device("led1", HardwareType.LED, 5)
    interpreter.hardware.set_device_value("led1", 77)
    interpreter.hardware.activate_device("led1", 77)
    interpreter.hardware.set_environment(temperature=29.0, humidity=55.0)

    variables = interpreter.get_variables()

    assert "__hardware__" in variables
    hardware = variables["__hardware__"]
    assert hardware["environment"]["temperature"] == 29.0
    assert hardware["environment"]["humidity"] == 55.0
    assert hardware["devices"]["led1"] == {
        "type": "led",
        "active": True,
        "value": 77,
        "reading_count": 0,
    }
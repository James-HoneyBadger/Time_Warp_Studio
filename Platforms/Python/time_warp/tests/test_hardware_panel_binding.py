"""Focused checks for shared hardware simulator binding."""

from time_warp.core.interpreter import Language
from time_warp.features.hardware_simulator import HardwareSimulator, HardwareType
from time_warp.graphics.turtle_state import TurtleState
from time_warp.ui.feature_panels import HardwareSimulatorPanel
from time_warp.ui.output import ImmediateModePanel, InterpreterThread, OutputPanel


def test_interpreter_thread_uses_shared_hardware_simulator():
    simulator = HardwareSimulator()
    turtle = TurtleState()
    thread = InterpreterThread(
        "10 HARDWARE DEVICE ADD temp1 TEMPERATURE_SENSOR PIN 0\n20 HARDWARE SENSOR READ temp1",
        turtle,
        Language.BASIC,
        hardware_simulator=simulator,
    )

    thread.run()

    assert thread.interp is not None
    assert thread.interp.hardware is simulator
    assert "temp1" in simulator.devices


def test_immediate_mode_clear_state_preserves_shared_hardware_simulator(qapp):
    panel = ImmediateModePanel()
    shared = HardwareSimulator()
    panel.set_hardware_simulator(shared)
    original = panel.hardware_simulator

    panel.clear_state()

    assert panel.interpreter.hardware is original


def test_hardware_panel_reflects_shared_simulator_state(qapp):
    output = OutputPanel()
    immediate = ImmediateModePanel()
    immediate.set_hardware_simulator(output.hardware_simulator)
    panel = HardwareSimulatorPanel()
    panel.bind_simulator(output.hardware_simulator)

    output.hardware_simulator.add_device("temp1", HardwareType.TEMPERATURE_SENSOR, 0)
    output.hardware_simulator.read_sensor("temp1")
    qapp.processEvents()

    assert immediate.interpreter.hardware is output.hardware_simulator
    assert panel.devices_table.rowCount() == 1
    assert panel.readings_table.rowCount() == 1
    assert panel.devices_table.item(0, 0).text() == "temp1"


def test_hardware_panel_applies_environment_to_shared_simulator(qapp):
    output = OutputPanel()
    panel = HardwareSimulatorPanel()
    panel.bind_simulator(output.hardware_simulator)

    panel.temperature_spin.setValue(31)
    panel.humidity_spin.setValue(72)
    panel.light_spin.setValue(900)
    panel.apply_environment()
    qapp.processEvents()

    environment = output.hardware_simulator.get_environment()
    assert environment == {
        "temperature": 31.0,
        "humidity": 72.0,
        "light_level": 900.0,
    }
    assert "31 °C" in panel.summary_label.text()


def test_hardware_panel_controls_selected_device_state(qapp):
    output = OutputPanel()
    panel = HardwareSimulatorPanel()
    panel.bind_simulator(output.hardware_simulator)

    output.hardware_simulator.add_device("led1", HardwareType.LED, 5)
    panel.refresh_state()
    panel.devices_table.selectRow(0)
    qapp.processEvents()

    panel.device_value_spin.setValue(77)
    panel.apply_selected_device_value()
    status = output.hardware_simulator.get_device_status("led1")
    assert status is not None
    assert status["current_value"] == 77

    panel.activate_selected_device()
    status = output.hardware_simulator.get_device_status("led1")
    assert status is not None
    assert status["is_active"] is True

    panel.deactivate_selected_device()
    status = output.hardware_simulator.get_device_status("led1")
    assert status is not None
    assert status["is_active"] is False
    assert status["current_value"] == 0.0